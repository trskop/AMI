{-# LANGUAGE OverloadedStrings #-}
module Network.AMI
  (Packet (..),
   ConnectInfo (..),
   open, openMD5,
   close,
   withAMI, withAMI_MD5,
   runAMI, runAMI',
   sendAction,
   handleEvent,
   wait
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Instances
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import Network
import Network.Socket
import System.IO

type Parameters = [(B.ByteString, B.ByteString)]

type ActionType = B.ByteString

type EventType = B.ByteString

type ActionID = B.ByteString

type ResponseType = B.ByteString

type EventHandler = Parameters -> AMI ()

type ResponseHandler = Packet -> AMI ()

-- | Any AMI packet
data Packet =
    Action ActionID ActionType Parameters
  | Response ActionID ResponseType Parameters [B.ByteString]
  | Event EventType Parameters
  deriving (Eq, Show)

data AMIState = AMIState {
  amiHandle :: Maybe Handle,
  amiActionID :: Integer,
  amiResponseHandlers :: M.Map ActionID ResponseHandler,
  amiEventHandlers :: M.Map EventType EventHandler }

-- | Info needed to connect and authenticate in Asterisk
data ConnectInfo = ConnectInfo {
  ciHost :: String,
  ciPort :: Int,
  ciUsername :: B.ByteString,
  ciSecret :: B.ByteString }
  deriving (Eq, Show)

-- | The AMI monad
type AMI a = StateT AMIState IO a

-- | Return next ActionID
inc :: AMI ActionID
inc = do
  st <- get
  let n = 1 + amiActionID st
  put $ st {amiActionID = n}
  return (B.pack $ show n)

-- | Get connection handle
getHandle :: AMI Handle
getHandle = do
  mbh <- gets amiHandle
  case mbh of
    Nothing -> fail "Connection is not opened"
    Just h -> return h

-- | Add an event handler
handleEvent :: EventType -> EventHandler -> AMI ()
handleEvent t handler = modify add
  where
    add st = st {amiEventHandlers = M.insert t handler (amiEventHandlers st)}

-- | Send an Action packet and install a handler for the anser
sendAction :: ActionType -> Parameters -> ResponseHandler -> AMI ()
sendAction t ps handler = do
    i <- inc
    modify (\st -> st {amiResponseHandlers = M.insert i (change i handler) (amiResponseHandlers st)})
    h <- getHandle
    liftIO $ sendPacket h (Action i t ps)
  where
    change i hdlr p = do
      hdlr p
      modify $ \st -> st {amiResponseHandlers = M.delete i (amiResponseHandlers st)}

-- | Wait for (one) response or event
wait :: AMI ()
wait = do
  h <- getHandle
  str <- liftIO $ readUntilEmptyLine h
  case parse str of
    Left err -> fail err
    Right (Action {}) -> fail "Unexpected Action from server"
    Right p@(Response i t _ _) -> do
        st <- get
        case M.lookup i (amiResponseHandlers st) of
          Nothing -> liftIO $ putStrLn $ "No handler for " ++ show i
          Just handler -> do
                          put $ st {amiResponseHandlers = M.delete i (amiResponseHandlers st)}
                          liftIO $ putStrLn $ "Calling handler for " ++ show i
                          handler p
    Right (Event t ps) -> do
        m <- gets amiEventHandlers
        case M.lookup t m of
          Nothing -> return ()
          Just handler -> handler ps

-- | Open a connection to Asterisk and authenticate
open :: ConnectInfo -> AMI ()
open info = do
    h <- liftIO $ connectTo (ciHost info) (PortNumber $ fromIntegral $ ciPort info)
    modify $ \st -> st {amiHandle = Just h}
    s <- liftIO $ B.hGetLine h
    sendAction "Login" [("Username", ciUsername info), ("Secret", ciSecret info)] handleAuth
    wait
  where
    handleAuth :: Packet -> AMI ()
    handleAuth (Response _ "Success" _ _) = return ()
    handleAuth _ = fail "Authentication failed"

-- | Open a connection to Asterisk and authenticate using MD5 challenge
openMD5 :: ConnectInfo -> AMI ()
openMD5 info = do
    h <- liftIO $ connectTo (ciHost info) (PortNumber $ fromIntegral $ ciPort info)
    modify $ \st -> st {amiHandle = Just h}
    s <- liftIO $ B.hGetLine h
    sendAction "Challenge" [("AuthType", "md5")] challenger
    wait
  where
    challenger :: Packet -> AMI ()
    challenger (Response _ "Success" [("Challenge", ch)] _) = do
      let key = B.pack $ show $ md5 $ L.fromChunks [ch `B.append` ciSecret info]
      sendAction "Login" [("AuthType", "md5"),
                          ("Username", ciUsername info),
                          ("Key", key)] auth
    challenger _ = fail "Cannot get challenge for MD5 authentication"

    auth :: Packet -> AMI ()
    auth (Response _ "Success" _ _) = return ()
    auth x = fail $ "MD5 authentication failed: " ++ show x

-- | Close Asterisk connection
close :: AMI ()
close = do
  sendAction "Logoff" [] (const $ return ())
  wait
  h <- getHandle
  liftIO $ hClose h
  modify $ \st -> st {amiHandle = Nothing}

-- | Connect, execute acions, disconnect
withAMI :: ConnectInfo -> AMI a -> IO a
withAMI info ami = runAMI $ do
    open info
    r <- ami
    close
    return r

-- | Connect (using MD5 challenge), execute acions, disconnect
withAMI_MD5 :: ConnectInfo -> AMI a -> IO a
withAMI_MD5 info ami = runAMI $ do
    openMD5 info
    r <- ami
    close
    return r

-- | Send one AMI packet
sendPacket :: Handle -> Packet -> IO ()
sendPacket h p = do
  let s = format p `B.append` "\r\n"
  B.hPutStr h s
  B.hPutStr h "\r\n"
  hFlush h

-- | Run AMI actions
runAMI :: AMI a -> IO a
runAMI ami = evalStateT ami (AMIState Nothing 0 M.empty M.empty)

-- | Run AMI actions, starting with given ActionID
runAMI' :: Integer -> AMI a -> IO a
runAMI' z ami = evalStateT ami (AMIState Nothing z M.empty M.empty)

readUntilEmptyLine :: Handle -> IO B.ByteString
readUntilEmptyLine h = do
  str <- B.hGetLine h
  if (str == "\n") || (str == "\r") || (str == "\r\n")
    then return str
    else do
         next <- readUntilEmptyLine h
         return $ str `B.append` next

linesB y = h : if B.null t then [] else linesB (B.drop 2 t)
   where (h,t) = B.breakSubstring "\r\n" y

-- | Parse packet
parse :: B.ByteString -> Either String Packet
parse str = uncurry toPacket =<< (toPairs [] $ B.split '\r' str)
  where
    toPairs :: Parameters -> [B.ByteString] -> Either String (Parameters, [B.ByteString])
    toPairs [] [] = Left "Empty packet"
    toPairs acc [] = Right (acc, [])
    toPairs acc (s:ss) =
      case B.split ':' s of
        []     -> return (acc, [])
        [n,v]  -> let new = (n, B.dropWhile (== ' ') v)
                  in  toPairs (acc ++ [new]) ss
        x      -> Right (acc, (s:ss))

    toPacket :: Parameters -> [B.ByteString] -> Either String Packet
    toPacket [] text = Right $ Response "" "text" [] text
    toPacket ((k,v):pairs) text =
      case k of
        "Action"   -> toAction   v pairs
        "Response" -> toResponse v pairs text
        "Event"    -> toEvent    v pairs
        _          -> Left $ "Invalid first parameter: " ++ show v

    getField :: B.ByteString -> Parameters -> Either String (B.ByteString, Parameters)
    getField x ps = go x [] ps

    go x acc [] = Left "No field in packet"
    go x acc ((k,v):rest)
      | x == k    = Right (v, acc ++ rest)
      | otherwise = go x ((k,v):acc) rest

    toAction name pairs = do
      (i, ps) <- getField "ActionID" pairs
      return $ Action i name ps

    toResponse name pairs text = do
      (i, ps) <- getField "ActionID" pairs
      return $ Response i name ps text

    toEvent name pairs = Right $ Event name pairs

format :: Packet -> B.ByteString
format (Action i name ps)   = formatParams $ [("Action", name), ("ActionID", i)] ++ ps
format (Response i name ps text) =
    formatParams ([("Response", name), ("ActionID", i)] ++ ps) `B.append` "\r\n" `B.append` B.intercalate "\r\n" text
format (Event name ps)      = formatParams $ [("Event", name)] ++ ps

formatParams :: Parameters -> B.ByteString
formatParams pairs = B.intercalate "\r\n" $ map one pairs
  where
    one (k,v) = k `B.append` ": " `B.append` v

