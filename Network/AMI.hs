{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Network.AMI
  (Parameters,
   ActionType, EventType,
   ActionID, ResponseType,
   EventHandler, ResponseHandler,
   Packet (..),
   ConnectInfo (..),
   open, openMD5,
   close,
   withAMI, withAMI_MD5,
   runAMI,
   query,
   handleEvent
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Instances
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import System.IO.Unsafe (unsafePerformIO)
import Network
import Network.Socket
import System.IO

type Parameters = [(B.ByteString, B.ByteString)]

type ActionType = B.ByteString

type EventType = B.ByteString

type ActionID = Integer

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
  amiActionID :: ActionID,
  amiResponses :: M.Map ActionID (Maybe Packet),
  amiEventHandlers :: M.Map EventType EventHandler }

-- | Info needed to connect and authenticate in Asterisk
data ConnectInfo = ConnectInfo {
  ciHost :: String,
  ciPort :: Int,
  ciUsername :: B.ByteString,
  ciSecret :: B.ByteString }
  deriving (Eq, Show)

-- | The AMI monad
type AMI a = ReaderT (TVar AMIState) IO a

packID :: ActionID -> B.ByteString
packID i = B.pack (show i)

getAMI :: (AMIState -> a) -> AMI a
getAMI fn = do
  var <- ask
  st <- liftIO $ atomically $ readTVar var
  return (fn st)

putAMI :: AMIState -> AMI ()
putAMI st = do
  var <- ask
  liftIO $ atomically $ writeTVar var st

modifyAMI :: (AMIState -> AMIState) -> AMI ()
modifyAMI fn = do
  st <- getAMI id
  putAMI (fn st)

-- | Return next ActionID
inc :: AMI ActionID
inc = do
  st <- getAMI id
  let n = 1 + amiActionID st
  putAMI $ st {amiActionID = n}
  return n

-- | Get connection handle
getHandle :: AMI Handle
getHandle = do
  mbh <- getAMI amiHandle
  case mbh of
    Nothing -> fail "Connection is not opened"
    Just h -> return h

-- | Add an event handler
handleEvent :: EventType -> EventHandler -> AMI ()
handleEvent t handler = modifyAMI add
  where
    add st = st {amiEventHandlers = M.insert t handler (amiEventHandlers st)}

-- | Send an Action packet and install a handler for the anser
query :: ActionType -> Parameters -> AMI Packet
query t ps = do
  i <- inc
  var <- ask
  liftIO $ atomically $ do
      st <- readTVar var
      let resps = M.insert i Nothing (amiResponses st)
      writeTVar var $ st {amiResponses = resps}

  -- За счет unsafePerformIO сам забор ответа произойдет только в момент вычисления
  -- значения, лениво. Единственное но: надо следить за утечками, потому что если значение
  -- не будет вычислено, то ответ так и будет лежать в мапе. Имеет смысл прикрепить
  -- финализатор к значению, короче.
  return $ unsafePerformIO $ atomically $ do 
    st <- readTVar var
    let resps = amiResponses st
    case M.lookup i resps of
      Just (Just a) -> do
         writeTVar var $ st {amiResponses = M.delete i resps}
         return a
      Just (Nothing) -> retry
      Nothing -> fail $ "There was no response for Action " ++ show i

-- | Open a connection to Asterisk and authenticate
open :: ConnectInfo -> AMI ()
open info = do
    h <- liftIO $ connectTo (ciHost info) (PortNumber $ fromIntegral $ ciPort info)
    modifyAMI $ \st -> st {amiHandle = Just h}
    s <- liftIO $ B.hGetLine h
    auth <- query "Login" [("Username", ciUsername info), ("Secret", ciSecret info)]
    case auth of
      Response _ "Success" _ _ -> return ()
      _ -> fail "Authentication failed"

-- | Open a connection to Asterisk and authenticate using MD5 challenge
openMD5 :: ConnectInfo -> AMI ()
openMD5 info = do
    h <- liftIO $ connectTo (ciHost info) (PortNumber $ fromIntegral $ ciPort info)
    modifyAMI $ \st -> st {amiHandle = Just h}
    s <- liftIO $ B.hGetLine h
    chp <- query "Challenge" [("AuthType", "md5")]
    case chp of
      Response _ "Success" [("Challenge", ch)] _ -> do
        let key = B.pack $ show $ md5 $ L.fromChunks [ch `B.append` ciSecret info]
        auth <- query "Login" [("AuthType", "md5"),
                            ("Username", ciUsername info),
                            ("Key", key)]
        case auth of
          Response _ "Success" _ _ -> return ()
          x -> fail $ "MD5 authentication failed: " ++ show x
      _ -> fail "Cannot get challenge for MD5 authentication"

-- | Close Asterisk connection
close :: AMI ()
close = do
  !x <- query "Logoff" [] 
  h <- getHandle
  liftIO $ hClose h
  modifyAMI $ \st -> st {amiHandle = Nothing}

-- | Connect, execute acions, disconnect
withAMI :: ConnectInfo -> AMI a -> IO a
withAMI info ami = runAMI $ do
    open info
    t <- forkAnswersReader
    r <- ami
    liftIO $ killThread t
    close
    return r

-- | Connect (using MD5 challenge), execute acions, disconnect
withAMI_MD5 :: ConnectInfo -> AMI a -> IO a
withAMI_MD5 info ami = runAMI $ do
    openMD5 info
    t <- forkAnswersReader
    r <- ami
    liftIO $ killThread t
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
runAMI ami = do
  var <- atomically $ newTVar (AMIState Nothing 0 M.empty M.empty)
  runReaderT ami var

readUntilEmptyLine :: Handle -> IO B.ByteString
readUntilEmptyLine h = do
  str <- B.hGetLine h
  if (str == "\n") || (str == "\r") || (str == "\r\n")
    then return str
    else do
         next <- readUntilEmptyLine h
         return $ str `B.append` next

forkAnswersReader :: AMI ThreadId
forkAnswersReader = do
    h <- getHandle
    var <- ask
    liftIO $ forkIO (forever $ reader h var)
  where
    reader :: Handle -> TVar AMIState -> IO ()
    reader h var = do
      str <- readUntilEmptyLine h
      case parse str of
        Left err -> do
                    putStrLn $ "Error parsing answer: " ++ err
                    return ()
        Right p@(Response i _ _ _) -> atomically $ do
            st <- readTVar var
            let resps = M.insert i (Just p) (amiResponses st)
            writeTVar var $ st {amiResponses = resps}

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
    toPacket [] text = Right $ Response 0 "text" [] text
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
      return $ Action (read $ B.unpack i) name ps

    toResponse name pairs text = do
      (i, ps) <- getField "ActionID" pairs
      return $ Response (read $ B.unpack i) name ps text

    toEvent name pairs = Right $ Event name pairs

format :: Packet -> B.ByteString
format (Action i name ps)   = formatParams $ [("Action", name), ("ActionID", packID i)] ++ ps
format (Response i name ps text) =
    formatParams ([("Response", name), ("ActionID", packID i)] ++ ps) `B.append` "\r\n" `B.append` B.intercalate "\r\n" text
format (Event name ps)      = formatParams $ [("Event", name)] ++ ps

formatParams :: Parameters -> B.ByteString
formatParams pairs = B.intercalate "\r\n" $ map one pairs
  where
    one (k,v) = k `B.append` ": " `B.append` v

