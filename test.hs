{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans

import AMI

info = ConnectInfo {
         ciHost = "localhost",
         ciPort = 5038,
         ciUsername = "monitor",
         ciSecret = "M%nit%r" }

main = runAMI $ do
  open info
  liftIO $ putStrLn "Open ok"
  handleEvent "FullyBooted" onBooted
  liftIO $ putStrLn "Set event handler ok"
  sendAction "MailboxCount" [("Mailbox","900")] cmdHandler
  wait
  wait
  liftIO $ putStrLn "Command ok"
  close

onBooted ps = liftIO $ do
  putStrLn "Asterisk is fully booted."
  print ps

cmdHandler p = liftIO $ do
  putStrLn "Answer for command:"
  print p
