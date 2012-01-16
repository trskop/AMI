{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans
import Control.Concurrent

import Network.AMI

info = ConnectInfo {
         ciHost = "localhost",
         ciPort = 5038,
         ciUsername = "monitor",
         ciSecret = "PASSWORD" }

main = do
  forkIO test
  threadDelay 150
  test

test = withAMI_MD5 info $ do
  handleEvent "FullyBooted" onBooted
  mail <- query "MailboxCount" [("Mailbox","900")]
  liftIO $ print mail
  jabber <- query "JabberSend" [("Jabber", "asterisk"),
                           ("JID", "portnov@free-alt.ru"),
                           ("ScreenName", "asterisk"),
                           ("Message", "Jabber via AMI")]
  liftIO $ print jabber

onBooted ps = liftIO $ do
  putStrLn "Asterisk is fully booted."
  print ps
