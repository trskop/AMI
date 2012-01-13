{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans

import Network.AMI

info = ConnectInfo {
         ciHost = "localhost",
         ciPort = 5038,
         ciUsername = "monitor",
         ciSecret = "M%nit%r" }

main = withAMI_MD5 info $ do
  liftIO $ putStrLn "Open ok"
  handleEvent "FullyBooted" onBooted
  liftIO $ putStrLn "Set event handler ok"
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
