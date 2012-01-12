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
  sendAction "Queues" [] queuesHandler
  wait
  close

queuesHandler p =
  liftIO $ print p
