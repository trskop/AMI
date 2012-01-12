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
  handleEvent "FullyBooted" onBooted
  sendAction "Queues" [] queuesHandler
  wait
  close

onBooted ps = liftIO $ do
  putStrLn "Asterisk is fully booted."
  print ps

queuesHandler p =
  liftIO $ print p
