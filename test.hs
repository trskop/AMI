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
  sendAction "Queues" [] queuesHandler
  wait
  wait
  liftIO $ putStrLn "Queues ok"
  close

onBooted ps = liftIO $ do
  putStrLn "Asterisk is fully booted."
  print ps

queuesHandler p = liftIO $ do
  putStrLn "Answer for Queues:"
  print p
