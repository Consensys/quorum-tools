{-# LANGUAGE OverloadedStrings #-}

-- Run a cluster, stop the entire thing, restart
module Mains.RestartClusterTest where

import Control.Monad.Reader     (ReaderT (runReaderT))
import Turtle

import Cluster
import Control
import TestOutline

run :: ReaderT ClusterEnv Shell () -> IO ()
run m = sh $ flip runReaderT (mkLocalEnv 3) m

waitForElection :: (MonadIO m, Traversable t) => t NodeInstrumentation -> m ()
waitForElection instruments = do
  timestampedMessage "awaiting a successful raft election"
  awaitAll (assumedRole <$> instruments)
  timestampedMessage "initial election succeeded"


restartClusterTestMain :: IO ()
restartClusterTestMain = do
  let gethIds = [1..3]
      numNodes = 3

  run $ do
    nodes <- wipeAndSetupNodes "gdata" gethIds
    instruments <- traverse (runNode numNodes) nodes
    waitForElection instruments
    withSpammer nodes $ td 2

  putStrLn "enter to continue"
  _ <- getLine

  -- TODO: would be really nice to check we start with the same block we left
  -- off with
  run $ do
    nodes <- setupNodes gethIds
    instruments <- traverse (runNode numNodes) nodes
    waitForElection instruments

    withSpammer nodes $ td 2

    -- pause a second before checking last block
    td 1

    let verifier = verify
          (lastBlock <$> instruments)
          (outstandingTxes <$> instruments)
          (nodeTerminated <$> instruments)
    result1 <- liftIO verifier

    -- wait an extra five seconds to guarantee raft has a chance to converge
    case result1 of
      Falsified (WrongOrder _ _) -> td 5
      Falsified NoBlockFound -> td 5
      _ -> return ()

    result2 <- liftIO verifier
    liftIO $ case result2 of
      Falsified _ -> exit failedTestCode
      _           -> putStrLn "all successful!"
