-- Late-joining node
module Main where

import Control.Concurrent         (threadDelay)
import Control.Concurrent.Async   (cancel)
import Control.Concurrent.MVar    (readMVar)
import Control.Monad.Reader       (ReaderT (runReaderT))
import Turtle

import Cluster
import ClusterAsync

main :: IO ()
main = sh $ flip runReaderT defaultClusterEnv $ do
  nodes <- setupNodes [1..3]

  (readyAsyncs, terminatedAsyncs, lastBlocks) <-
    unzip3 <$> traverse runNode nodes

  -- wait for geth to launch, then unlock and start raft
  awaitAll readyAsyncs

  -- have the first one join three seconds after the others
  let late:early = nodes

  void $ forConcurrently' early (\geth -> unlockAccount geth >> startRaft geth)

  spammer <- clusterAsync $ spamTransactions early

  void $ clusterAsync $ do
    void $ liftIO $ threadDelay (3 * second)
    unlockAccount late
    startRaft late

  void $ liftIO $ do
    -- run it for ten seconds
    threadDelay (10 * second)
    cancel spammer

    -- HACK: wait three seconds for geths to catch up
    threadDelay (3 * second)

    -- verify that all have consistent logs
    lastBlocks' <- traverse readMVar lastBlocks
    print $ verifySameLastBlock lastBlocks'

    -- cancel all the workers
    mapM_ cancel terminatedAsyncs
