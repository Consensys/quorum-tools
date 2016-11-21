{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Test1 where

import Control.Concurrent         (threadDelay)
import Control.Concurrent.Async   (cancel)
import Control.Concurrent.MVar    (readMVar)
import Control.Monad.Reader       (ReaderT (runReaderT))
import Data.Monoid.Same
import Turtle

import Cluster
import ClusterAsync

data FailureReason
  = WrongOrder LastBlock LastBlock
  | DidPanic
  deriving Show

data Validity
  = Verified
  | Falsified FailureReason
  deriving Show

verifySameLastBlock :: [LastBlock] -> Validity
verifySameLastBlock lastBlocks = case allSame lastBlocks of
  NotSame a b -> Falsified $ case (a, b) of
    (Panic, _) -> DidPanic
    (_, Panic) -> DidPanic
    (_, _) -> WrongOrder a b
  _ -> Verified

second :: Int
second = 10 ^ (6 :: Int)

main :: IO ()
main = sh $ flip runReaderT defaultClusterEnv $ do
  nodes <- setupNodes [1..3]
  (readyAsyncs, terminatedAsyncs, lastBlocks) <-
    unzip3 <$> traverse runNode nodes

  -- wait for geth to launch, then unlock and start raft
  awaitAll readyAsyncs
  void $ forConcurrently' nodes (\geth -> unlockAccount geth >> startRaft geth)

  -- HACK: inconsistency taking `tail nodes` here, and assuming `GethId 1`
  -- later on equivalent to the head
  spammer <- clusterAsync $ spamTransactions (tail nodes)

  -- run with all three nodes for a second, partition 1 for a second, run with
  -- all three for another second
  partitioner <- clusterAsync $ do
    void $ liftIO $ threadDelay (1 * second)
    partition 1000 (GethId 1)
    void $ liftIO $ threadDelay (1 * second)

  -- run spammer and partitioner concurrently, wait for the partitioner process
  -- to finish
  void $ waitAnyCluster [spammer, partitioner]

  void $ liftIO $ do
    -- HACK: wait three seconds for geths to catch up
    threadDelay (3 * second)

    -- verify that all have consistent logs
    lastBlocks' <- traverse readMVar lastBlocks
    print $ verifySameLastBlock lastBlocks'

    -- cancel all the workers
    mapM_ cancel terminatedAsyncs
