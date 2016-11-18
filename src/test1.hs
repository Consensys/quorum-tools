module Test1 where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async   (forConcurrently, cancel)
import Control.Concurrent.MVar    (readMVar)
import Control.Monad.Reader       (ReaderT (runReaderT))
import Data.Monoid.Same
import Turtle

import Cluster

-- data FailureReason
--   = MissingTx
--   | WrongOrder
--   | Panic

data Validity
  = Verified
  -- we need to give a reason for the failure
  | Falsified
  deriving Show

verifySameLastBlock :: [LastBlock] -> Validity
verifySameLastBlock lastBlocks =
  let allEq = allSame lastBlocks
  in if allEq then Verified else Falsified

main :: IO ()
main = sh $ flip runReaderT defaultClusterEnv $ do
  nodes <- setupNodes [1..3]
  (readyAsyncs, _terminatedAsyncs, lastBlocks) <-
    unzip3 <$> traverse runNode nodes

  awaitAll readyAsyncs

  void $ liftIO $ do
    void $ forConcurrently nodes unlockAccount
    forConcurrently nodes startRaft

  -- HACK: inconsistency taking `tail nodes` here, and assuming `GethId 1`
  -- later on equivalent to the head
  --
  -- TODO: make a ClusterConcurrently primitive to allow symmetric forking
  spamAsync <- fork (spamTransactions (tail nodes))

  -- run with all three nodes for a second, partition 1 for a second, run with
  -- all three for another second
  liftIO $ threadDelay (1 * 1000000)
  partition 1000 (GethId 1)
  liftIO $ do
    threadDelay (1 * 1000000)

    cancel spamAsync

    -- HACK: wait three seconds for them to catch up
    threadDelay (3 * 1000000)

    -- verify that all have consistent logs
    lastBlocks' <- traverse readMVar lastBlocks
    print $ verifySameLastBlock lastBlocks'
