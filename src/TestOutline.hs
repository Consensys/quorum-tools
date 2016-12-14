{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module TestOutline where

import Control.Concurrent         (threadDelay)
import Control.Concurrent.Async   (Async, cancel)
import Control.Concurrent.MVar    (MVar, readMVar)
import Control.Monad.Managed      (MonadManaged)
import Control.Monad.Reader       (ReaderT (runReaderT), MonadReader)
import System.Info
import Turtle

import Cluster
import qualified PacketFilter as PF
import qualified IpTables as IPT
import ClusterAsync

newtype Repeat = Repeat { unRepeat :: Int }
newtype NumNodes = NumNodes { unNumNodes :: Int }

-- | Run this test up to @Repeat@ times or until it fails
repeatTester
  :: Repeat
  -> NumNodes
  -> ([Geth] -> ReaderT ClusterEnv Shell ())
  -> IO ()
repeatTester repeatNum numNodes cb = sh $ do
  (lastBlocks, terminatedAsyncs, nodes) <-
    runReaderT (setupNodesForTester numNodes) defaultClusterEnv
  repeatTesterHelper repeatNum nodes terminatedAsyncs lastBlocks cb

-- | Run this test once
tester :: NumNodes -> ([Geth] -> ReaderT ClusterEnv Shell ()) -> IO ()
tester = repeatTester (Repeat 1)

setupNodesForTester
  :: (MonadManaged m, HasEnv m)
  => NumNodes
  -> m ([MVar LastBlock], [Async NodeTerminated], [Geth])
setupNodesForTester (NumNodes n) = do
  let geths = [1..GethId n]
  _ <- when (os == "darwin") PF.acquirePf

  nodes <- setupNodes geths
  (readyAsyncs, terminatedAsyncs, lastBlocks) <-
    unzip3 <$> traverse runNode nodes

  -- wait for geth to launch, then unlock and start raft
  awaitAll readyAsyncs
  startRaftAcross nodes
  return (lastBlocks, terminatedAsyncs, nodes)

repeatTesterHelper
  :: Repeat
  -> [Geth]
  -> [Async NodeTerminated]
  -> [MVar LastBlock]
  -> ([Geth] -> ReaderT ClusterEnv Shell ())
  -> Shell ()
repeatTesterHelper (Repeat 0) _ _ _ _ = return ()
repeatTesterHelper (Repeat n) geths terminatedAsyncs lastBlocks cb = do
  runReaderT (cb geths) defaultClusterEnv

  -- HACK: wait three seconds for geths to catch up
  liftIO $ threadDelay (3 * second)

  -- verify that all have consistent logs
  lastBlocks' <- traverse (liftIO . readMVar) lastBlocks
  let verified = verifySameLastBlock lastBlocks'
  liftIO $ print verified

  case verified of
    Verified ->
      repeatTesterHelper (Repeat (n - 1)) geths terminatedAsyncs lastBlocks cb
    _ -> mapM_ (liftIO . cancel) terminatedAsyncs -- cancel all the workers

partition :: (MonadManaged m, HasEnv m) => Millis -> GethId -> m ()
partition millis node =
  if os == "darwin"
  then PF.partition millis node >> PF.flushPf
  else IPT.partition millis node

startRaftAcross
  :: (Traversable t, MonadIO m, MonadReader ClusterEnv m)
  => t Geth
  -> m ()
startRaftAcross gs = void $ forConcurrently' gs startRaft

-- TODO make this not callback-based
-- spammer :: MonadManaged m =>
withSpammer :: (MonadIO m, MonadReader ClusterEnv m) => [Geth] -> m () -> m ()
withSpammer geths action = do
  spammer <- clusterAsync $ spamTransactions geths
  action
  liftIO $ cancel spammer

td :: MonadIO m => Int -> m ()
td = liftIO . threadDelay . (* second)
