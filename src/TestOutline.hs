{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module TestOutline where

import Control.Concurrent         (threadDelay)
import Control.Concurrent.Async   (cancel)
import Control.Concurrent.MVar    (readMVar)
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
repeatTester (Repeat 0) _ _ = return ()
repeatTester (Repeat n) numNodes cb = do
  sh $ flip runReaderT defaultClusterEnv $ do
    let geths = [1..GethId (unNumNodes numNodes)]
    _ <- when (os == "darwin") PF.acquirePf

    nodes <- setupNodes geths
    (readyAsyncs, terminatedAsyncs, lastBlocks) <-
      unzip3 <$> traverse runNode nodes

    -- wait for geth to launch, then unlock and start raft
    awaitAll readyAsyncs

    startRaftAcross nodes

    cb nodes

    void $ liftIO $ do
      -- HACK: wait three seconds for geths to catch up
      threadDelay (3 * second)

      -- verify that all have consistent logs
      lastBlocks' <- traverse readMVar lastBlocks
      print $ verifySameLastBlock lastBlocks'

      -- cancel all the workers
      mapM_ cancel terminatedAsyncs

  repeatTester (Repeat (n - 1)) numNodes cb

tester :: NumNodes -> ([Geth] -> ReaderT ClusterEnv Shell ()) -> IO ()
tester = repeatTester (Repeat 1)

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
