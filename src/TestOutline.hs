{-# LANGUAGE FlexibleContexts #-}
module TestOutline where

import Control.Concurrent         (threadDelay)
import Control.Concurrent.Async   (cancel)
import Control.Concurrent.MVar    (readMVar)
import Control.Monad.Reader       (ReaderT (runReaderT), MonadReader)
import Turtle

import Cluster
import ClusterAsync

-- TODO make this not callback-based
tester :: Int -> ([Geth] -> ReaderT ClusterEnv Shell ()) -> IO ()
tester n cb = sh $ flip runReaderT defaultClusterEnv $ do
  nodes <- setupNodes (map GethId [1..n])
  (readyAsyncs, terminatedAsyncs, lastBlocks) <-
    unzip3 <$> traverse runNode nodes

  -- wait for geth to launch, then unlock and start raft
  awaitAll readyAsyncs

  cb nodes

  void $ liftIO $ do
    -- HACK: wait three seconds for geths to catch up
    threadDelay (3 * second)

    -- verify that all have consistent logs
    lastBlocks' <- traverse readMVar lastBlocks
    print $ verifySameLastBlock lastBlocks'

    -- cancel all the workers
    mapM_ cancel terminatedAsyncs

startAndUnlock
  :: (Traversable t, MonadIO m, MonadReader ClusterEnv m)
  => t Geth
  -> m ()
startAndUnlock gs =
  void $ forConcurrently' gs (\geth -> unlockAccount geth >> startRaft geth)

-- TODO make this not callback-based
-- spammer :: MonadManaged m => 
withSpammer :: (MonadIO m, MonadReader ClusterEnv m) => [Geth] -> m () -> m ()
withSpammer geths action = do
  spammer <- clusterAsync $ spamTransactions geths
  action
  liftIO $ cancel spammer

td :: MonadIO m => Int -> m ()
td = liftIO . threadDelay . (* second)
