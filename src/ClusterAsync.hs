{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module ClusterAsync where

import Control.Concurrent.Async   (Async, async, forConcurrently, waitAny)
import Control.Monad.Reader       (ReaderT (ReaderT, runReaderT), ask)
import Turtle

import Cluster

-- | An asynchronous action with access to the cluster environment.
newtype ClusterAsync a = ClusterAsync
  { runClusterAsync :: ReaderT ClusterEnv Async a }
  deriving Functor

-- | Spawn an asynchronous cluster action.
--
-- Note: We force a return type of @()@ so we can use @sh@, discarding any
-- unused values from the shell. It would be possible to get a return value, if
-- we need it, but you'd have to supply a fold.
clusterAsync
  :: (MonadIO m, HasEnv m)
  => ReaderT ClusterEnv Shell a
  -> m (ClusterAsync ())
clusterAsync m = do
  clusterEnv <- ask
  a <- liftIO $ async $ sh (runReaderT m clusterEnv)
  return $ ClusterAsync (ReaderT (const a))

-- | Wait for any cluster action to finish.
waitAnyCluster :: (MonadIO m, HasEnv m) => [ClusterAsync a] -> m (Async a, a)
waitAnyCluster asyncs = do
  clusterEnv <- ask
  let asyncs_ = map (flip runReaderT clusterEnv . runClusterAsync) asyncs
  liftIO $ waitAny asyncs_

-- | Map an IO-performing function with cluster environment access.
forConcurrently'
  :: (Traversable t, MonadIO m, HasEnv m)
  => t a
  -> (a -> ReaderT ClusterEnv IO b)
  -> m (t b)
forConcurrently' struct f = do
  clusterEnv <- ask
  liftIO $ forConcurrently struct $ \a -> runReaderT (f a) clusterEnv
