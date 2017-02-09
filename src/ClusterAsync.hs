{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module ClusterAsync where

import Control.Concurrent.Async   (Async, async, forConcurrently)
import Control.Monad.Reader       (ReaderT (ReaderT, runReaderT), ask)
import Turtle                     (MonadIO, Shell, liftIO, sh)

import Cluster.Types

-- | An asynchronous action with access to the cluster environment.
newtype ClusterAsync a
  = ClusterAsync { runClusterAsync :: ReaderT ClusterEnv Async a }
  deriving Functor

-- | Spawn an asynchronous cluster action.
--
-- Note: We force a return type of @()@ so we can use @sh@, discarding any
-- unused values from the shell. It would be possible to get a return value, if
-- we need it, but you'd have to supply a fold.
clusterAsync
  :: (MonadIO m, HasEnv m)
  => ReaderT ClusterEnv Shell a
  -> m (Async ())
clusterAsync m = do
  clusterEnv <- ask
  liftIO $ async $ sh (runReaderT m clusterEnv)

-- | Map an IO-performing function with cluster environment access.
forConcurrently'
  :: (Traversable t, MonadIO m, HasEnv m)
  => t a
  -> (a -> ReaderT ClusterEnv IO b)
  -> m (t b)
forConcurrently' struct f = do
  clusterEnv <- ask
  liftIO $ forConcurrently struct $ \a -> runReaderT (f a) clusterEnv
