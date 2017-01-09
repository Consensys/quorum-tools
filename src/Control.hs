module Control where

import           Control.Concurrent.Async   (Async)
import           Control.Concurrent.MVar    (MVar, takeMVar, modifyMVar,
                                             tryPutMVar)
import           Control.Exception          (bracket)
import           Control.Monad.Managed      (MonadManaged)
import           Data.Foldable              (traverse_)
import           Turtle                     (MonadIO, void, fork, liftIO, wait)

-- | Non-blocking MVar put.
--
-- This is idempotent, intended to be used for transitions that happen exactly
-- once.
ensureMVarTransition :: MVar a -> a -> IO ()
ensureMVarTransition var a = void $ tryPutMVar var a

pureModifyMVar :: MVar a -> (a -> a) -> IO a
pureModifyMVar var f = modifyMVar var f' where
  f' a = let result = f a in pure (result, result)

-- | Fulfil the returned async as soon as the MVar is filled.
awaitMVar :: MonadManaged m => MVar a -> m (Async a)
awaitMVar = fork . takeMVar

-- | Await fulfillment of all asyncs before continuing.
--
-- A "join point".
awaitAll :: (MonadIO m, Traversable t) => t (Async a) -> m ()
awaitAll = liftIO . traverse_ wait

-- | Execute an action before exiting. Exception safe.
--
-- @
--     onExit (putStrLn "exited!") $ \_ -> { code }
-- @
onExit :: IO () -> (() -> IO r) -> IO r
onExit action = bracket (pure ()) (const action)
