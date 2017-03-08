{-# LANGUAGE RankNTypes          #-}
module Cluster.Control where

import           Control.Concurrent.Async   (Async)
import           Control.Concurrent.MVar    (MVar, takeMVar, modifyMVar, putMVar,
                                             tryPutMVar, swapMVar, newMVar, newEmptyMVar)
import           Control.Exception          (bracket)
import           Control.Monad.Managed      (MonadManaged)
import           Data.Foldable              (traverse_)
import           Turtle                     (MonadIO, void, fork, liftIO, wait,
                                             Fold(..))

-- | Non-blocking MVar put.
--
-- This is idempotent, intended to be used for transitions that happen exactly
-- once.
ensureMVarTransition :: MVar a -> a -> IO ()
ensureMVarTransition var a = void $ tryPutMVar var a

pureModifyMVar :: MVar a -> (a -> a) -> IO a
pureModifyMVar var f = modifyMVar var f' where
  f' a = let result = f a in pure (result, result)

pureModifyMVar_ :: MVar a -> (a -> a) -> IO ()
pureModifyMVar_ var = void . pureModifyMVar var

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

-- | @(find' predicate)@ returns the first @Just@ predicate result or 'Nothing'
-- if no element satisfies the predicate
find' :: (a -> Maybe b) -> Fold a (Maybe b)
find' predicate = Fold step Nothing id where
  step accum a =
    let match = predicate a
    in case (accum, match) of
         (Just _b, _)       -> accum
         (Nothing, Just _b) -> match
         (Nothing, Nothing) -> Nothing

eventVar :: forall m a. MonadManaged m => a -> m (Async a, IO ())
eventVar a = do
  mvar <- liftIO newEmptyMVar
  triggered <- awaitMVar mvar
  let transition = ensureMVarTransition mvar a
  pure (triggered, transition)

behaviorVar :: (MonadManaged m, Monoid a) => m (MVar a, a -> IO ())
behaviorVar = do
  mvar <- liftIO $ newMVar mempty
  pure (mvar, void . swapMVar mvar)

behaviorVar' :: (MonadManaged m, Monoid a) => m (MVar a, (a -> a) -> IO ())
behaviorVar' = do
  mvar <- liftIO $ newMVar mempty
  pure (mvar, pureModifyMVar_ mvar)

behaviorToEvent :: MonadManaged io => MVar a -> (a -> Maybe b) -> io (Async b)
behaviorToEvent bvar predicate = do
  evar <- liftIO newEmptyMVar
  let helper = do
        val <- takeMVar bvar
        case predicate val of
          Just b -> putMVar evar b
          Nothing -> helper
  fork helper
  awaitMVar evar
