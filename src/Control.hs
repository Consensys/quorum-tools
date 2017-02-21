module Control where

import           Control.Concurrent.Async   (Async, waitAny)
import           Control.Concurrent.MVar    (MVar, takeMVar, modifyMVar,
                                             tryPutMVar)
import           Control.Exception          (bracket)
import           Control.Monad.Managed      (MonadManaged)
import           Data.Foldable              (traverse_, toList)
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

-- | Await fulfillment of any async before continuing.
awaitAny :: (MonadIO m, Traversable t) => t (Async a) -> m ()
awaitAny = liftIO . void . waitAny . toList

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
