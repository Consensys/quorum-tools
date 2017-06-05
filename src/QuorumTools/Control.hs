{-# LANGUAGE RankNTypes #-}
module QuorumTools.Control where

import           Control.Concurrent.Async (Async)
import           Control.Concurrent.Chan  (Chan, dupChan, newChan, readChan,
                                           writeChan)
import           Control.Concurrent.MVar  (MVar, modifyMVar, newEmptyMVar,
                                           newMVar, readMVar, takeMVar,
                                           tryPutMVar)
import           Control.Exception        (bracket)
import           Control.Monad.Loops      (untilJust)
import           Control.Monad.Managed    (MonadManaged)
import           Data.Foldable            (traverse_)
import           Turtle                   (Fold (..), MonadIO, fork, liftIO,
                                           void, wait)

import           QuorumTools.Types

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

event :: forall m e. MonadManaged m => e -> m (Async e, IO ())
event e = do
  mvar <- liftIO newEmptyMVar
  occurred <- awaitMVar mvar
  let fire = void $ tryPutMVar mvar e
  pure (occurred, fire)

-- | Creates a stream of values for a single publisher to update, and many
-- consumers to subscribe (to value changes) or observe the current value.
--
-- NOTE: if we used STM+TChan+TMVar, we could remove the single-producer
-- restriction by modifying the chan and mvar concurrently.
--
behavior :: (MonadIO m, Monoid a) => m (Behavior a)
behavior = Behavior <$> liftIO newChan <*> liftIO (newMVar mempty)

transition :: MonadIO m => Behavior a -> (a -> a) -> m ()
transition (Behavior chan mvar) update = liftIO $ do
  nextValue <- pureModifyMVar mvar update
  writeChan chan nextValue

subscribe :: MonadIO m => Behavior a -> m (Chan a)
subscribe (Behavior chan _) = liftIO $ dupChan chan

observe :: MonadIO m => Behavior a -> m a
observe (Behavior _ mvar) = liftIO $ readMVar mvar

--
-- TODO: this won't leak memory via the duplicated channel after our event
-- occurs, right?
--
watch :: MonadManaged m => Behavior a -> (a -> Maybe b) -> m (Async b)
watch b check = do
  chan <- subscribe b
  fork $ liftIO $ untilJust $ check <$> readChan chan
