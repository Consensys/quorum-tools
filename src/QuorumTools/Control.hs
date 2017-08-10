{-# LANGUAGE RankNTypes #-}

module QuorumTools.Control where

import           Control.Concurrent.Async     (Async)
import           Control.Concurrent.MVar      (newEmptyMVar, takeMVar,
                                               tryPutMVar)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (TChan, dupTChan, newTChan,
                                               readTChan, writeTChan)
import           Control.Concurrent.STM.TMVar (TMVar, newTMVar, putTMVar,
                                               readTMVar, takeTMVar)
import           Control.Exception            (bracket)
import           Control.Monad.Loops          (untilJust)
import           Control.Monad.Managed        (MonadManaged)
import           Data.Foldable                (traverse_)
import           Data.Maybe                   (fromMaybe)
import           Turtle                       (Fold (..), MonadIO, fork, liftIO,
                                               void, wait)

data Behavior a = Behavior (TChan a) (TMVar (Maybe a))

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
  occurred <- fork $ takeMVar mvar
  let fire = void $ tryPutMVar mvar e
  pure (occurred, fire)

-- | Creates a stream of values for a many publishers to update, and many
-- consumers to subscribe (to value changes) or observe the current value.
behavior :: (MonadIO m) => m (Behavior a)
behavior = liftIO $ atomically $ Behavior <$> newTChan <*> newTMVar Nothing

transition' :: MonadIO m => Behavior a -> (Maybe a -> a) -> m ()
transition' (Behavior tc tm) f = liftIO $ atomically $ do
  prev <- takeTMVar tm
  let next = f prev
  putTMVar tm $ Just next
  writeTChan tc next

transition :: (MonadIO m, Monoid a) => Behavior a -> (a -> a) -> m ()
transition b f = transition' b (f . fromMaybe mempty)

subscribe :: MonadIO m => Behavior a -> m (TChan a)
subscribe (Behavior chan _) = liftIO $ atomically $ dupTChan chan

observe' :: MonadIO m => Behavior a -> m (Maybe a)
observe' (Behavior _ mvar) = liftIO $ atomically $ readTMVar mvar

observe :: (MonadIO m, Monoid a) => Behavior a -> m a
observe = fmap (fromMaybe mempty) . observe'

watch :: MonadManaged m => Behavior a -> (a -> Maybe b) -> m (Async b)
watch b decide = do
  chan <- subscribe b
  fork $ liftIO $ untilJust $ decide <$> atomically (readTChan chan)
