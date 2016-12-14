{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestOutline where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (cancel, poll)
import           Control.Concurrent.MVar  (readMVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception        (throwIO)
import           Control.Monad            (zipWithM)
import           Control.Monad.Managed    (MonadManaged)
import           Control.Monad.Reader     (ReaderT (runReaderT), MonadReader)
import           Data.Monoid              (Last (Last))
import           Data.Monoid.Same         (Same (NotSame, Same), allSame)
import qualified IpTables                 as IPT
import qualified PacketFilter             as PF
import           System.Info
import           Turtle

import Cluster
import ClusterAsync

newtype Repeat = Repeat { unRepeat :: Int }
newtype NumNodes = NumNodes { unNumNodes :: Int }

data FailureReason
  = WrongOrder (Last Block) (Last Block)
  | NoBlockFound
  | DidPanic
  deriving Show

data Validity
  = Verified
  | Falsified FailureReason
  deriving Show

second :: Int
second = 10 ^ (6 :: Int)

failedTestCode :: ExitCode
failedTestCode = ExitFailure 1

verifySameLastBlock :: [Either NodeTerminated (Last Block)] -> Validity
verifySameLastBlock results = case allSame results of
  NotSame a b -> Falsified $ case (a, b) of
    (Left NodeTerminated, _) -> DidPanic
    (_, Left NodeTerminated) -> DidPanic
    (Right b1, Right b2)     -> WrongOrder b1 b2
  Same (Left NodeTerminated)  -> Falsified DidPanic
  Same (Right (Last Nothing)) -> Falsified NoBlockFound
  _                           -> Verified

-- | Run this test up to @Repeat@ times or until it fails
repeatTester
  :: Repeat
  -> NumNodes
  -> ([Geth] -> ReaderT ClusterEnv Shell ())
  -> IO ()
repeatTester (Repeat 0) _ _ = return ()
repeatTester (Repeat n) numNodes cb = do
  resultVar <- liftIO newEmptyMVar

  sh $ flip runReaderT defaultClusterEnv $ do
    let geths = [1..GethId (unNumNodes numNodes)]
    _ <- when (os == "darwin") PF.acquirePf

    nodes <- setupNodes geths
    (readyAsyncs, terminatedAsyncs, lastBlockMs) <-
      unzip3 <$> traverse runNode nodes

    -- wait for geth to launch, then unlock and start raft
    awaitAll readyAsyncs

    startRaftAcross nodes

    cb nodes

    liftIO $ do
      -- pause an extra second before checking last block
      td 1
      
      -- verify that all have consistent logs
      lastBlocks <- traverse readMVar lastBlockMs
      meEarlyTerms <- traverse poll terminatedAsyncs

      results <- zipWithM (curry $ \case
                            (Just (Left ex), _)     -> throwIO ex
                            (Just (Right panic), _) -> pure $ Left panic
                            (Nothing, lastBlock)    -> pure $ Right lastBlock)
                          meEarlyTerms
                          lastBlocks

      putMVar resultVar (verifySameLastBlock results)

  result <- takeMVar resultVar
  print result
  case result of
    Verified -> repeatTester (Repeat (n - 1)) numNodes cb
    _ -> exit failedTestCode

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
