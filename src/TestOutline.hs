{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestOutline where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, cancel, poll)
import           Control.Concurrent.MVar  (readMVar, newEmptyMVar, putMVar)
import           Control.Lens
import           Control.Monad            (forM_)
import           Control.Monad.Except
import           Control.Monad.Managed    (MonadManaged)
import           Control.Monad.Reader     (ReaderT (runReaderT), MonadReader)
import           Data.Monoid              (Last (Last))
import           Data.Monoid.Same         (Same (NotSame, Same), allSame)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text, pack)
import qualified Data.Text.IO             as T
import           Data.Time                (getZonedTime, formatTime, defaultTimeLocale)
import qualified IpTables                 as IPT
import qualified PacketFilter             as PF
import           Prelude                  hiding (FilePath)
import           System.Info
import           Turtle

import Cluster
import Cluster.Types
import Cluster.Client
import Cluster.Control (awaitAll, observe)
import ClusterAsync
import Constellation

newtype TestNum = TestNum { unTestNum :: Int } deriving (Enum, Num)
newtype NumNodes = NumNodes { unNumNodes :: Int }

data FailureReason
  = WrongOrder (Last Block) (Last Block)
  | NoBlockFound
  | DidPanic
  | LostTxes (Set TxId)
  -- Expected @Int@, received @Either Text Int@
  | WrongValue Int (Either Text Int)
  deriving Show

data Validity
  = Verified
  | Falsified FailureReason
  deriving Show

instance Monoid Validity where
  mempty = Verified

  mappend Verified falsified@(Falsified _) = falsified
  mappend falsified@(Falsified _) _        = falsified
  mappend _ _                              = Verified

second :: Int
second = 10 ^ (6 :: Int)

failedTestCode :: ExitCode
failedTestCode = ExitFailure 1

data ShouldTerminate
  = DoTerminateSuccess
  | DoTerminateFailure
  | DontTerminate

instance Monoid ShouldTerminate where
  mempty = DontTerminate
  mappend DoTerminateSuccess _ = DoTerminateSuccess
  mappend DoTerminateFailure _ = DoTerminateFailure
  mappend DontTerminate      t = t

type TestPredicate = TestNum -> Validity -> ShouldTerminate

type TestM = ExceptT FailureReason (ReaderT ClusterEnv Shell)

-- | Run this test up to @TestNum@ times or until it fails
tester
  :: TestPredicate
  -> PrivacySupport
  -> NumNodes
  -> ([(Geth, NodeInstrumentation)] -> TestM ())
  -> IO ()
tester p privacySupport numNodes cb = foldr go mempty [0..] >>= \case
  DoTerminateSuccess -> exit ExitSuccess
  DoTerminateFailure -> exit failedTestCode
  DontTerminate      -> putStrLn "all successful!"

  where
    go :: TestNum -> IO ShouldTerminate -> IO ShouldTerminate
    go testNum runMoreTests = do
      let numNodes' = unNumNodes numNodes
          gethNums = [1..GethId numNodes']
          cEnv = mkLocalEnv numNodes'
               & clusterPrivacySupport .~ privacySupport

      putStrLn $ "test #" ++ show (unTestNum testNum)

      result <- run cEnv $ do
        _ <- when (os == "darwin") PF.acquirePf

        geths <- wipeAndSetupNodes Nothing "gdata" gethNums
        when (privacySupport == PrivacyEnabled) (startConstellationNodes geths)
        instruments <- traverse (runNode numNodes') geths

        timestampedMessage "awaiting a successful raft election"
        awaitAll (assumedRole <$> instruments)
        timestampedMessage "initial election succeeded"

        -- perform the actual test
        cb (zip geths instruments)

        -- pause a second before checking last block
        td 1

        let verifier = liftIO $ verify
              (lastBlock <$> instruments)
              (outstandingTxes <$> instruments)
              (nodeTerminated <$> instruments)

        -- wait an extra five seconds to guarantee raft has a chance to
        -- converge
        verifier >>= \case
          Falsified (WrongOrder _ _) -> td 5
          Falsified NoBlockFound     -> td 5
          _                          -> pure ()

        verifier

      print result
      case result of
        Left reason -> print reason >> pure DoTerminateFailure
        Right result' -> case p testNum result' of
          DontTerminate -> runMoreTests
          term -> pure term

-- Run nodes in a local cluster environment.
run :: ClusterEnv -> TestM a -> IO (Either FailureReason a)
run cEnv action = do
  var <- newEmptyMVar
  sh $ do
    result <- runReaderT (runExceptT action) cEnv
    liftIO $ putMVar var result
  readMVar var

testNTimes
  :: Int
  -> PrivacySupport
  -> NumNodes
  -> ([(Geth, NodeInstrumentation)] -> TestM ())
  -> IO ()
testNTimes times privacySupport numNodes =
  let predicate _           (Falsified _) = DoTerminateFailure
      predicate (TestNum n) _
        | n == times - 1
        = DoTerminateSuccess
      predicate _           _             = DontTerminate
  in tester predicate privacySupport numNodes

-- | Verify nodes show normal behavior:
--
-- * None have exited (this assumes termination is an error)
-- * There are no lost transactions
-- * The nodes all have the same last block
verify
  :: [Behavior (Last Block)]
  -> [Behavior OutstandingTxes]
  -> [Async NodeTerminated]
  -> IO Validity
verify lastBlockBs outstandingTxesBs terminatedAsyncs = do
  lastBlocks <- traverse observe lastBlockBs
  outstandingTxes_ <- traverse observe outstandingTxesBs
  earlyTerminations <- traverse poll terminatedAsyncs

  forM_ outstandingTxes_ $ \(OutstandingTxes txes) -> do
    let num = Set.size txes
    when (num > 0) $ putStrLn $ "Outstanding txes: " ++ show num

  let noEarlyTerminations = mconcat $ flip map earlyTerminations $ \case
        Just _  -> Falsified DidPanic -- assume termination is unexpected
        Nothing -> Verified

  pure $ mconcat
    [ noEarlyTerminations
    , verifyLastBlocks lastBlocks
    , verifyOutstandingTxes outstandingTxes_
    ]

verifyLastBlocks :: [Last Block] -> Validity
verifyLastBlocks blocks = case allSame blocks of
  NotSame a b -> Falsified $ WrongOrder a b
  Same (Last Nothing) -> Falsified NoBlockFound
  _ -> Verified

verifyOutstandingTxes :: [OutstandingTxes] -> Validity
verifyOutstandingTxes txes =
  let lostTxes :: Set TxId
      lostTxes = unOutstandingTxes (mconcat txes)
  in if Set.null lostTxes
     then Verified
     else Falsified (LostTxes lostTxes)

partition :: MonadManaged m => FilePath -> Millis -> GethId -> m ()
partition gdata millis node =
  if os == "darwin"
  then PF.partition gdata millis node >> PF.flushPf
  else IPT.partition gdata millis node

spamTransactions :: MonadIO m => [Geth] -> m ()
spamTransactions = mapM_ $ \geth -> spamGeth BenchEmptyTx geth $ perSecond 10

withSpammer :: (MonadIO m, MonadReader ClusterEnv m) => [Geth] -> m () -> m ()
withSpammer geths action = do
  spammer <- clusterAsync $ spamTransactions geths
  action
  liftIO $ cancel spammer

td :: MonadIO m => Int -> m ()
td = liftIO . threadDelay . (* second)

timestampedMessage :: MonadIO m => Text -> m ()
timestampedMessage msg = liftIO $ do
  zonedTime <- getZonedTime
  let locale = defaultTimeLocale
      formattedTime = pack $ formatTime locale "%I:%M:%S.%q" zonedTime
  T.putStrLn $ formattedTime <> ": " <> msg
