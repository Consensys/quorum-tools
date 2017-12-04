{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module QuorumTools.Metrics
  ( Store (..)
  , Metric (..)
  , blackhole
  , localEkg
  ) where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Text                   (Text)
import           Data.Time.Units             (Microsecond, getCPUTimeWithUnit,
                                              toMicroseconds)
import qualified System.Metrics              as EKG
import qualified System.Remote.Monitoring    as EKG
import qualified System.Metrics.Counter      as Counter
import qualified System.Metrics.Distribution as Dist

import           QuorumTools.Types

data Metric a where
  SendTx :: Metric (Either Text TxId) -- NOTE: this could take an arg

class Monad m => Store m s where
  log :: s        -- ^ metrics store
      -> Metric a -- ^ associated metric
      -> m a      -- ^ action that produces an 'a'
      -> m a      -- ^ new action augmented with instrumentation

newtype MetricLogger m = MetricLogger (forall a. Metric a -> m a -> m a)

--
-- Blackhole / no-op
--

data Blackhole = Blackhole

instance Monad m => Store m Blackhole where
  log Blackhole _metric = id

blackhole :: Blackhole
blackhole = Blackhole

--
-- EKG
--
-- TODO: break this into a submodule
--

data LocalEkg m
  = LocalEkg
    { _ekgServer :: EKG.Server
    , _ekgLog    :: MetricLogger m
    }

mkLocalEkgServer :: MonadIO m => Int -> m EKG.Server
mkLocalEkgServer = liftIO . EKG.forkServer "localhost"

currentMicros :: MonadIO m => m Integer
currentMicros = liftIO $
  toMicroseconds <$> (getCPUTimeWithUnit :: IO Microsecond)

-- We separate logger- from server creation so we have the option of easily
-- using other ekg backends (e.g. prometheus)
mkEkgLogger :: MonadIO m => EKG.Store -> m (MetricLogger m)
mkEkgLogger store = liftIO $ do
  txSentTotal    <- EKG.createCounter "cluster.tx.submit.total" store
  txSentAccepted <- EKG.createCounter "cluster.tx.submit.accepted" store
  txSentRejected <- EKG.createCounter "cluster.tx.submit.rejected" store
  txSentRtt      <- EKG.createDistribution "cluster.tx.submit.rtt_μs" store

  return $ MetricLogger $ \metric act ->
    case metric of
      SendTx -> do
        before <- currentMicros
        val <- act
        after <- currentMicros
        --
        -- NOTE: json decoding is currently captured in rtt time
        --
        let rtt = fromIntegral $ after - before

        liftIO $ do
          Counter.inc txSentTotal
          case val of
            Left _ -> Counter.inc txSentRejected
            Right _ -> Counter.inc txSentAccepted
          Dist.add txSentRtt rtt

        return val

localEkg :: MonadIO m => Int -> m (LocalEkg m)
localEkg port = do
  server <- mkLocalEkgServer port
  logger <- mkEkgLogger $ EKG.serverMetricStore server

  return $ LocalEkg server logger

instance MonadIO m => Store m (LocalEkg m) where
  log (LocalEkg _ (MetricLogger logMetric)) = logMetric
