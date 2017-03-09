{-# LANGUAGE OverloadedStrings #-}

-- Test public state consistency
module Mains.PublicStateTest where

import           Control.Monad              (forM_)
import           Control.Monad.Reader       (ReaderT (runReaderT))

import Prelude hiding (FilePath)
import Turtle hiding (match)
import Cluster
import Cluster.Control
import Cluster.Types
import TestOutline hiding (verify)
import Cluster.StateTestsShared

publicStateTestMain :: IO ()
publicStateTestMain = sh $ flip runReaderT startEnv $ do
  let gids = clusterGids clusterSize

  geths <- wipeAndSetupNodes Nothing "gdata" gids
  instruments <- traverse (runNode clusterSize) geths

  timestampedMessage "awaiting a successful raft election"
  awaitAll (assumedRole <$> instruments)
  timestampedMessage "initial election succeeded"

  let sendTo = cycle geths
      contract = simpleStorage Public
  storageAddr <- createContract (head geths) contract

  forM_ (zip [1..10] sendTo) $ \(no, geth) -> do
    incrementStorage geth contract storageAddr
    i <- getStorage geth contract storageAddr
    expectEq i (42 + no)

  liftIO $ putStrLn "all successful!"
