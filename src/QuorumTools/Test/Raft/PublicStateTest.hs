{-# LANGUAGE OverloadedStrings #-}

-- Test public state consistency
module QuorumTools.Test.Raft.PublicStateTest where

import           Control.Monad            (forM_)
import           Prelude                  hiding (FilePath)

import           QuorumTools.Test.Outline hiding (verify)
import           QuorumTools.Test.State
import           QuorumTools.Types

publicStateTestMain :: IO ()
publicStateTestMain = testNTimes 1 PrivacyDisabled (NumNodes 3) $ \iNodes -> do
  let (geth1, geth1Instruments) = head iNodes
      geths = fst <$> iNodes
      sendTo = cycle geths
      contract = simpleStorage Public
      instruments = snd <$> iNodes

  storageAddr <- createContract geth1 contract (txAddrs geth1Instruments)

  let increments = 10

  forM_ (take increments sendTo) $ \geth ->
    incrementStorage geth contract storageAddr

  awaitBlockConvergence instruments

  let expectedValue = 42 + increments

  forM_ geths $ \geth -> do
    i <- getStorage geth contract storageAddr
    expectEq i expectedValue
