{-# LANGUAGE OverloadedStrings #-}

-- Test public state consistency
module Mains.PublicStateTest where

import           Control.Monad            (forM_)
import           Prelude                  hiding (FilePath)

import           Cluster.StateTestsShared
import           Cluster.Types
import           TestOutline              hiding (verify)

publicStateTestMain :: IO ()
publicStateTestMain = testNTimes 5 PrivacyDisabled (NumNodes 3) $ \iNodes -> do
  let (geth1, geth1Instruments) = head iNodes
      geths = fst <$> iNodes
      sendTo = cycle geths
      contract = simpleStorage Public

  storageAddr <- createContract geth1 contract (txAddrs geth1Instruments)

  let increments = 10

  forM_ (take increments sendTo) $ \geth ->
    incrementStorage geth contract storageAddr
  td 1

  let expectedValue = 42 + increments

  forM_ geths $ \geth -> do
    i <- getStorage geth contract storageAddr
    expectEq i expectedValue
