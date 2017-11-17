{-# LANGUAGE OverloadedStrings #-}

-- Test public state consistency
module QuorumTools.Test.Raft.PublicStateTest where

import           Control.Monad            (forM_)
import           Prelude                  hiding (FilePath)

import           QuorumTools.Test.Outline hiding (verify)
import           QuorumTools.Test.State
import           QuorumTools.Types

publicStateTestMain :: IO ()
publicStateTestMain = testNTimes 1 PrivacyDisabled Raft (NumNodes 3) $ \iNodes -> do
  let (geth1, geth1Instruments) = head iNodes
      (geths, instruments) = unzip iNodes
      -- sendTo = cycle geths
      sendTo = repeat geth1
      contract = simpleStorage Public

  td 1

  storageAddr <- createContract geth1 contract (txAddrs geth1Instruments)

  let increments = 10

  forM_ (take increments sendTo) $ \geth ->
    incrementStorage geth Sync contract storageAddr

  awaitBlockConvergence instruments

  [i1, i2, i3] <- traverse (getStorage contract storageAddr) geths

  let expectedValue = 42 + increments
      [id1, id2, id3] = gethId <$> geths

  expectEq
    [ (id1, expectedValue, i1)
    , (id2, expectedValue, i2)
    , (id3, expectedValue, i3)
    ]
