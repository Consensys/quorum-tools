{-# LANGUAGE OverloadedStrings #-}

-- Test private state consistency
module QuorumTools.Test.Raft.PrivateStateTest where

import           Prelude                  hiding (FilePath)
import           Turtle                   hiding (match)

import           QuorumTools.Test.Outline hiding (verify)
import           QuorumTools.Test.State
import           QuorumTools.Types

privateStateTestMain :: IO ()
privateStateTestMain = testNTimes 1 PrivacyEnabled (NumNodes 3) $ \iNodes -> do
  let (geths, instruments) = unzip iNodes
      (g1, geth1Instruments) = head iNodes

  -- geth1 and geth3 are both party to this tx, but geth2 is not
  key3 <- liftIO $ readTextFile "gdata/geth3/keys/constellation.pub"

  td 2

  let privStorage = simpleStorage (PrivateFor [Secp256k1 key3])
  privStorageAddr <- createContract g1 privStorage (txAddrs geth1Instruments)

  -- The storage starts with a value of 42 and we increment it five times
  let increments = 5
  replicateM_ increments $ incrementStorage g1 Sync privStorage privStorageAddr

  awaitBlockConvergence instruments

  [i1, i2, i3] <- traverse (getStorage privStorage privStorageAddr) geths

  let expectedPrivateValue = 42 + increments
      [id1, id2, id3] = gethId <$> geths

  expectEq
    [ (id1, expectedPrivateValue, i1)
    , (id2,                    0, i2)
    , (id3, expectedPrivateValue, i3)
    ]
