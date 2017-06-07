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
  let [g1, g2, g3] = fst <$> iNodes
      (_, geth1Instruments) = head iNodes

  -- geth1 and geth3 are both party to this tx, but geth2 is not
  key3 <- liftIO $ readTextFile "gdata/geth3/keys/constellation.pub"

  let privStorage = simpleStorage (PrivateFor [Secp256k1 key3])
  privStorageAddr <- createContract g1 privStorage (txAddrs geth1Instruments)

  -- The storage starts with a value of 42 and we increment it five times
  let increments = 5
  replicateM_ increments $ incrementStorage g1 privStorage privStorageAddr

  td 2

  let expectedPrivateValue = 42 + increments

  i1 <- getStorage g1 privStorage privStorageAddr
  expectEq i1 expectedPrivateValue

  i2 <- getStorage g2 privStorage privStorageAddr
  expectEq i2 0
  -- TODO confirm geth2 also gets transaction

  i3 <- getStorage g3 privStorage privStorageAddr
  expectEq i3 expectedPrivateValue
