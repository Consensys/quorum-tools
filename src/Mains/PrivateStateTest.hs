{-# LANGUAGE OverloadedStrings #-}

-- Test private state consistency
module Mains.PrivateStateTest where

import           Control.Monad            (forM_)

import           Cluster.StateTestsShared
import           Cluster.Types
import           Prelude                  hiding (FilePath)
import           TestOutline              hiding (verify)
import           Turtle                   hiding (match)

clusterMain :: IO ()
clusterMain = testNTimes 5 PrivacyEnabled (NumNodes 3) $ \iNodes -> do
  let [g1, g2, g3] = fst <$> iNodes
      (_, geth1Instruments) = head iNodes

  key3 <- liftIO $
    readTextFile "credentials/constellation-keys/3/constellation.pub"
  -- geth1 and geth3 are both party to this tx, but geth2 is not
  let privStorage = simpleStorage (PrivateFor [Secp256k1 key3])
  privStorageAddr <- createContract g1 privStorage (txAddrs geth1Instruments)

  -- The storage starts with a value of 42 and we increment it five times
  forM_ [1..5] $ \no -> do
    incrementStorage g1 privStorage privStorageAddr

    i1 <- getStorage g1 privStorage privStorageAddr
    expectEq i1 (42 + no)

    i2 <- getStorage g2 privStorage privStorageAddr
    expectEq i2 0
    -- TODO confirm geth2 also gets transaction

    i3 <- getStorage g3 privStorage privStorageAddr
    expectEq i3 (42 + no)
