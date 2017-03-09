{-# LANGUAGE OverloadedStrings #-}

-- Test public state consistency
module Mains.PublicStateTest where

import           Control.Monad              (forM_)

import Prelude hiding (FilePath)
import Cluster.Types
import TestOutline hiding (verify)
import Cluster.StateTestsShared

publicStateTestMain :: IO ()
publicStateTestMain = testNTimes 5 (NumNodes 3) $ \iNodes -> do
  let (geth1, geth1Instruments) = head iNodes
      sendTo = cycle (fst <$> iNodes)
      contract = simpleStorage Public
  storageAddr
    <- createContract geth1 contract (txAddrs geth1Instruments)

  forM_ (zip [1..10] sendTo) $ \(no, geth) -> do
    incrementStorage geth contract storageAddr
    i <- getStorage geth contract storageAddr
    expectEq i (42 + no)
