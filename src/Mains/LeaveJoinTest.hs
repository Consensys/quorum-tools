{-# LANGUAGE OverloadedStrings #-}

-- Test removing a node from the cluster and adding it back.
module Mains.LeaveJoinTest where

import           Control.Lens
import Turtle.Shell

import Cluster
import Cluster.Client
import Cluster.Control
import Cluster.Types
import TestOutline

leaveJoinTestMain :: IO ()
leaveJoinTestMain = do
  let gethNums = [1..3]
      cEnv = mkLocalEnv 3
           & clusterPrivacySupport .~ PrivacyDisabled

  result <- run cEnv $ do
    [g1, g2, g3] <- wipeAndSetupNodes Nothing "gdata" gethNums

    instruments <- traverse (runNode 3) [g1, g2, g3]

    awaitAll (assumedRole <$> instruments)

    timestampedMessage "starting test with a pause"
    td 2

    withSpammer [g1, g2, g3] $ td 1

    -- remove g1, pause, add it back
    membershipChange g2 (RemoveNode g1)

    withSpammer [g2, g3] $ td 1

    membershipChange g3 (AddNode g1)

    withSpammer [g1, g2, g3] $ td 1
    td 1

    liftIO $ verify
      (lastBlock <$> instruments)
      (outstandingTxes <$> instruments)
      (nodeTerminated <$> instruments)

  print result
