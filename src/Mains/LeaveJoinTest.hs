{-# LANGUAGE OverloadedStrings #-}

-- Test removing a node from the cluster and adding it back.
--
-- NOTE: this test is no longer valid -- raft does not allow a geth to re-join
-- the cluster after it's been removed. the node would have to re-join with a
-- new ID.
--
module Mains.LeaveJoinTest where

import           Control.Lens
import Turtle.Shell

import Cluster
import Cluster.Control
import Cluster.Types
import TestOutline

leaveJoinTestMain :: IO ()
leaveJoinTestMain = do
  let gids = [1..3] :: [GethId]
      password = CleartextPassword "abcd"

  keys <- generateClusterKeys (length gids) password
  let cEnv = mkLocalEnv keys
           & clusterPrivacySupport .~ PrivacyDisabled
           & clusterPassword       .~ password

  result <- run cEnv $ do
    [g1, g2, g3] <- wipeAndSetupNodes Nothing "gdata" gids

    instruments <- traverse (runNode 3) [g1, g2, g3]

    awaitAll (assumedRole <$> instruments)

    timestampedMessage "starting test with a pause"
    td 2

    withSpammer [g1, g2, g3] $ td 1

    -- remove g1, pause, add it back
    g2 `removesNode` g1

    withSpammer [g2, g3] $ td 1

    -- TODO: update g1's GethId to be 4.

    g3 `addsNode` g1

    -- TODO: start geth 4

    withSpammer [g1, g2, g3] $ td 1
    td 1

    liftIO $ verify
      (lastBlock <$> instruments)
      (outstandingTxes <$> instruments)
      (nodeTerminated <$> instruments)

  print result
