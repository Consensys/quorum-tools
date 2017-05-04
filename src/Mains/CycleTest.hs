{-# LANGUAGE OverloadedStrings #-}

-- Test removing nodes from the cluster and adding new ones.
--
-- Cycle through nodes until we've completely replaced the originals.
module Mains.CycleTest where

import           Control.Lens
import           Data.Monoid     ((<>))
import           Data.Set        as Set
import qualified Data.Text       as T
import           Turtle          (MonadIO, liftIO)

import           Cluster
import           Cluster.Client
import           Cluster.Control
import           Cluster.Types
import           TestOutline

proposes :: MonadIO m => Geth -> MembershipChange -> m ()
geth `proposes` change = do
  let message = T.pack $ case change of
        AddNode target -> "adding node " <> show (gId (gethId target))
        RemoveNode target -> "removing node " <> show (gId (gethId target))

  timestampedMessage $ "waiting before " <> message
  td 2
  timestampedMessage message
  membershipChange geth change

cycleTestMain :: IO ()
cycleTestMain = do
  let gids = [1..6] :: [GethId]
      cEnv = mkLocalEnv 6
           & clusterPrivacySupport .~ PrivacyDisabled
           & clusterInitialMembers .~ Set.fromList (take 3 gids)

  result <- run cEnv $ do
    [g1, g2, g3, g4, g5, g6] <- wipeAndSetupNodes Nothing "gdata" gids

    startInstrs <- traverse (runNode 3) [g1, g2, g3]
    endInstrs <- traverse (runNode 3) [g4, g5, g6]

    timestampedMessage "waiting for initial nodes to assume raft roles"

    awaitAll (assumedRole <$> startInstrs)

    timestampedMessage "starting test with a pause"

    td 2
    sendEmptyTx g1

    g2 `proposes` AddNode g4

    g3 `proposes` RemoveNode g1

    td 2
    sendEmptyTx g2

    g3 `proposes` AddNode g5

    g4 `proposes` RemoveNode g2

    td 2
    sendEmptyTx g3

    g4 `proposes` AddNode g6

    g5 `proposes` RemoveNode g3

    td 2
    withSpammer [g4, g5, g6] $ td 2

    td 2
    liftIO $ verify
      (lastBlock <$> endInstrs)
      (outstandingTxes <$> endInstrs)
      (nodeTerminated <$> endInstrs)

  print result
