{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
module Cluster.Observing where

import qualified Data.Map.Strict            as Map
import           Data.Monoid                (Last, (<>))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text, isInfixOf, pack)
import qualified Data.Text                  as T
import           Prelude                    hiding (FilePath, lines)
import           Turtle                     hiding (env, view)

import           Checkpoint
import           Cluster.Types              hiding (lastBlock, lastRaftStatus)
import           Cluster.Util               (matchOnce)

-- | Helper for the most common (only) use case for matchCheckpoint.
matchCheckpoint' :: Checkpoint a -> Text -> (a -> IO ()) -> IO ()
matchCheckpoint' cpt line cb = case matchCheckpoint cpt line of
  Just a  -> cb a
  Nothing -> pure ()

observingLines :: (Text -> IO ()) -> Shell Line -> Shell Line
observingLines action lines = do
  line <- lines
  liftIO $ action (lineToText line)
  return line

observingBoot :: IO () -> Shell Line -> Shell Line
observingBoot trigger = observingLines $ \line ->
  when ("IPC endpoint opened:" `isInfixOf` line) trigger

observingLastBlock
  :: ((Last Block -> Last Block) -> IO ())
  -> Shell Line
  -> Shell Line
observingLastBlock updateLastBlock = observingLines $ \line ->
    case matchOnce blockPattern line of
      Just latest -> updateLastBlock (<> pure latest)
      _           -> pure ()

  where
    blockPattern :: Pattern Block
    blockPattern = has $
      Block . pack <$> ("Successfully extended chain: " *> count 64 hexDigit)

observingTxes
  :: ((OutstandingTxes -> OutstandingTxes) -> IO ())
  -> ((TxAddrs -> TxAddrs) -> IO ())
  -> Shell Line
  -> Shell Line
observingTxes updateOutstanding updateAddrs = observingLines $ \line -> do
    matchCheckpoint' TxCreated line $ \(tx, addr) -> do
      updateOutstanding (OutstandingTxes . Set.insert tx . unOutstandingTxes)
      updateAddrs (TxAddrs . Map.insert tx addr . unTxAddrs)

    matchCheckpoint' TxAccepted line $ \tx ->
      updateOutstanding (OutstandingTxes . Set.delete tx . unOutstandingTxes)

observingRaftStatus
  :: ((Last RaftStatus -> Last RaftStatus) -> IO ())
  -> Shell Line
  -> Shell Line
observingRaftStatus updateRaftStatus = observingLines $ \line ->
    case matchOnce statusPattern line of
      Just raftStatus -> liftIO $ updateRaftStatus (<> pure raftStatus)
      _               -> pure ()

  where
    statusPattern :: Pattern RaftStatus
    statusPattern = has $ RaftStatus <$> (text " became "  *> fmap toRole (plus lower))
                                     <*> (text " at term " *> decimal)

    toRole :: Text -> RaftRole
    toRole "follower"  = Follower
    toRole "candidate" = Candidate
    toRole "leader"    = Leader
    toRole unknown = error $ "failed to parse unknown raft role: " ++ T.unpack unknown

observingRoles :: IO () -> Shell Line -> Shell Line
observingRoles trigger = observingLines $ \line -> do
  matchCheckpoint' BecameMinter   line $ \() -> trigger
  matchCheckpoint' BecameVerifier line $ \() -> trigger

observingActivation
  :: ((Set GethId -> Set GethId) -> IO ())
  -> Shell Line
  -> Shell Line
observingActivation updateConnections = observingLines $ \line -> do
  matchCheckpoint' PeerConnected line $ \(PeerJoined joined) ->
    updateConnections (Set.insert joined)

  matchCheckpoint' PeerDisconnected line $ \(PeerLeft left) ->
    updateConnections (Set.delete left)
