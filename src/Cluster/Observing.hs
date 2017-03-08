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
matchCheckpoint' :: Checkpoint a -> Line -> (a -> IO ()) -> Shell ()
matchCheckpoint' cpt line cb = case matchCheckpoint cpt line of
  Just a  -> liftIO (cb a)
  Nothing -> pure ()

observingTransition :: (a -> Bool) -> IO () -> Shell a -> Shell a
observingTransition test trigger lines = do
  line <- lines
  when (test line) (liftIO trigger)
  return line

observingBoot :: IO () -> Shell Line -> Shell Line
observingBoot trigger lines =
  let ipcOpened line = "IPC endpoint opened:" `isInfixOf` lineToText line
  in observingTransition ipcOpened trigger lines

observingLastBlock
  :: ((Last Block -> Last Block) -> IO ())
  -> Shell Line
  -> Shell Line
observingLastBlock updateLastBlock incoming = do
    line <- incoming
    case matchOnce blockPattern (lineToText line) of
      Just latest -> liftIO $ updateLastBlock (<> pure latest)
      _           -> pure ()
    return line

  where
    blockPattern :: Pattern Block
    blockPattern = has $
      Block . pack <$> ("Successfully extended chain: " *> count 64 hexDigit)

observingTxes
  :: ((OutstandingTxes -> OutstandingTxes) -> IO ())
  -> ((TxAddrs -> TxAddrs) -> IO ())
  -> Shell Line
  -> Shell Line
observingTxes updateOutstanding updateAddrs incoming = do
    line <- incoming

    matchCheckpoint' TxCreated line $ \(tx, addr) -> do
      updateOutstanding (OutstandingTxes . Set.insert tx . unOutstandingTxes)
      updateAddrs (TxAddrs . Map.insert tx addr . unTxAddrs)

    matchCheckpoint' TxAccepted line $ \tx ->
      updateOutstanding (OutstandingTxes . Set.delete tx . unOutstandingTxes)

    return line

observingRaftStatus
  :: ((Last RaftStatus -> Last RaftStatus) -> IO ())
  -> Shell Line
  -> Shell Line
observingRaftStatus updateRaftStatus incoming = do
    line <- incoming
    case matchOnce statusPattern (lineToText line) of
      Just raftStatus -> liftIO $ updateRaftStatus (<> pure raftStatus)
      _               -> pure ()
    return line

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
observingRoles trigger incoming = do
  line <- incoming
  matchCheckpoint' BecameMinter line $ \() -> trigger
  matchCheckpoint' BecameVerifier line $ \() -> trigger
  return line

observingActivation
  :: ((Set GethId -> Set GethId) -> IO ())
  -> Shell Line
  -> Shell Line
observingActivation updateConnections incoming = do
  line <- incoming

  matchCheckpoint' PeerConnected line $ \(PeerJoined joined) ->
    updateConnections (Set.insert joined)

  matchCheckpoint' PeerDisconnected line $ \(PeerLeft left) ->
    updateConnections (Set.delete left)

  return line
