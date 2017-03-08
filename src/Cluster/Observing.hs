{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
module Cluster.Observing where

import           Control.Concurrent.MVar    (isEmptyMVar, modifyMVar_,
                                             newEmptyMVar, newMVar, putMVar,
                                             readMVar, tryTakeMVar)
import           Data.Bifunctor             (first, second)
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

import           Control

-- | Helper for the most common (only) use case for matchCheckpoint.
matchCheckpoint' :: Checkpoint a -> Line -> (a -> IO ()) -> Shell ()
matchCheckpoint' cpt line cb = case matchCheckpoint cpt line of
  Just a  -> liftIO (cb a)
  Nothing -> pure ()

startObserving :: Shell Line -> Shell (Line, ())
startObserving incoming = (, ()) <$> incoming

observingTransition :: (a -> Bool) -> Shell (a, b) -> Shell (a, (Transitioned, b))
observingTransition test lines = do
  mvar <- liftIO newEmptyMVar
  (line, b) <- lines
  isEmpty <- liftIO $ isEmptyMVar mvar

  let thisIsTheLine = test line
  when (isEmpty && thisIsTheLine) $ liftIO $ putMVar mvar ()

  let isPost = not isEmpty || thisIsTheLine
  return (line, (if isPost then PostTransition else PreTransition, b))

-- TODO: move from tuple to first-class data type
-- TODO: once we have >1 data type (e.g. regular vs partition testing), we can
--       use a typeclass ReportsOnline/ReportsBooted/HasBooted to access the
--       (Maybe NodeOnline) field.
observingBoot :: Shell (Line, a) -> Shell (Line, (Maybe NodeOnline, a))
observingBoot lines = (second.first) isOnline <$> observingTransition ipcOpened lines
  where
    ipcOpened line = "IPC endpoint opened:" `isInfixOf` lineToText line
    isOnline = \case
      PreTransition -> Nothing
      PostTransition -> Just NodeOnline

observingLastBlock :: Shell (Line, a)
                   -> Shell (Line, (Last Block, a))
observingLastBlock incoming = do
    st <- liftIO $ newMVar (mempty :: Last Block)
    (line, a) <- incoming
    case matchOnce blockPattern (lineToText line) of
      Just latest -> liftIO $ modifyMVar_ st (\prev -> pure (prev <> pure latest))
      _           -> pure ()
    lastBlock <- liftIO (readMVar st)
    return (line, (lastBlock, a))

  where
    blockPattern :: Pattern Block
    blockPattern = has $
      Block . pack <$> ("Successfully extended chain: " *> count 64 hexDigit)

observingTxes
  :: Shell (Line, a)
  -> Shell (Line, ((OutstandingTxes, TxAddrs), a))
observingTxes incoming = do
    outstanding <- liftIO $ newMVar mempty
    addrs <- liftIO $ newMVar mempty
    (line, a) <- incoming

    matchCheckpoint' TxCreated line $ \(tx, addr) -> do
      pureModifyMVar_ outstanding (Set.insert tx)
      pureModifyMVar_ addrs (Map.insert tx addr)

    matchCheckpoint' TxAccepted line $ \tx ->
      pureModifyMVar_ outstanding (Set.delete tx)

    txResult <- liftIO $ OutstandingTxes <$> readMVar outstanding
    addrsResult <- liftIO $ TxAddrs <$> readMVar addrs

    return (line, ((txResult, addrsResult), a))

observingRaftStatus :: Shell (Line, a)
                    -> Shell (Line, (Last RaftStatus, a))
observingRaftStatus incoming = do
    st <- liftIO $ newMVar (mempty :: Last RaftStatus)
    (line, a) <- incoming
    case match statusPattern (lineToText line) of
      [raftStatus] -> liftIO $ modifyMVar_ st (const $ pure $ pure raftStatus)
      _            -> pure ()
    lastRaftStatus <- liftIO (readMVar st)
    return (line, (lastRaftStatus, a))

  where
    statusPattern :: Pattern RaftStatus
    statusPattern = has $ RaftStatus <$> (text " became "  *> fmap toRole (plus lower))
                                     <*> (text " at term " *> decimal)

    toRole :: Text -> RaftRole
    toRole "follower"  = Follower
    toRole "candidate" = Candidate
    toRole "leader"    = Leader
    toRole unknown = error $ "failed to parse unknown raft role: " ++ T.unpack unknown

observingRoles
  :: Shell (Line, a)
  -> Shell (Line, (Maybe AssumedRole, a))
observingRoles incoming = do
  roleMV <- liftIO newEmptyMVar
  (line, a) <- incoming

  matchCheckpoint' BecameMinter line $ \() ->
    void $ ensureMVarTransition roleMV AssumedRole

  matchCheckpoint' BecameVerifier line $ \() ->
    void $ ensureMVarTransition roleMV AssumedRole

  status <- liftIO $ tryTakeMVar roleMV
  return (line, (status, a))

observingActivation :: Shell (Line, a)
                    -> Shell (Line, (Set GethId, a))
observingActivation incoming = do
  connections <- liftIO $ newMVar (mempty :: Set GethId)
  (line, a) <- incoming

  matchCheckpoint' PeerConnected line $ \(PeerJoined joined) ->
    pureModifyMVar_ connections (Set.insert joined)

  matchCheckpoint' PeerDisconnected line $ \(PeerLeft left) ->
    pureModifyMVar_ connections (Set.delete left)

  result <- liftIO $ readMVar connections

  return (line, (result, a))
