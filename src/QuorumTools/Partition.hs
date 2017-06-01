{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module QuorumTools.Partition where

import qualified Control.Foldl              as Fold
import           Control.Lens.Prism         (prism', Prism')
import           Data.List                  (nub)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (First(First), getFirst)
import           Prelude                    hiding (FilePath, lines)
import           Text.Read                  (readMaybe)
import           Turtle

import           QuorumTools.Cluster
import           QuorumTools.Types
import           QuorumTools.Util           (matchOnce)

inshellWithNoErr :: Text -> Shell Line -> Shell Line
inshellWithNoErr cmd inputShell = do
  line <- inshellWithErr cmd inputShell
  case line of
    Left _shellErr -> empty
    Right out -> pure out

portPattern :: Pattern Port
portPattern = has $ do
  let matchPort = bounded 4 5 digit
  str <-
    "*:"         *> matchPort <* " (LISTEN)" <|>
    "localhost:" *> matchPort <* "->"

  case readMaybe str of
    Just portNum -> return (Port portNum)
    Nothing -> mzero

getPorts :: MonadIO io => Pid -> io [Port]
getPorts (Pid pid) =
  let helper :: Line -> Maybe Port
      helper t = case match portPattern (lineToText t) of
        [p] -> Just p
        _ -> Nothing

      matches' :: Prism' Line Port
      matches' = prism' (error "XXX(joel)") helper

      findPorts :: Fold Line [Port]
      findPorts = Fold.handles matches' Fold.list

      cmd = format ("lsof -p "%d) pid
  in nub <$> fold (inshell cmd "") findPorts

getPid :: MonadIO io => DataDir -> io Pid
getPid gDataDir =
  let pidPat :: Pattern Pid
      pidPat = do
        str <- "p" *> bounded 2 6 digit
        case readMaybe str of
          Just pid -> return (Pid pid)
          Nothing -> mzero

      forceFirst :: First Pid -> Pid
      forceFirst =
        fromMaybe (error "failed to find pid (check you're a sudoer)")
        . getFirst

      step :: First Pid -> Line -> First Pid
      step acc line = (acc <> First (matchOnce pidPat (lineToText line)))

      findPid :: Fold Line Pid
      findPid = Fold step mempty forceFirst

  in do
       let cmd' = format ("lsof -Fp "%fp) (gethIpcPath gDataDir)
       fold (inshell cmd' "") findPid

getPortsForGeth :: MonadIO io => FilePath -> GethId -> io [Port]
getPortsForGeth ddRoot gid = do
  -- Caution:
  -- lsof requires an absolute path (who knew) -- otherwise it returns nothing
  base <- pwd
  let gDataDir = DataDir $ base </> ddRoot </> fromText (nodeName gid)
  getPid gDataDir >>= getPorts
