{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

module PacketFilter (acquirePf, partition) where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (bracket)
import qualified Control.Foldl              as Fold
import           Control.Lens.Prism         (prism', Prism')
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader.Class (MonadReader (reader))
import           Data.List                  (nub)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (First(First), getFirst)
import           Data.Text                  (pack)
import qualified Data.Text                  as T
import           Prelude                    hiding (FilePath, lines)
import           Text.Read                  (readMaybe)
import           Turtle
import Cluster

newtype PfToken = PfToken Text

newtype Pid = Pid Int

inshellWithNoErr :: Text -> Shell Text -> Shell Text
inshellWithNoErr cmd inputShell = do
  line <- inshellWithErr cmd inputShell
  case line of
    Left _err -> empty
    Right out -> pure out

-- create something like this:
--
-- anchor "raft-anchor" {
--   anchor "geth1" {
--     pass
--   }
--   anchor "geth2" {
--     pass
--   }
--   anchor "geth3" {
--     pass
--   }
-- }
emptyRuleset :: [GethId] -> Text
emptyRuleset geths =
  let gethRule (GethId i) =
        [ format ("anchor \"geth"%d%"\" {") i
        , "pass"
        , "}"
        ]
      gethRules = concatMap gethRule geths
      gethRules_ = map ("  " <>) gethRules
      anchorLines =
        [ "anchor \"raft-anchor\" {" ] <>
        gethRules_ <>
        [ "}" ]
  in T.unlines anchorLines

acquirePfHelper :: IO PfToken
acquirePfHelper =
  let tokenPat :: Pattern PfToken
      tokenPat = PfToken . pack <$> ("Token : " *> some digit)

      forceFirst :: First PfToken -> PfToken
      forceFirst = fromMaybe (error "failed to find pf token") . getFirst

      step :: First PfToken -> Either Text Text -> First PfToken
      step acc line = (acc <> First (matchOnce tokenPat (either id id line)))

      findToken :: Fold (Either Text Text) PfToken
      findToken = Fold step mempty forceFirst

  in fold (inshellWithErr "sudo -n pfctl -E" "") findToken

-- | Turn on pf and initialize with empty rules for every geth node.
--
-- Must be called once at the beginning of a test.
acquirePf :: MonadManaged m => [GethId] -> m ()
acquirePf geths = do
  -- enable pf / increment its enable reference count, then release on exit
  _ <- using $ managed $ bracket
    acquirePfHelper
    (\(PfToken tk) -> sh $ inshellWithNoErr (format ("sudo -n pfctl -X "%s) tk) "")

  -- write the initial ruleset which passes packets through to each geth
  ruleFile <- using $ fileContaining $ pure (emptyRuleset geths)
  view $ inshellWithNoErr (format ("sudo -n pfctl -f "%fp) ruleFile) ""

  -- flush the rules we added on exit
  _ <- using $ managed (onExit (sh $ inshellWithNoErr "sudo -n pfctl -a 'raft-anchor' -F rules" ""))
  return ()

matchOnce :: Pattern a -> Text -> Maybe a
matchOnce pat line = case match pat line of
  [result] -> Just result
  _        -> Nothing

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
  let helper :: Text -> Maybe Port
      helper t = case match portPattern t of
        [p] -> Just p
        _ -> Nothing

      matches' :: Prism' Text Port
      matches' = prism' (error "XXX(joel)") helper

      findPorts :: Fold Text [Port]
      findPorts = Fold.handles matches' Fold.list

      cmd = format ("lsof -p "%d) pid
  in nub <$> fold (inshell cmd "") findPorts

getPid :: MonadIO io => FilePath -> GethId -> io Pid
getPid gdata (GethId gid) =
  let pidPat :: Pattern Pid
      pidPat = do
        str <- "p" *> bounded 2 6 digit
        case readMaybe str of
          Just pid -> return (Pid pid)
          Nothing -> mzero

      forceFirst :: First Pid -> Pid
      forceFirst = fromMaybe (error "failed to find pid") . getFirst

      step :: First Pid -> Text -> First Pid
      step acc line = (acc <> First (matchOnce pidPat line))

      findPid :: Fold Text Pid
      findPid = Fold step mempty forceFirst

      cmd :: Text
      cmd = format ("lsof -Fp "%fp%"/geth"%d%"/geth.ipc") gdata gid

  in fold (inshell cmd "") findPid

getPortsForGeth :: MonadIO io => FilePath -> GethId -> io [Port]
getPortsForGeth gdata = getPid gdata >=> getPorts

-- | Make a packet filter rule to block a specific port.
blockPortsRule :: [Port] -> Text
blockPortsRule ports =
  let ports_ = T.intercalate ", " (map (T.pack . show . getPort) ports)
  in format ("block quick proto { tcp, udp } from any to any port { "%s%" }") ports_

-- | Partition some geth node for a number of milliseconds.
--
-- TODO: This will currently only work for partitioning a single node.
partition :: (MonadManaged m, HasEnv m) => Millis -> GethId -> m ()
partition (Millis ms) geth = do
  gdata <- reader clusterDataRoot
  ports <- getPortsForGeth gdata geth
  let anchor = format ("raft-anchor/geth"%d) (gId geth)
  _ <- sh $ inshellWithNoErr
    (format ("sudo -n pfctl -a "%s%" -f -") anchor)
    (select [blockPortsRule ports])

  -- make sure to reset pf.conf on exit
  _ <- using $ managed (onExit (sh $ inshellWithNoErr "sudo -n pfctl -a raft-partition" ""))

  liftIO $ threadDelay (1000 * ms)
