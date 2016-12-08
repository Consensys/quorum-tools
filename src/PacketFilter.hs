{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns          #-}

module PacketFilter (acquirePf, partition) where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (bracket)
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader.Class (MonadReader (reader))
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (First(First), getFirst)
import           Data.Text                  (pack)
import qualified Data.Text                  as T
import           Prelude                    hiding (FilePath, lines)
import           Turtle

import Cluster
import SharedPartitioning

pfctl :: Format Text r -> r
pfctl args = format ("sudo -n pfctl "%args)

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
      forceFirst =
        fromMaybe (error "failed to find pf token (check you're a sudoer)")
        . getFirst

      step :: First PfToken -> Either Text Text -> First PfToken
      step acc line = (acc <> First (matchOnce tokenPat (either id id line)))

      findToken :: Fold (Either Text Text) PfToken
      findToken = Fold step mempty forceFirst

  in fold (inshellWithErr (pfctl "-E") "") findToken

-- | Turn on pf and initialize with empty rules for every geth node.
--
-- Must be called once at the beginning of a test.
acquirePf :: MonadManaged m => [GethId] -> m ()
acquirePf geths = do
  -- enable pf / increment its enable reference count, then release on exit
  -- Note: this could fail if we're not sudoers -- bind strictly to trigger
  -- failure
  !_ <- using $ managed $ bracket
    acquirePfHelper
    (\(PfToken tk) -> sh $ inshellWithNoErr (pfctl ("-X "%s) tk) "")

  -- write the initial ruleset which passes packets through to each geth
  ruleFile <- using $ fileContaining $ pure (emptyRuleset geths)
  view $ inshellWithNoErr (pfctl ("-f "%fp) ruleFile) ""

  -- flush the rules we added on exit
  !_ <- using $ managed $ onExit $ sh $
    inshellWithNoErr (pfctl "-a 'raft-anchor' -F rules") ""
  return ()

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
    (pfctl ("-a "%s%" -f -") anchor)
    (select [blockPortsRule ports])

  -- make sure to reset pf.conf on exit
  !_ <- using $ managed $ onExit $ sh $
    inshellWithNoErr (pfctl "-a raft-anchor") ""

  liftIO $ threadDelay (1000 * ms)
