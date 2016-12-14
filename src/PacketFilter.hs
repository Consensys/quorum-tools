{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns          #-}

module PacketFilter (acquirePf, flushPf, partition) where

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

-- TODO(joel): this should really use anchors for easy cleanup
acquirePfHelper :: IO PfToken
acquirePfHelper =
  let tokenPat :: Pattern PfToken
      tokenPat = PfToken . pack <$> ("Token : " *> some digit)

      forceFirst :: First PfToken -> PfToken
      forceFirst =
        fromMaybe (error "failed to find pf token (check you're a sudoer)")
        . getFirst

      step :: First PfToken -> Either Line Line -> First PfToken
      step acc line = acc <> First (matchOnce tokenPat
                                              (lineToText $ either id id line))

      findToken :: Fold (Either Line Line) PfToken
      findToken = Fold step mempty forceFirst

  in fold (inshellWithErr (pfctl "-E") "") findToken

flushPf :: MonadManaged m => m ()
flushPf = sh $ inshellWithNoErr (pfctl "-F rules") ""

-- | Turn on pf and initialize with empty rules for every geth node.
--
-- Must be called once at the beginning of a test.
acquirePf :: MonadManaged m => m ()
acquirePf = do
  -- enable pf / increment its enable reference count, then release on exit
  -- Note: this could fail if we're not sudoers -- bind strictly to trigger
  -- failure
  !_ <- using $ managed $ bracket
    acquirePfHelper
    (\(PfToken tk) -> sh $ inshellWithNoErr (pfctl ("-X "%s) tk) "")

  -- flush the rules we added on exit
  flushPf

-- | Make a packet filter rule to block a specific port.
blockPortsRule :: [Port] -> Text
blockPortsRule ports =
  let ports_ = T.intercalate ", " (map (T.pack . show . getPort) ports)
  in format ("block quick proto { tcp, udp } from any to port { "%s%" }") ports_

-- | Partition some geth node for a number of milliseconds.
--
-- TODO: This will currently only work for partitioning a single node.
partition :: (MonadManaged m, HasEnv m) => Millis -> GethId -> m ()
partition (Millis ms) geth = do
  gdata <- reader clusterDataRoot
  ports <- getPortsForGeth gdata geth
  _ <- sh $ inshellWithNoErr
    (pfctl "-f -")
    (select $ textToLines $ blockPortsRule ports)

  -- make sure to reset pf.conf on exit
  !_ <- using $ managed $ onExit $ sh $
    inshellWithNoErr (pfctl "") ""

  liftIO $ threadDelay (1000 * ms)
