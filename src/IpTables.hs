{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns          #-}

module IpTables where

import           Control.Concurrent         (threadDelay)
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad.Reader.Class (MonadReader (reader))
import qualified Data.Text                  as T
import           Prelude                    hiding (FilePath, lines)
import           Turtle

import Cluster
import SharedPartitioning

iptables :: Format Text r -> r
iptables args = format ("sudo -n iptables "%args)

partition :: (MonadManaged m, HasEnv m) => Millis -> GethId -> m ()
partition (Millis ms) geth = do
  gdata <- reader clusterDataRoot
  ports <- getPortsForGeth gdata geth
  let n = gId geth

  _ <- sh $ do
    port <- select ports
    let portStr = T.pack $ show $ getPort port
    inshell (iptables ("-A geth"%d%" -s "%s%" -j DROP") n portStr) ""

  !_ <- using $ managed $ onExit $ sh $ do
    void $ inshell (iptables ("-F "%d) n) ""
    inshell (iptables ("-X "%d) n) ""

  liftIO $ threadDelay (1000 * ms)
