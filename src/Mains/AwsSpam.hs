{-# LANGUAGE OverloadedStrings #-}

module Mains.AwsSpam where

import           Control.Monad.Reader (runReaderT)
import           Control.RateLimit    (RateLimit)
import           Data.Bool            (bool)
import           Data.Time.Units      (Millisecond)
import           Turtle

import           Cluster
import           Cluster.Aws          (internalAwsIp)
import           Cluster.Client       (loadLocalNode, perSecond, spamGeth)
import           Cluster.Types

data SpamConfig = SpamConfig { rateLimit   :: RateLimit Millisecond
                             , clusterType :: AwsClusterType
                             }

cliParser :: Parser SpamConfig
cliParser = SpamConfig
  <$> fmap perSecond (optInteger "rps"  'r' "The number of requests per second")
  <*> fmap (bool SingleRegion MultiRegion)
           (switch  "multi-region" 'g' "Whether the cluster is multi-region")

cEnv :: AwsClusterType -> GethId -> ClusterEnv
cEnv cType gid = mkClusterEnv mkIp mkDataDir [gid]
  where
    --
    -- TODO: fix/improve this
    --
    maxClusterSize = 10
    numSubnets     = 3
    mkDataDir = const $ DataDir "/datadir"

    mkIp = case cType of
      SingleRegion -> internalAwsIp maxClusterSize numSubnets
      MultiRegion  -> const $ Ip "127.0.0.1"

readGidFromHomedir :: IO GethId
readGidFromHomedir = GethId . read <$> readFile "/home/ubuntu/node-id"

awsSpamMain :: IO ()
awsSpamMain = do
  config <- options "Spams the local node with public transactions" cliParser
  gid <- readGidFromHomedir
  geth <- runReaderT (loadLocalNode gid) (cEnv (clusterType config) gid)
  spamGeth geth (rateLimit config)
