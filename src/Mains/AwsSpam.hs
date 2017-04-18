{-# LANGUAGE OverloadedStrings #-}

module Mains.AwsSpam where

import           Control.Monad.Reader (runReaderT)
import           Control.RateLimit    (RateLimit)
import           Data.Bool            (bool)
import           Data.Time.Units      (Millisecond)
import           Turtle

import           Cluster
import           Cluster.Aws          (dockerHostIp, internalAwsIp)
import           Cluster.Client       (loadLocalNode, perSecond, spamGeth)
import           Cluster.SpamArgs
import           Cluster.Types

data SpamConfig = SpamConfig { rateLimit   :: RateLimit Millisecond
                             , clusterType :: AwsClusterType
                             , contract    :: Maybe Text
                             , privateFor  :: Maybe Text
                             }

cliParser :: Parser SpamConfig
cliParser = SpamConfig
  <$> fmap perSecond (optInteger "rps"  'r' "The number of requests per second")
  <*> fmap (bool SingleRegion MultiRegion)
           (switch  "multi-region" 'g' "Whether the cluster is multi-region")
  <*> optional contractP
  <*> optional privateForP

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
      MultiRegion  -> const dockerHostIp

readGidFromHomedir :: IO GethId
readGidFromHomedir = GethId . read <$> readFile "/home/ubuntu/node-id"

awsSpamMain :: IO ()
awsSpamMain = do
  awsSpam =<< options "Spams the local node with public transactions" cliParser

awsSpam :: SpamConfig -> IO ()
awsSpam config = do
  gid <- readGidFromHomedir
  let benchTx = processContractArgs (contract config) (privateFor config)
  geth <- runReaderT (loadLocalNode gid) (cEnv (clusterType config) gid)
  spamGeth benchTx geth (rateLimit config)
