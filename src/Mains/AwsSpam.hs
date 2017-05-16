{-# LANGUAGE OverloadedStrings #-}

module Mains.AwsSpam where

import           Control.Monad.Reader (runReaderT)
import           Control.RateLimit    (RateLimit)
import           Data.Bool            (bool)
import qualified Data.Map.Strict      as Map
import           Data.Time.Units      (Millisecond)
import           Turtle

import           Cluster
import           Cluster.Aws          (dockerHostIp, internalAwsIp)
import           Cluster.Client       (loadNode, perSecond, spamGeth)
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

mkSingletonEnv :: MonadIO m => AwsClusterType -> GethId -> m ClusterEnv
mkSingletonEnv cType gid = do
    key <- readAccountKey dataDir gid
    return $ mkClusterEnv mkIp (const dataDir) (Map.singleton gid key)

  where
    --
    -- TODO: fix/improve this
    --
    maxClusterSize = 10
    numSubnets     = 3
    dataDir = DataDir "/datadir"

    mkIp = case cType of
      SingleRegion -> internalAwsIp maxClusterSize numSubnets
      MultiRegion  -> const dockerHostIp

readGidFromHomedir :: IO GethId
readGidFromHomedir = GethId . read <$> readFile "/home/ubuntu/node-id"

awsSpamMain :: IO ()
awsSpamMain = awsSpam =<< parseConfig
  where
    parseConfig = options "Spams the local node with public transactions"
                          cliParser

awsSpam :: SpamConfig -> IO ()
awsSpam config = do
  gid <- readGidFromHomedir
  let benchTx = processContractArgs (contract config) (privateFor config)
  cEnv <- mkSingletonEnv (clusterType config) gid
  geth <- runReaderT (loadNode gid) cEnv
  spamGeth benchTx geth (rateLimit config)
