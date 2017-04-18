{-# LANGUAGE OverloadedStrings #-}

module Mains.LocalSpam where

import           Control.Monad.Reader (runReaderT)
import           Control.RateLimit    (RateLimit)
import           Data.Time.Units      (Millisecond)
import           Turtle

import           Cluster              (mkLocalEnv)
import           Cluster.Types
import           Cluster.Client       (loadLocalNode, spamGeth,
                                       perSecond)
import           Cluster.SpamArgs

data LocalSpamConfig = LocalSpamConfig
  { gethId :: GethId
  , rateLimit :: RateLimit Millisecond
  , contractAddr :: Maybe Text
  , privateFor :: Maybe Text
  }

cliParser :: Parser LocalSpamConfig
cliParser = LocalSpamConfig
  <$> gethIdP
  <*> rateLimitP
  <*> optional contractP
  <*> optional privateForP
  where
    gethIdP = GethId <$>
      optInt     "geth" 'g' "The Geth ID of the local node to spam"
    rateLimitP = perSecond <$>
      optInteger "rps"  'r' "The number of requests per second"

localSpam :: LocalSpamConfig -> IO ()
localSpam (LocalSpamConfig gid rateLimit' contractM privateForM) = do
    let benchTx = processContractArgs contractM privateForM
    -- We don't need the exact cluster size here; just something higher than the
    -- geth ID we want to spam.
        maxClusterSize = 10

    geth <- runReaderT (loadLocalNode gid) (mkLocalEnv maxClusterSize)
    spamGeth benchTx geth rateLimit'

localSpamMain :: IO ()
localSpamMain = localSpam =<< options "Local geth spammer" cliParser
