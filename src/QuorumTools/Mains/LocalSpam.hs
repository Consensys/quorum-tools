{-# LANGUAGE OverloadedStrings #-}

module QuorumTools.Mains.LocalSpam where

import           Control.Monad.Reader (runReaderT)
import           Control.RateLimit    (RateLimit)
import qualified Data.Map.Strict      as Map
import           Data.Time.Units      (Microsecond)
import           Turtle

import           QuorumTools.Client   (loadNode, perSecond, spamGeth)
import           QuorumTools.Cluster  (mkLocalEnv, nodeName,
                                       readAccountKey)
import qualified QuorumTools.Metrics  as Metrics
import           QuorumTools.Spam
import           QuorumTools.Types

data LocalSpamConfig = LocalSpamConfig
  { gethId       :: GethId
  , rateLimit    :: RateLimit Microsecond
  , contractAddr :: Maybe Text
  , privateFor   :: Maybe Text
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
    keys <- Map.singleton gid <$> readAccountKey dataDir gid
    geth <- runReaderT (loadNode gid) (mkLocalEnv keys Raft)
    let store = Metrics.blackhole

    spamGeth store benchTx rateLimit' geth

  where
    dataDir = DataDir $ "gdata" </> fromText (nodeName gid)
    benchTx = processContractArgs contractM privateForM

localSpamMain :: IO ()
localSpamMain = localSpam =<< options "Local geth spammer" cliParser
