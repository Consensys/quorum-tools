{-# LANGUAGE OverloadedStrings #-}

-- | Creates and starts a new cluster, destroying old datadirs in the process.
module QuorumTools.Mains.LocalNew where

import           Control.Lens              (view, (.~))
import           Control.Monad.Reader      (runReaderT)
import           Data.Maybe                (fromMaybe)
import           Data.Optional             (Optional(Specific))
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import           Turtle                    hiding (view)
import           Turtle.Options            (HelpMessage(..))

import           QuorumTools.Cluster       (generateClusterKeys, mkLocalEnv,
                                            runNode, wipeAndSetupNodes)
import           QuorumTools.Constellation
import           QuorumTools.Control       (awaitAll)
import           QuorumTools.Types

data LocalNewConfig
  = LocalNewConfig { totalPeers   :: Int
                   , initialPeers :: Maybe Int
                   }

defaultClusterSize :: Int
defaultClusterSize = 3

cliParser :: Parser LocalNewConfig
cliParser = LocalNewConfig
    <$> (optInt "nodes" 'n' nodesMessage <|> pure defaultClusterSize)
    <*> optional (optInt "initial" 'i' initialPeersMessage)

  where
    nodesMessage = Specific . HelpMessage $
      "The total number of peers. Default: " <> T.pack (show defaultClusterSize)
    initialPeersMessage =
      "The number of initial peers. Default: the total number of peers."

localNewMain :: IO ()
localNewMain = do
    config <- parseConfig

    let totalSize   = totalPeers config
        initialSize = fromMaybe totalSize (initialPeers config)
        gids        = clusterGids totalSize

    when (totalSize < initialSize) $
      error "initial peers can not be greater than total peers"

    keys <- generateClusterKeys gids password
    let cEnv = mkLocalEnv keys
             & clusterPrivacySupport .~ PrivacyEnabled
             & clusterInitialMembers .~ Set.fromList (take initialSize gids)
             & clusterPassword       .~ password

    sh $ flip runReaderT cEnv $ do
      geths <- wipeAndSetupNodes Nothing "gdata" gids

      privacySupport <- view clusterPrivacySupport
      when (privacySupport == PrivacyEnabled) (startConstellationNodes geths)

      instruments <- traverse (runNode totalSize) geths

      awaitAll $ nodeTerminated <$> instruments

  where
    password    = CleartextPassword "abcd"
    parseConfig = options "Creates a new local cluster" cliParser
