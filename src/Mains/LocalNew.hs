{-# LANGUAGE OverloadedStrings #-}

-- | Creates and starts a new cluster, destroying old datadirs in the process.
module Mains.LocalNew where

import           Control.Lens         (view, (.~))
import           Control.Monad.Reader (runReaderT)
import           Turtle               hiding (view)

import           Cluster              (generateClusterKeys, mkLocalEnv,
                                       runNodesIndefinitely, wipeAndSetupNodes)
import           Cluster.Types
import           Constellation

localNewMain :: IO ()
localNewMain = do
    keys <- generateClusterKeys clusterSize password
    let cEnv = mkLocalEnv keys
             & clusterPrivacySupport .~ PrivacyEnabled
             & clusterPassword       .~ password

    sh $ flip runReaderT cEnv $ do
      geths <- wipeAndSetupNodes Nothing "gdata" (clusterGids clusterSize)

      privacySupport <- view clusterPrivacySupport
      when (privacySupport == PrivacyEnabled) (startConstellationNodes geths)

      runNodesIndefinitely geths

  where
    password    = CleartextPassword "abcd"
    clusterSize = 3
