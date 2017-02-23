{-# LANGUAGE OverloadedStrings #-}

-- | Creates and starts a new cluster, destroying old datadirs in the process.
module Mains.LocalNew where

import           Control.Lens         (view, (.~))
import           Control.Monad.Reader (runReaderT)
import           Turtle               hiding (view)

import           Cluster              (mkLocalEnv, runNodesIndefinitely,
                                       wipeAndSetupNodes)
import           Cluster.Types
import           Constellation

localNewMain :: IO ()
localNewMain = sh $ flip runReaderT cEnv $ do
    liftIO $ putStrLn "creating a new empty blockchain with privacy support"
    geths <- wipeAndSetupNodes Nothing "gdata" (clusterGids clusterSize)

    privacySupport <- view clusterPrivacySupport
    when (privacySupport == PrivacyEnabled) $ do
      liftIO $ putStrLn "starting constellation nodes"
      startConstellationNodes geths

    liftIO $ putStrLn "starting geth nodes"
    runNodesIndefinitely geths

  where
    clusterSize = 3
    cEnv = mkLocalEnv clusterSize & clusterPrivacySupport .~ PrivacyEnabled
