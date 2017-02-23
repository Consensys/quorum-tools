{-# LANGUAGE OverloadedStrings #-}

-- | Starts an existing cluster.
module Mains.LocalStart where

import           Control.Lens         (view, (.~))
import           Control.Monad.Reader (runReaderT)
import           Turtle               hiding (view)

import           Cluster              (mkLocalEnv, runNodesIndefinitely)
import           Cluster.Client       (loadLocalNode)
import           Cluster.Types
import           Constellation

localStartMain :: IO ()
localStartMain = sh $ flip runReaderT cEnv $ do
    geths <- traverse loadLocalNode $ clusterGids clusterSize

    privacySupport <- view clusterPrivacySupport
    when (privacySupport == PrivacyEnabled) $ do
      liftIO $ putStrLn "starting constellation nodes"
      startConstellationNodes geths

    liftIO $ putStrLn "starting geth nodes"
    runNodesIndefinitely geths

  where
    clusterSize = 3
    cEnv = mkLocalEnv clusterSize & clusterPrivacySupport .~ PrivacyEnabled
