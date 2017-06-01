{-# LANGUAGE OverloadedStrings #-}

-- | Starts an existing cluster.
module QuorumTools.Mains.LocalStart where

import           Control.Lens              (view, (.~))
import           Control.Monad.Reader      (runReaderT)
import           Data.Map.Strict           (traverseWithKey)
import qualified Data.Map.Strict           as Map
import           Turtle                    hiding (view)

import           QuorumTools.Client        (loadNode)
import           QuorumTools.Cluster       (mkLocalEnv, nodeName,
                                            readAccountKey,
                                            runNodesIndefinitely)
import           QuorumTools.Constellation
import           QuorumTools.Types

localStartMain :: IO ()
localStartMain = do
  keys <- traverseWithKey (flip readAccountKey) dataDirs
  let cEnv = mkLocalEnv keys
           & clusterPrivacySupport .~ PrivacyEnabled
           & clusterPassword       .~ password

  sh $ flip runReaderT cEnv $ do
    geths <- traverse loadNode gids

    privacySupport <- view clusterPrivacySupport
    when (privacySupport == PrivacyEnabled) (startConstellationNodes geths)

    runNodesIndefinitely geths

  where
    password     = CleartextPassword "abcd"
    clusterSize  = 3
    gids         = clusterGids clusterSize
    mkDataDir gid = DataDir $ "gdata" </> fromText (nodeName gid)
    dataDirs      = Map.fromList $ zip gids (mkDataDir <$> gids)
