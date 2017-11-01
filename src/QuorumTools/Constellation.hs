{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuorumTools.Constellation where

import           Constellation.Enclave.Key (b64EncodePublicKey,
                                            jsonEncodePrivateKey, newKeyPair)
import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forM_)
import           Control.Monad.Managed     (MonadManaged)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import           Data.Text.Encoding        (decodeUtf8)
import           Prelude                   hiding (FilePath, lines)
import           Turtle                    hiding (f)

import           QuorumTools.Types
import           QuorumTools.Util          (inshellWithJoinedErr, tee)

constellationConfPath :: DataDir -> FilePath
constellationConfPath (DataDir ddPath) = ddPath </> "constellation.toml"

generateKeyPair :: MonadIO m => DataDir -> m ()
generateKeyPair datadir = liftIO $ do
    (pub, priv) <- newKeyPair
    mktree keyDir
    let pubText = decodeUtf8 $ b64EncodePublicKey pub
    writeTextFile pubFile pubText
    privText <- decodeUtf8 . LBS.toStrict <$> jsonEncodePrivateKey passwd priv
    writeTextFile keyFile privText

  where
    passwd  = Nothing
    keyDir  = dataDirPath datadir </> "keys"
    pubFile = keyDir </> "constellation.pub"
    keyFile = keyDir </> "constellation.key"

-- | Writes the constellation config to a deploy datadir, or its datadir
installConfig :: MonadIO io => Maybe DataDir -> ConstellationConfig -> io ()
installConfig mDeployDataDir conf = liftIO $ writeTextFile path contents
  where
    path          = constellationConfPath localDataDir
    localDataDir  = ccDataDir conf
    deployDataDir = fromMaybe localDataDir mDeployDataDir
    contents      = confText deployDataDir conf

setupConstellationNode :: MonadIO io
                       => Maybe DataDir
                       -> ConstellationConfig
                       -> io ()
setupConstellationNode deployDataDir conf = do
    generateKeyPair localDataDir
    installConfig deployDataDir conf

  where
    localDataDir = ccDataDir conf

startConstellationNode :: MonadManaged io => Geth -> io ()
startConstellationNode geth =
    void $ fork $ sh $ inshellWithJoinedErr command "" & tee logPath

  where
    forceConfigPath :: Maybe FilePath -> FilePath
    forceConfigPath = fromMaybe $ error "missing constellation config"

    confPath = forceConfigPath $ gethConstellationConfig geth
    command = format ("constellation-node -v "%fp) confPath
    logPath = fromText $ format ("constellation"%d%".out") (gId $ gethId geth)

startConstellationNodes :: (Foldable f, MonadManaged io) => f Geth -> io ()
startConstellationNodes geths = do
  forM_ geths startConstellationNode
  --
  -- TODO: connect to constellations via http instead of this:
  --
  liftIO $ threadDelay 1000000

-- We parameterize by a DataDir here so that we can handle the case of
-- bootstrapping a cluster (eg for AWS) -- where the datadir is located in a
-- different place on the filesystem.
confText :: DataDir -> ConstellationConfig -> Text
confText (DataDir ddPath) conf =
  let ConstellationConfig {ccUrl, ccPort, ccOtherNodes} = conf

      lf :: Format r r
      lf = "\n"

      quote :: Format a b -> Format a b
      quote f = "\""%f%"\""

      template =
        "url = "%quote s%lf%
        "port = "%d%lf%
        "socketPath = "%quote fp%lf%
        "otherNodeUrls = "%w%lf%
        "publicKeyPath = "%quote fp%lf%
        "privateKeyPath = "%quote fp%lf%
        "storagePath = "%quote fp%lf

  in format template
            ccUrl
            ccPort
            (ddPath </> "constellation.ipc")
            ccOtherNodes
            (ddPath </> "keys" </> "constellation.pub")
            (ddPath </> "keys" </> "constellation.key")
            (ddPath </> "constellation")
