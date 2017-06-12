{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuorumTools.Constellation where

import           Constellation.Enclave.Key (newKeyPair, b64EncodePublicKey,
                                            jsonEncodePrivateKey)
import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forM_)
import           Control.Monad.Managed     (MonadManaged)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text)
import           Prelude                   hiding (FilePath, lines)
import           Turtle                    hiding (f)

import           QuorumTools.Types
import           QuorumTools.Util          (tee, inshellWithJoinedErr)

fShow :: Show a => a -> FilePath
fShow = fromString . show

constellationConfPath :: DataDir -> FilePath
constellationConfPath (DataDir ddPath) = ddPath </> "constellation.toml"

--
-- TODO: we can now change all of these to take Geth values. should simplify
--

generateKeyPair :: MonadIO m => DataDir -> m ()
generateKeyPair datadir = liftIO $ do
    (pub, priv) <- newKeyPair
    mktree keyDir
    writeLazyBytes pubFile $ b64EncodePublicKey pub
    writeLazyBytes keyFile =<< jsonEncodePrivateKey passwd priv

  where
    passwd  = Nothing
    keyDir  = dataDirPath datadir </> "keys"
    pubFile = keyDir </> "constellation.pub"
    keyFile = keyDir </> "constellation.key"

    writeLazyBytes :: MonadIO m => FilePath -> LBS.ByteString -> m ()
    writeLazyBytes path contents = sh $ do
      handle <- using $ writeonly path
      liftIO $ LBS.hPut handle contents

-- | Writes the constellation config to its datadir
installConfig :: MonadIO io => Maybe DataDir -> ConstellationConfig -> io ()
installConfig mDeployDatadir conf = do
  let localDataDir = ccDatadir conf
      confPath = constellationConfPath localDataDir
      deployDatadir = fromMaybe localDataDir mDeployDatadir
  liftIO $ writeTextFile confPath (confText deployDatadir conf)

setupConstellationNode :: MonadIO io => Maybe DataDir -> ConstellationConfig -> io ()
setupConstellationNode deployDatadir conf = do
  generateKeyPair (ccDatadir conf)
  installConfig deployDatadir conf

constellationNodeName :: GethId -> Text
constellationNodeName gid = format ("constellation"%d) (gId gid)

startConstellationNode :: MonadManaged io => Geth -> io ()
startConstellationNode geth = do
  let confPath = forceConfigPath $ gethConstellationConfig geth
  let logPath = fromText $ constellationNodeName (gethId geth) <> ".out"

  _ <- fork $ sh $
    inshellWithJoinedErr (format ("constellation-node "%fp) confPath) ""
    & tee logPath

  -- put in a small delay so this constellation can start its server before the
  -- next hits it
  liftIO $ threadDelay 50000

  where
    forceConfigPath :: Maybe FilePath -> FilePath
    forceConfigPath = fromMaybe $ error "missing constellation config"

startConstellationNodes :: (Foldable f, MonadManaged io) => f Geth -> io ()
startConstellationNodes geths = do
  forM_ geths startConstellationNode
  liftIO $ threadDelay 1000000

-- We parameterize by a DataDir here so that we can handle the case of
-- bootstrapping a cluster (eg for AWS) -- where the datadir is located in a
-- different place on the filesystem.
confText :: DataDir -> ConstellationConfig -> Text
confText (DataDir ddPath) conf =
  let ConstellationConfig {ccUrl, ccGethId, ccOtherNodes} = conf

      lf :: Format r r
      lf = "\n"

      quote :: Format a b -> Format a b
      quote f = "\""%f%"\""

      contents =
        "url = "%quote s%lf%
        "port = "%d%lf%
        "socketPath = "%quote fp%lf%
        "otherNodeUrls = "%w%lf%
        "publicKeyPath = "%quote fp%lf%
        "privateKeyPath = "%quote fp%lf%
        "storagePath = "%quote fp%lf

  --
  -- TODO: use base constellation port
  --
  in format contents ccUrl (9000 + gId ccGethId)
       (ddPath </> "constellation.ipc")
       ccOtherNodes
       (ddPath </> "keys" </> "constellation.pub")
       (ddPath </> "keys" </> "constellation.key")
       (ddPath </> "constellation")
