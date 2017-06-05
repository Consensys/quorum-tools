{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module QuorumTools.Constellation where

import           Control.Concurrent    (threadDelay)
import           Control.Monad         (forM_)
import           Control.Monad.Managed (MonadManaged)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Prelude               hiding (FilePath, lines)
import           Turtle                hiding (f)

import           QuorumTools.Types
import           QuorumTools.Util      (tee, inshellWithJoinedErr)

fShow :: Show a => a -> FilePath
fShow = fromString . show

constellationConfPath :: DataDir -> FilePath
constellationConfPath (DataDir ddPath) = ddPath </> "constellation.toml"

--
-- TODO: we can now change all of these to take Geth values. should simplify
--

-- | Copy the contellation's keypair to its datadir
copyKeys :: MonadIO io => ConstellationConfig -> io ()
copyKeys conf = sh $ do
  --
  -- TODO: remove this hard-coded path.
  --
  let predir = "credentials/constellation-keys" </> fShow (gId (ccGethId conf))
      postdir = dataDirPath (ccDatadir conf) </> "keys"

  file <- ls predir
  mktree postdir
  cp file (postdir </> filename file)

-- | Writes the constellation config to its datadir
installConfig :: MonadIO io => Maybe DataDir -> ConstellationConfig -> io ()
installConfig mDeployDatadir conf = do
  let localDataDir = ccDatadir conf
      confPath = constellationConfPath localDataDir
      deployDatadir = fromMaybe localDataDir mDeployDatadir
  liftIO $ writeTextFile confPath (confText deployDatadir conf)

setupConstellationNode :: MonadIO io => Maybe DataDir -> ConstellationConfig -> io ()
setupConstellationNode deployDatadir conf = do
  copyKeys conf
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
-- bootstrapping a cluster for AWS -- where the datadir is located in a
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
