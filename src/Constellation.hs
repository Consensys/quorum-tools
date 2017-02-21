{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Constellation where

import           Control.Concurrent         (threadDelay)
import           Control.Monad.Managed      (MonadManaged)
import           Prelude                    hiding (FilePath, lines)
import           Data.Text (Text)

import Turtle                               hiding (f)
import Cluster.Types

fShow :: Show a => a -> FilePath
fShow = fromString . show

-- | Copy the contellation's keypair to its datadir
copyKeys :: MonadIO io => ConstellationConfig -> io ()
copyKeys conf = sh $ do
  let predir = "credentials/constellation-keys" </> fShow (gId (ccGethId conf))
      postdir = keydir conf

  file <- ls predir
  mktree postdir
  cp file (postdir </> filename file)

setupConstellationNode :: MonadManaged io => ConstellationConfig -> io FilePath
setupConstellationNode conf = do
  copyKeys conf
  confFile <- mktempfile "/tmp" "constellation"
  liftIO $ writeTextFile confFile (confText conf)
  return confFile

startConstellationNode :: MonadManaged io => FilePath -> io ()
startConstellationNode confPath = do
  _ <- fork $ sh $
    inshellWithErr (format ("constellation-node "%fp) confPath) ""

  -- put in a small delay so this constellation can start its server before the
  -- next hits it
  liftIO $ threadDelay 50000

keydir :: ConstellationConfig -> FilePath
keydir ConstellationConfig {ccDatadir = DataDir dir} = dir </> "keys"

confText :: ConstellationConfig -> Text
confText conf =
  let kdir = keydir conf
      ConstellationConfig
        {ccUrl, ccDatadir = DataDir dir, ccGethId, ccOtherNodes} = conf

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

  in format contents ccUrl (9000 + gId ccGethId)
       (dir </> "constellation.ipc")
       ccOtherNodes
       (kdir </> "constellation.pub")
       (kdir </> "constellation.key")
       (dir </> "constellation")
