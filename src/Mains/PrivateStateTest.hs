{-# LANGUAGE OverloadedStrings          #-}

-- Test private state consistency
module Mains.PrivateStateTest where

import qualified Data.Map.Strict            as Map
import           Control.Monad.Managed      (MonadManaged)
import           Control.Monad              (forM_)
import           Control.Monad.Reader       (ReaderT (runReaderT))

import Prelude hiding (FilePath)
import Turtle hiding (match)
import Cluster.StateTestsShared
import Cluster.Types
import Cluster
import Constellation
import Control
import TestOutline hiding (verify)

privateStateTestMain :: IO ()
privateStateTestMain = sh $ do
    geths <- runReaderT (wipeAndSetupNodes "gdata" [1..3]) startEnv
    (confs, clusterEnv) <- constellationStartup
    let geths' = map
          (\(geth, conf) -> geth { gethConstellationConfig = Just conf })
          (zip geths confs)

    clusterMain geths' clusterEnv

constellationStartup :: MonadManaged io => io ([FilePath], ClusterEnv)
constellationStartup = do
  let clusterEnv = startEnv
      gethIds = [1, 2, 3]
      firmament = map (mkConstellationConfig clusterEnv gethIds) gethIds

  confFiles <- mapM setupConstellationNode firmament
  mapM_ startConstellationNode confFiles

  td 1 -- delay to allow constellation to set up

  let confMap = Map.fromList (zip [1..] confFiles)
  pure (confFiles, clusterEnv { _clusterConstellationConfs = confMap })

clusterMain :: MonadManaged io => [Geth] -> ClusterEnv -> io ()
clusterMain geths clusterEnv = flip runReaderT clusterEnv $ do
  let [g1, g2, g3] = geths

  instruments <- traverse (runNode clusterSize) geths

  timestampedMessage "awaiting a successful raft election"
  awaitAll (assumedRole <$> instruments)
  timestampedMessage "initial election succeeded"

  key3 <- liftIO $
    readTextFile "credentials/constellation-keys/3/constellation.pub"
  -- geth1 and geth3 are both party to this tx, but geth2 is not
  let privStorage = simpleStorage (PrivateFor [key3])
  privStorageAddr <- createContract (head geths) privStorage

  -- The storage starts with a value of 42 and we increment it five times
  forM_ [1..5] $ \no -> do
    incrementStorage g1 privStorage privStorageAddr

    i1 <- getStorage g1 privStorage privStorageAddr
    expectEq i1 (42 + no)

    i2 <- getStorage g2 privStorage privStorageAddr
    expectEq i2 0
    -- TODO confirm geth2 also gets transaction

    i3 <- getStorage g3 privStorage privStorageAddr
    expectEq i3 (42 + no)

  liftIO $ putStrLn "all successful!"

mkConstellationConfig :: ClusterEnv -> [GethId] -> GethId -> ConstellationConfig
mkConstellationConfig clusterEnv peers thisGeth =
  let gDataDir = pureGidDataDir thisGeth clusterEnv

      constellationUrl :: GethId -> Text
      constellationUrl (GethId intId) =
        format ("http://127.0.0.1:"%d%"/") (9000 + intId)

      -- Everyone connects to all the nodes spun up before them
      priorPeers :: [GethId]
      priorPeers = takeWhile (/= thisGeth) peers

  in ConstellationConfig
       (constellationUrl thisGeth)
       gDataDir
       thisGeth
       (map constellationUrl priorPeers)
