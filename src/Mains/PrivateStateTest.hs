{-# LANGUAGE OverloadedStrings #-}

-- Test private state consistency
module Mains.PrivateStateTest where

import           Control.Lens             ((.~))
import           Control.Monad            (forM_)
import           Control.Monad.Managed    (MonadManaged)
import           Control.Monad.Reader     (ReaderT (runReaderT))

import           Cluster
import           Cluster.StateTestsShared
import           Cluster.Types
import           Constellation
import           Control
import           Prelude                  hiding (FilePath)
import           TestOutline              hiding (verify)
import           Turtle                   hiding (match)

privateStateTestMain :: IO ()
privateStateTestMain = sh $ do
  let cEnv = startEnv & clusterPrivacySupport .~ PrivacyEnabled
      gids = [1..3]

  geths <- runReaderT (wipeAndSetupNodes Nothing "gdata" gids) cEnv
  startConstellationNodes geths

  clusterMain geths cEnv

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
