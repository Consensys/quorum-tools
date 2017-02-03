{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader (runReaderT)
import           Turtle

import           Cluster              (mkLocalEnv, runNodesIndefinitely,
                                       wipeAndSetupNodes, clusterGids)

--
-- TODO: rename this local-cluster
--

main :: IO ()
main = sh $ flip runReaderT (mkLocalEnv size) $ do
    geths <- wipeAndSetupNodes "gdata" (clusterGids size)
    runNodesIndefinitely geths

  where
    size = 3
