{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader (runReaderT)
import           Turtle

import           Cluster              (defaultClusterEnv, runNodesIndefinitely,
                                       wipeAndSetupNodes)

main :: IO ()
main = sh $ flip runReaderT defaultClusterEnv $ do
  geths <- wipeAndSetupNodes "gdata" [1, 2, 3]
  runNodesIndefinitely geths
