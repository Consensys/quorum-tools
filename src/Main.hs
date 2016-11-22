module Main where

import           Control.Monad.Reader (runReaderT)
import           Turtle

import           Cluster (defaultClusterEnv, setupNodes, runNodesIndefinitely)

main :: IO ()
main = sh $ flip runReaderT defaultClusterEnv $ do
  geths <- setupNodes [1, 2, 3]
  runNodesIndefinitely geths
