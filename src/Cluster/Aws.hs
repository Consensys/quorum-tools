{-# LANGUAGE OverloadedStrings #-}

module Cluster.Aws
  ( awsIp
  ) where

import Turtle

import Checkpoint
import Cluster

--
-- TODO: use newtypes
--
awsIp :: Int -> Int -> GethId -> Ip
awsIp clusterSize subnets (GethId gid) =
    Ip $ format ("10.0."%d%"."%d) subnet lastOctet
  where
    idx = gid - 1 -- Zero-indexed geth id
    subnet    = 1 + (idx `mod` clusterSize)
    lastOctet = 101 + (idx `div` subnets)
