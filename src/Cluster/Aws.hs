{-# LANGUAGE OverloadedStrings #-}

module Cluster.Aws
  ( internalAwsIp
  , dockerHostIp
  ) where

import Turtle

import Cluster.Types

--
-- TODO: use newtypes
--
internalAwsIp :: Int -> Int -> GethId -> Ip
internalAwsIp clusterSize subnets (GethId gid) =
    Ip $ format ("10.0."%d%"."%d) subnet lastOctet
  where
    idx = gid - 1 -- Zero-indexed geth id
    subnet    = 1 + (idx `mod` clusterSize)
    lastOctet = 101 + (idx `div` subnets)

dockerHostIp :: Ip
dockerHostIp = Ip "172.17.0.1"
