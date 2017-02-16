{-# LANGUAGE OverloadedStrings #-}

module Cluster.Aws
  ( internalAwsIp
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
