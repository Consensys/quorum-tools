module Main where

import Mains.CycleTest
import Mains.LeaderPartitionTest
import Mains.LeaveJoinTest
import Mains.NewcomerRejoinTest
import Mains.PrivateStateTest
import Mains.PublicStateTest
import Mains.RestartNodeTest

main :: IO ()
main = do
  cycleTestMain
  leaderPartitionTestMain
  leaveJoinTestMain
  newcomerRejoinTestMain
  privateStateTestMain
  publicStateTestMain
  restartNodeTestMain
