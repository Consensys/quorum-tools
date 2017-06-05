module Main where

import QuorumTools.Test.Raft.CycleTest
import QuorumTools.Test.Raft.LeaderPartitionTest
import QuorumTools.Test.Raft.LeaveJoinTest
import QuorumTools.Test.Raft.NewcomerRejoinTest
import QuorumTools.Test.Raft.PrivateStateTest
import QuorumTools.Test.Raft.PublicStateTest
import QuorumTools.Test.Raft.RestartNodeTest

main :: IO ()
main = do
  cycleTestMain
  leaderPartitionTestMain
  leaveJoinTestMain
  newcomerRejoinTestMain
  privateStateTestMain
  publicStateTestMain
  restartNodeTestMain
