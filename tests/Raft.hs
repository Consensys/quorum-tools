module Main where

import Data.Monoid ((<>))

import QuorumTools.Test.Raft.CycleTest
import QuorumTools.Test.Raft.LeaderPartitionTest
import QuorumTools.Test.Raft.LeaveJoinTest
import QuorumTools.Test.Raft.NewcomerRejoinTest
import QuorumTools.Test.Raft.PrivateStateTest
import QuorumTools.Test.Raft.PublicStateTest
import QuorumTools.Test.Raft.RestartNodeTest

run :: String -> IO () -> IO ()
run description action = do
  putStrLn $ "\n" <> description <> " test"
  action

main :: IO ()
main = do
  run "cycle"                       cycleTestMain
  run "leader partition"            leaderPartitionTestMain
  run "initial member leave/rejoin" leaveJoinTestMain
  run "newcomer leave/rejoin"       newcomerRejoinTestMain
  run "private state"               privateStateTestMain
  run "public state"                publicStateTestMain
  run "restart node"                restartNodeTestMain
