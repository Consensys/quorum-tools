-- | Quorum-tools toplevel.
module QuorumTools where

import Control.Concurrent

import QuorumTools.Mains.AwsBootstrap (AwsConfig(..))
import qualified QuorumTools.Mains.AwsBootstrap as AwsBootstrap

import QuorumTools.Mains.AwsSpam (SpamConfig(..))
import qualified QuorumTools.Mains.AwsSpam as AwsSpam

import QuorumTools.Mains.LocalNew (localNewMain)

import QuorumTools.Mains.LocalSpam (LocalSpamConfig(..))
import qualified QuorumTools.Mains.LocalSpam as LocalSpam

import QuorumTools.Mains.LocalStart (localStartMain)

awsBootstrap :: AwsConfig -> IO ThreadId
awsBootstrap = forkIO . AwsBootstrap.awsBootstrap

awsSpam :: SpamConfig -> IO ThreadId
awsSpam = forkIO . AwsSpam.awsSpam

localNew :: IO ThreadId
localNew = forkIO localNewMain

localSpam :: LocalSpamConfig -> IO ThreadId
localSpam = forkIO . LocalSpam.localSpam

localStart :: IO ThreadId
localStart = forkIO localStartMain
