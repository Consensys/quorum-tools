-- | Quorum-tools toplevel.
module QuorumTools where

import Control.Concurrent

import Mains.AwsBootstrap (AwsConfig(..))
import qualified Mains.AwsBootstrap as AwsBootstrap

import Mains.AwsSpam (SpamConfig(..))
import qualified Mains.AwsSpam as AwsSpam

import Mains.LocalNew (localNewMain)

import Mains.LocalSpam (LocalSpamConfig(..))
import qualified Mains.LocalSpam as LocalSpam

import Mains.LocalStart (localStartMain)

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
