-- | Quorum-tools toplevel.
module QuorumTools where

import Control.Concurrent

import QuorumTools.Mains.LocalNew (localNewMain)

import QuorumTools.Mains.LocalSpam (LocalSpamConfig(..))
import qualified QuorumTools.Mains.LocalSpam as LocalSpam

import QuorumTools.Mains.LocalStart (localStartMain)

localNew :: IO ThreadId
localNew = forkIO localNewMain

localSpam :: LocalSpamConfig -> IO ThreadId
localSpam = forkIO . LocalSpam.localSpam

localStart :: IO ThreadId
localStart = forkIO localStartMain
