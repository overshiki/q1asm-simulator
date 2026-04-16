module Q1Asm.Repl.State
  ( ReplState(..)
  , SchedulerSnapshot(..)
  , mkReplState
  , snapshot
  , restore
  ) where

import Data.Vector (Vector)
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Q1Asm.Q1Core (initQ1State)
import Q1Asm.RtCore (initRtEnv, rtInitialState)
import Q1Asm.Types
import Q1Asm.Repl.Queue

data ReplState = ReplState
  { rsQ1 :: !Q1CoreState
  , rsRt :: !RtCoreState
  , rsQueue :: !InspectableQueue
  , rsEnv :: !RtEnvStatic
  , rsHistory :: ![SchedulerSnapshot]
  , rsStepCount :: !Int
  }
  deriving (Show)

data SchedulerSnapshot = SchedulerSnapshot
  { snapQ1 :: !Q1CoreState
  , snapRt :: !RtCoreState
  , snapQueue :: !InspectableQueue
  , snapStepCount :: !Int
  }
  deriving (Show)

mkReplState :: [Instruction] -> Vector (Vector Float) -> Vector (Vector Float) -> Map.Map Word8 Bool -> ReplState
mkReplState prog wfs wts trigs = ReplState
  { rsQ1 = initQ1State prog
  , rsRt = rtInitialState
  , rsQueue = emptyIQ 32
  , rsEnv = initRtEnv wfs wts trigs
  , rsHistory = []
  , rsStepCount = 0
  }

snapshot :: ReplState -> SchedulerSnapshot
snapshot st = SchedulerSnapshot (rsQ1 st) (rsRt st) (rsQueue st) (rsStepCount st)

restore :: SchedulerSnapshot -> ReplState -> ReplState
restore snap st = st
  { rsQ1 = snapQ1 snap
  , rsRt = snapRt snap
  , rsQueue = snapQueue snap
  , rsStepCount = snapStepCount snap
  }
