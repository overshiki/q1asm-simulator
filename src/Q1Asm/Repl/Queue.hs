module Q1Asm.Repl.Queue
  ( InspectableQueue(..)
  , emptyIQ
  , pushIQ
  , popIQ
  , peekIQ
  , depthIQ
  , isFullIQ
  , isEmptyIQ
  , peekAllIQ
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Q1Asm.Types

data InspectableQueue = InspectableQueue
  { iqBuffer :: !(Seq RtInstr)
  , iqCapacity :: !Int
  }
  deriving (Show)

emptyIQ :: Int -> InspectableQueue
emptyIQ cap = InspectableQueue Seq.empty cap

pushIQ :: InspectableQueue -> RtInstr -> Maybe InspectableQueue
pushIQ q instr =
  if Seq.length (iqBuffer q) >= iqCapacity q
  then Nothing
  else Just q { iqBuffer = iqBuffer q Seq.|> instr }

popIQ :: InspectableQueue -> Maybe (RtInstr, InspectableQueue)
popIQ q = case Seq.viewl (iqBuffer q) of
  Seq.EmptyL -> Nothing
  instr Seq.:< rest -> Just (instr, q { iqBuffer = rest })

peekIQ :: InspectableQueue -> Maybe RtInstr
peekIQ q = case Seq.viewl (iqBuffer q) of
  Seq.EmptyL -> Nothing
  instr Seq.:< _ -> Just instr

depthIQ :: InspectableQueue -> Int
depthIQ q = Seq.length (iqBuffer q)

isFullIQ :: InspectableQueue -> Bool
isFullIQ q = Seq.length (iqBuffer q) >= iqCapacity q

isEmptyIQ :: InspectableQueue -> Bool
isEmptyIQ q = Seq.null (iqBuffer q)

peekAllIQ :: InspectableQueue -> [RtInstr]
peekAllIQ q = toList (iqBuffer q)
