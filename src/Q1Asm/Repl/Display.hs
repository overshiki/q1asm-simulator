module Q1Asm.Repl.Display
  ( displayStep
  , displayInfo
  ) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Q1Asm.Types
import Q1Asm.Repl.State
import Q1Asm.Repl.Scheduler
import Q1Asm.Repl.Queue
import Q1Asm.Repl.Commands (InfoTarget(..))

displayStep :: StepResult -> String
displayStep sr = case srEvent sr of
  EvQ1Classical qi -> displayQ1Classical sr qi
  EvQ1Push ri -> displayQ1Push sr ri
  EvRtOnly ri -> displayRtOnly sr ri
  EvSimultaneousQ1Classical qi rt -> displaySimulClassical sr qi rt
  EvSimultaneousQ1Push q1Rt rt -> displaySimulPush sr q1Rt rt
  EvHalt -> "Simulation halted."

displayQ1Classical :: StepResult -> Q1Instr -> String
displayQ1Classical sr qi =
  let old = srOldState sr
      new = srNewState sr
      tQ1Old = q1CycleTime (rsQ1 old)
      tQ1New = q1CycleTime (rsQ1 new)
      tRt = rtTime (rsRt old)
      busyNote = if tQ1Old < tRt then " | RT busy until " ++ show tRt ++ "ns" else ""
  in unlines $
    [ "────────────────────────────────────────────"
    , "Step " ++ show (rsStepCount new) ++ " | Q1 | t_q1=" ++ show tQ1Old ++ "ns → " ++ show tQ1New ++ "ns" ++ busyNote
    , "       " ++ show qi
    ] ++ filter (not . null) (q1DeltaLines (rsQ1 old) (rsQ1 new)) ++
    [ "────────────────────────────────────────────"
    ]

displayQ1Push :: StepResult -> RtInstr -> String
displayQ1Push sr ri =
  let old = srOldState sr
      new = srNewState sr
      tQ1Old = q1CycleTime (rsQ1 old)
      tQ1New = q1CycleTime (rsQ1 new)
      tRt = rtTime (rsRt old)
      busyNote = if tQ1Old < tRt then " | RT busy until " ++ show tRt ++ "ns" else ""
  in unlines $
    [ "────────────────────────────────────────────"
    , "Step " ++ show (rsStepCount new) ++ " | Q1 | t_q1=" ++ show tQ1Old ++ "ns → " ++ show tQ1New ++ "ns" ++ busyNote
    , "       queue push: " ++ show ri
    , "       queue depth: " ++ show (depthIQ (rsQueue old)) ++ " → " ++ show (depthIQ (rsQueue new))
    , "────────────────────────────────────────────"
    ]

displayRtOnly :: StepResult -> RtInstr -> String
displayRtOnly sr rtInstr =
  let old = srOldState sr
      new = srNewState sr
      tRtOld = rtTime (rsRt old)
      tRtNew = rtTime (rsRt new)
      tQ1 = q1CycleTime (rsQ1 old)
      behindNote = if tRtOld < tQ1 then " | Q1 ahead at " ++ show tQ1 ++ "ns" else ""
  in unlines $
    [ "────────────────────────────────────────────"
    , "Step " ++ show (rsStepCount new) ++ " | RT | t_rt=" ++ show tRtOld ++ "ns → " ++ show tRtNew ++ "ns" ++ behindNote
    , "       " ++ show rtInstr
    ] ++ filter (not . null) (rtDeltaLines (rsRt old) (rsRt new)) ++
    [ "       queue pop | depth: " ++ show (depthIQ (rsQueue old)) ++ " → " ++ show (depthIQ (rsQueue new))
    , "────────────────────────────────────────────"
    ]

displaySimulClassical :: StepResult -> Q1Instr -> RtInstr -> String
displaySimulClassical sr qi rtInstr =
  let old = srOldState sr
      new = srNewState sr
      t = q1CycleTime (rsQ1 old)
  in unlines $
    [ "────────────────────────────────────────────"
    , "Step " ++ show (rsStepCount new) ++ " | SIMUL | t=" ++ show t ++ "ns"
    , "[Q1]   " ++ show qi
    ] ++ filter (not . null) (q1DeltaLines (rsQ1 old) (rsQ1 new)) ++
    [ "[RT]   " ++ show rtInstr
    ] ++ filter (not . null) (rtDeltaLines (rsRt old) (rsRt new)) ++
    [ "       queue net | depth: " ++ show (depthIQ (rsQueue old)) ++ " → " ++ show (depthIQ (rsQueue new))
    , "────────────────────────────────────────────"
    ]

displaySimulPush :: StepResult -> RtInstr -> RtInstr -> String
displaySimulPush sr q1Rt rtInstr =
  let old = srOldState sr
      new = srNewState sr
      t = q1CycleTime (rsQ1 old)
  in unlines $
    [ "────────────────────────────────────────────"
    , "Step " ++ show (rsStepCount new) ++ " | SIMUL | t=" ++ show t ++ "ns"
    , "[Q1]   queue push: " ++ show q1Rt
    , "[RT]   " ++ show rtInstr
    ] ++ filter (not . null) (rtDeltaLines (rsRt old) (rsRt new)) ++
    [ "       queue net | depth: " ++ show (depthIQ (rsQueue old)) ++ " → " ++ show (depthIQ (rsQueue new))
    , "────────────────────────────────────────────"
    ]

q1DeltaLines :: Q1CoreState -> Q1CoreState -> [String]
q1DeltaLines old new =
  [ if q1Pc old /= q1Pc new then "       PC: " ++ show (q1Pc old) ++ " → " ++ show (q1Pc new) else ""
  ] ++ [ "       R" ++ show i ++ ": " ++ show oldVal ++ " → " ++ show newVal
       | i <- [0..63]
       , let oldVal = q1Registers old VU.! i
       , let newVal = q1Registers new VU.! i
       , oldVal /= newVal
       ] ++ [ if q1Running old && not (q1Running new) then "       Q1 Core halted" else "" ]

rtDeltaLines :: RtCoreState -> RtCoreState -> [String]
rtDeltaLines old new = filter (not . null)
  [ if rtTime old /= rtTime new then "       RT time: " ++ show (rtTime old) ++ "ns → " ++ show (rtTime new) ++ "ns" else ""
  , let oldSamples = Seq.length (rtOutputI old)
        newSamples = Seq.length (rtOutputI new)
    in if newSamples > oldSamples then "       Output: +" ++ show (newSamples - oldSamples) ++ " samples (I/Q)" else ""
  , let addedBins = Map.differenceWith diffAcq (rtAcqBins new) (rtAcqBins old)
    in if Map.null addedBins then "" else "       Acquisitions: " ++ show (Map.toList addedBins)
  , let oldAp = rtActiveParams old
        newAp = rtActiveParams new
    in if oldAp /= newAp then "       Active params updated" else ""
  , let oldLat = rtLatched old
        newLat = rtLatched new
    in if newLat /= LatchedState Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing && newLat /= oldLat
       then "       Latched: " ++ show newLat
       else ""
  , if rtRunning old && not (rtRunning new) then "       RT Core halted" else ""
  ]
  where
    diffAcq newList oldList =
      let n = length newList - length oldList
      in if n > 0 then Just (take n (reverse newList)) else Nothing

displayInfo :: InfoTarget -> ReplState -> String
displayInfo target st = case target of
  InfoQ1 -> unlines
    [ "Q1 Core State:"
    , "  PC:        " ++ show (q1Pc (rsQ1 st))
    , "  Running:   " ++ show (q1Running (rsQ1 st))
    , "  CycleTime: " ++ show (q1CycleTime (rsQ1 st)) ++ " ns"
    , "  Registers R0-R7: " ++ show [ (i, q1Registers (rsQ1 st) VU.! i) | i <- [0..7] ]
    ]
  InfoRt -> unlines
    [ "RT Core State:"
    , "  Time:         " ++ show (rtTime (rsRt st)) ++ " ns"
    , "  Running:      " ++ show (rtRunning (rsRt st))
    , "  Active Params:" ++ show (rtActiveParams (rsRt st))
    , "  Latched:      " ++ show (rtLatched (rsRt st))
    , "  NCO:          " ++ show (rtNcoState (rsRt st))
    , "  Output I:     " ++ show (Seq.length (rtOutputI (rsRt st))) ++ " samples"
    , "  Output Q:     " ++ show (Seq.length (rtOutputQ (rsRt st))) ++ " samples"
    ]
  InfoQueue -> unlines $
    [ "Queue depth: " ++ show (depthIQ (rsQueue st)) ++ " / " ++ show (iqCapacity (rsQueue st))
    , "Contents:"
    ] ++ zipWith (\i instr -> "  " ++ show i ++ ": " ++ show instr) [0..] (peekAllIQ (rsQueue st))
  InfoBins -> unlines $
    [ "Acquisition bins:"
    ] ++ [ "  " ++ show k ++ ": " ++ show bins
         | (k, bins) <- Map.toList (rtAcqBins (rsRt st))
         ]
  InfoWaveforms -> unlines $
    [ "Waveforms: " ++ show (length (rtWaveforms (rsEnv st)))
    ] ++ [ "  " ++ show i ++ ": " ++ show (Seq.length (Seq.fromList (toList wf))) ++ " samples"
         | (i, wf) <- zip [0..] (V.toList (rtWaveforms (rsEnv st)))
         ]
