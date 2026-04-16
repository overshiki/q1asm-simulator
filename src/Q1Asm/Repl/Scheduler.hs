module Q1Asm.Repl.Scheduler
  ( StepEvent(..)
  , StepResult(..)
  , runStep
  , runStepQ1Force
  , runStepRtForce
  ) where

import Q1Asm.Types
import Q1Asm.Q1Core (runQ1StepPure, peekQ1Instr)
import Q1Asm.RtCore (runRtStepPure)
import Q1Asm.Repl.State
import Q1Asm.Repl.Queue

data StepEvent
  = EvQ1Classical !Q1Instr
  | EvQ1Push !RtInstr
  | EvRtOnly !RtInstr
  | EvSimultaneousQ1Classical !Q1Instr !RtInstr
  | EvSimultaneousQ1Push !RtInstr !RtInstr
  | EvHalt
  deriving (Show)

data StepResult = StepResult
  { srEvent :: !StepEvent
  , srOldState :: !ReplState
  , srNewState :: !ReplState
  }
  deriving (Show)

mkResult :: StepEvent -> ReplState -> ReplState -> StepResult
mkResult ev old new = StepResult ev old new

extractQ1Instr :: Q1CoreState -> Q1Instr
extractQ1Instr st =
  let pc = q1Pc st
      prog = q1Program st
  in case prog !! pc of
       Q1Only qi -> qi
       _ -> error "extractQ1Instr: expected Q1-only instruction"

runStep :: ReplState -> Either SimError StepResult
runStep st =
  let q1s = rsQ1 st
      rts = rsRt st
      q = rsQueue st
      tQ1 = q1CycleTime q1s
      tRt = rtTime rts
      q1Run = q1Running q1s
      rtRun = rtRunning rts
      qFull = isFullIQ q
      qEmpty = isEmptyIQ q
      nextQ1IsRt = case peekQ1Instr q1s of
        Just (Rt _) -> True
        _ -> False
  in
    if not q1Run && not rtRun
    then Right (mkResult EvHalt st st)
    else if not q1Run && qEmpty
    then Right (mkResult EvHalt st (st { rsRt = rts { rtRunning = False } }))
    else if rtRun && qEmpty && not q1Run
    then Right (mkResult EvHalt st (st { rsRt = rts { rtRunning = False } }))
    else if q1Run && qFull && nextQ1IsRt
    then runRtOnly st
    else if rtRun && qEmpty && q1Run
    then runQ1Only st
    else if not q1Run && not qEmpty && rtRun
    then runRtOnly st
    else if q1Run && not qEmpty && rtRun
    then case compare tQ1 tRt of
         LT -> runQ1Only st
         EQ -> runBoth st
         GT -> runRtOnly st
    else if q1Run && qEmpty && not rtRun
    then runQ1Only st
    else Right (mkResult EvHalt st st)

runQ1Only :: ReplState -> Either SimError StepResult
runQ1Only st = do
  (q1s', mInstr) <- runQ1StepPure (rsQ1 st)
  case mInstr of
    Nothing -> do
      let qi = extractQ1Instr (rsQ1 st)
          st' = st { rsQ1 = q1s', rsStepCount = rsStepCount st + 1 }
      Right (mkResult (EvQ1Classical qi) st st')
    Just instr -> do
      case pushIQ (rsQueue st) instr of
        Nothing -> Left (ReplError "Q1 stalled: queue full")
        Just q' -> do
          let st' = st { rsQ1 = q1s', rsQueue = q', rsStepCount = rsStepCount st + 1 }
          Right (mkResult (EvQ1Push instr) st st')

runRtOnly :: ReplState -> Either SimError StepResult
runRtOnly st = do
  case popIQ (rsQueue st) of
    Nothing -> Left (RtUnderflow (rtTime (rsRt st)))
    Just (instr, q') -> do
      rts' <- runRtStepPure (rsEnv st) (rsRt st) instr
      let st' = st { rsRt = rts', rsQueue = q', rsStepCount = rsStepCount st + 1 }
      Right (mkResult (EvRtOnly instr) st st')

runBoth :: ReplState -> Either SimError StepResult
runBoth st = do
  (q1s', mPushed) <- runQ1StepPure (rsQ1 st)
  case mPushed of
    Nothing -> do
      let qi = extractQ1Instr (rsQ1 st)
      case popIQ (rsQueue st) of
        Nothing -> Left (RtUnderflow (rtTime (rsRt st)))
        Just (rtInstr, q') -> do
          rts' <- runRtStepPure (rsEnv st) (rsRt st) rtInstr
          let st' = st { rsQ1 = q1s', rsRt = rts', rsQueue = q', rsStepCount = rsStepCount st + 1 }
          Right (mkResult (EvSimultaneousQ1Classical qi rtInstr) st st')
    Just q1Instr -> do
      case pushIQ (rsQueue st) q1Instr of
        Nothing -> Left (ReplError "Q1 stalled: queue full")
        Just qAfterPush -> do
          case popIQ qAfterPush of
            Nothing -> Left (RtUnderflow (rtTime (rsRt st)))
            Just (rtInstr, qFinal) -> do
              rts' <- runRtStepPure (rsEnv st) (rsRt st) rtInstr
              let st' = st { rsQ1 = q1s', rsRt = rts', rsQueue = qFinal, rsStepCount = rsStepCount st + 1 }
              Right (mkResult (EvSimultaneousQ1Push q1Instr rtInstr) st st')

runStepQ1Force :: ReplState -> Either SimError StepResult
runStepQ1Force st =
  if q1Running (rsQ1 st)
  then case peekQ1Instr (rsQ1 st) of
         Just (Q1Only _) -> runQ1Only st
         Just (Rt _) -> if isFullIQ (rsQueue st)
                        then Left (ReplError "Q1 stalled: queue full")
                        else runQ1Only st
         Nothing -> Left InvalidProgramCounter
  else Left (ReplError "Q1 Core is halted")

runStepRtForce :: ReplState -> Either SimError StepResult
runStepRtForce st =
  if rtRunning (rsRt st)
  then if isEmptyIQ (rsQueue st)
       then if q1Running (rsQ1 st)
            then Left (ReplError "RT stalled: queue empty (waiting for Q1)")
            else Right (mkResult EvHalt st (st { rsRt = (rsRt st) { rtRunning = False } }))
       else runRtOnly st
  else Left (ReplError "RT Core is halted")
