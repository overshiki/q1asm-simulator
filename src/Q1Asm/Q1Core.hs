{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Q1Asm.Q1Core
  ( Q1M
  , runQ1M
  , initQ1State
  , q1Loop
  , q1Cost
  , rtQ1Cost
  , runQ1StepPure
  , peekQ1Instr
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Bits ((.&.), (.|.), xor, complement, shiftL, shiftR)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)
import Q1Asm.Types
import Q1Asm.Queue

newtype Q1M a = Q1M (StateT Q1CoreState (ExceptT SimError IO) a)
  deriving (Functor, Applicative, Monad, MonadState Q1CoreState, MonadError SimError, MonadIO)

runQ1M :: Q1CoreState -> Q1M a -> IO (Either SimError a)
runQ1M st (Q1M m) = runExceptT (evalStateT m st)

initQ1State :: [Instruction] -> Q1CoreState
initQ1State prog = Q1CoreState
  { q1Pc = 0
  , q1Registers = VU.replicate 64 0
  , q1Running = True
  , q1CycleTime = 0
  , q1Program = prog
  }

getReg :: RegIdx -> Q1M Word32
getReg r = do
  regs <- gets q1Registers
  return (regs VU.! fromIntegral r)

modifyReg :: RegIdx -> Word32 -> Q1M ()
modifyReg r v = modify $ \s ->
  let regs = q1Registers s
  in s { q1Registers = regs VU.// [(fromIntegral r, v)] }

evalOperand :: Operand -> Q1M Word32
evalOperand (Imm v) = return v
evalOperand (Reg r) = getReg r

advancePc :: Word64 -> Q1M ()
advancePc cost = modify $ \s -> s
  { q1Pc = q1Pc s + 1
  , q1CycleTime = q1CycleTime s + cost
  }

q1Cost :: Q1Instr -> Word64
q1Cost (Loop _ _) = 8
q1Cost (Jr _)     = 8
q1Cost (Jmp _)    = 16
q1Cost (Jge _ _ _) = 12  -- continue path; jump path is 24 in hardware but we model single cost
q1Cost (Jlt _ _ _) = 12
q1Cost _          = 4

isReg :: Operand -> Bool
isReg (Reg _) = True
isReg _       = False

rtQ1Cost :: RtInstr -> Word64
rtQ1Cost (Play op0 op1 _)       = if any isReg [op0, op1] then 8 else 4
rtQ1Cost (Acquire _ op _)       = if isReg op then 4 else 4
rtQ1Cost (AcquireWeighed _ _ _ op0 op1) =
  if any isReg [op0, op1] then 12 else 4
rtQ1Cost (AcquireTtl _ _ op _)  = if isReg op then 4 else 4
rtQ1Cost (SetFreq op path)      = if any isReg [op, path] then 8 else 4
rtQ1Cost (SetPhase op path)     = if any isReg [op, path] then 8 else 4
rtQ1Cost (SetAwgOffs op0 op1)   = if any isReg [op0, op1] then 8 else 4
rtQ1Cost (SetAwgGain op0 op1)   = if any isReg [op0, op1] then 8 else 4
rtQ1Cost (SetMrk op)            = if isReg op then 8 else 4
rtQ1Cost (UpdParam _)           = 4
rtQ1Cost (SetCond en _ _ _)     = if isReg en then 8 else 4
rtQ1Cost (LatchRst _)           = 4
rtQ1Cost (SetLatchEn op _)      = if isReg op then 4 else 4
rtQ1Cost (SetScopeEn op)        = if isReg op then 8 else 4
rtQ1Cost (SetDigital op)        = if isReg op then 8 else 4
rtQ1Cost SetTimeRef             = 4
rtQ1Cost _                      = 4

q1Loop :: RtQueue q => q -> Q1M ()
q1Loop q = do
  st <- get
  when (q1Running st) $ do
    stepQ1 q
    st' <- get
    liftIO $ qSetQ1Time q (q1CycleTime st')
    q1Loop q

stepQ1 :: RtQueue q => q -> Q1M ()
stepQ1 q = do
  st <- get
  let pc = q1Pc st
      prog = q1Program st
  if pc < 0 || pc >= length prog
    then throwError InvalidProgramCounter
    else case prog !! pc of
      Q1Only qi -> execQ1 qi
      Rt ri     -> do
        let ri' = resolveRtInstr st ri
        liftIO $ qPush q ri'
        advancePc (rtQ1Cost ri)

execQ1 :: Q1Instr -> Q1M ()
execQ1 (Move op rd) = do
  v <- evalOperand op
  modifyReg rd v
  advancePc (q1Cost (Move op rd))
execQ1 (Add ra op rd) = do
  a <- getReg ra
  b <- evalOperand op
  modifyReg rd (a + b)
  advancePc (q1Cost (Add ra op rd))
execQ1 (Sub ra op rd) = do
  a <- getReg ra
  b <- evalOperand op
  modifyReg rd (a - b)
  advancePc (q1Cost (Sub ra op rd))
execQ1 (And ra op rd) = do
  a <- getReg ra
  b <- evalOperand op
  modifyReg rd (a .&. b)
  advancePc (q1Cost (And ra op rd))
execQ1 (Or ra op rd) = do
  a <- getReg ra
  b <- evalOperand op
  modifyReg rd (a .|. b)
  advancePc (q1Cost (Or ra op rd))
execQ1 (Xor ra op rd) = do
  a <- getReg ra
  b <- evalOperand op
  modifyReg rd (xor a b)
  advancePc (q1Cost (Xor ra op rd))
execQ1 (Not ra rd) = do
  a <- getReg ra
  modifyReg rd (complement a)
  advancePc (q1Cost (Not ra rd))
execQ1 (Asl ra op rd) = do
  a <- getReg ra
  b <- evalOperand op
  modifyReg rd (shiftL a (fromIntegral b))
  advancePc (q1Cost (Asl ra op rd))
execQ1 (Asr ra op rd) = do
  a <- getReg ra
  b <- evalOperand op
  modifyReg rd (shiftR a (fromIntegral b))
  advancePc (q1Cost (Asr ra op rd))
execQ1 (Jmp addr) = do
  modify $ \s -> s { q1Pc = fromIntegral addr, q1CycleTime = q1CycleTime s + q1Cost (Jmp addr) }
execQ1 (Jge ra op addr) = do
  a <- getReg ra
  b <- evalOperand op
  if a >= b
    then modify $ \s -> s { q1Pc = fromIntegral addr, q1CycleTime = q1CycleTime s + q1Cost (Jge ra op addr) }
    else advancePc (q1Cost (Jge ra op addr))
execQ1 (Jlt ra op addr) = do
  a <- getReg ra
  b <- evalOperand op
  if a < b
    then modify $ \s -> s { q1Pc = fromIntegral addr, q1CycleTime = q1CycleTime s + q1Cost (Jlt ra op addr) }
    else advancePc (q1Cost (Jlt ra op addr))
execQ1 (Loop rc addr) = do
  c <- getReg rc
  let c' = if c == 0 then 0 else c - 1
  modifyReg rc c'
  if c' /= 0
    then modify $ \s -> s { q1Pc = fromIntegral addr, q1CycleTime = q1CycleTime s + 8 }
    else advancePc 8
execQ1 (Jr offset) = do
  modify $ \s -> s { q1Pc = q1Pc s + 1 + fromIntegral offset, q1CycleTime = q1CycleTime s + 8 }
execQ1 Nop = advancePc 4
execQ1 Stop = modify $ \s -> s { q1Running = False, q1CycleTime = q1CycleTime s + 4 }

--------------------------------------------------------------------------------
-- * Pure REPL stepping functions
--------------------------------------------------------------------------------

peekQ1Instr :: Q1CoreState -> Maybe Instruction
peekQ1Instr st =
  let pc = q1Pc st
      prog = q1Program st
  in if pc >= 0 && pc < length prog then Just (prog !! pc) else Nothing

runQ1StepPure :: Q1CoreState -> Either SimError (Q1CoreState, Maybe RtInstr)
runQ1StepPure st =
  let pc = q1Pc st
      prog = q1Program st
  in if pc < 0 || pc >= length prog
     then Left InvalidProgramCounter
     else case prog !! pc of
       Q1Only qi -> Right (runQ1InstrPure qi st, Nothing)
       Rt ri     -> let ri' = resolveRtInstr st ri in Right (advanceQ1Pure (rtQ1Cost ri) st, Just ri')

runQ1InstrPure :: Q1Instr -> Q1CoreState -> Q1CoreState
runQ1InstrPure qi st = case qi of
  Move op rd ->
    let v = evalOperandPure op st
    in advanceQ1Pure (q1Cost qi) $ st { q1Registers = (q1Registers st) VU.// [(fromIntegral rd, v)] }
  Add ra op rd ->
    let a = q1Registers st VU.! fromIntegral ra
        b = evalOperandPure op st
    in advanceQ1Pure (q1Cost qi) $ st { q1Registers = (q1Registers st) VU.// [(fromIntegral rd, a + b)] }
  Sub ra op rd ->
    let a = q1Registers st VU.! fromIntegral ra
        b = evalOperandPure op st
    in advanceQ1Pure (q1Cost qi) $ st { q1Registers = (q1Registers st) VU.// [(fromIntegral rd, a - b)] }
  And ra op rd ->
    let a = q1Registers st VU.! fromIntegral ra
        b = evalOperandPure op st
    in advanceQ1Pure (q1Cost qi) $ st { q1Registers = (q1Registers st) VU.// [(fromIntegral rd, a .&. b)] }
  Or ra op rd ->
    let a = q1Registers st VU.! fromIntegral ra
        b = evalOperandPure op st
    in advanceQ1Pure (q1Cost qi) $ st { q1Registers = (q1Registers st) VU.// [(fromIntegral rd, a .|. b)] }
  Xor ra op rd ->
    let a = q1Registers st VU.! fromIntegral ra
        b = evalOperandPure op st
    in advanceQ1Pure (q1Cost qi) $ st { q1Registers = (q1Registers st) VU.// [(fromIntegral rd, xor a b)] }
  Not ra rd ->
    let a = q1Registers st VU.! fromIntegral ra
    in advanceQ1Pure (q1Cost qi) $ st { q1Registers = (q1Registers st) VU.// [(fromIntegral rd, complement a)] }
  Asl ra op rd ->
    let a = q1Registers st VU.! fromIntegral ra
        b = evalOperandPure op st
    in advanceQ1Pure (q1Cost qi) $ st { q1Registers = (q1Registers st) VU.// [(fromIntegral rd, shiftL a (fromIntegral b))] }
  Asr ra op rd ->
    let a = q1Registers st VU.! fromIntegral ra
        b = evalOperandPure op st
    in advanceQ1Pure (q1Cost qi) $ st { q1Registers = (q1Registers st) VU.// [(fromIntegral rd, shiftR a (fromIntegral b))] }
  Jmp addr ->
    st { q1Pc = fromIntegral addr, q1CycleTime = q1CycleTime st + q1Cost qi }
  Jge ra op addr ->
    let a = q1Registers st VU.! fromIntegral ra
        b = evalOperandPure op st
    in if a >= b
       then st { q1Pc = fromIntegral addr, q1CycleTime = q1CycleTime st + q1Cost qi }
       else advanceQ1Pure (q1Cost qi) st
  Jlt ra op addr ->
    let a = q1Registers st VU.! fromIntegral ra
        b = evalOperandPure op st
    in if a < b
       then st { q1Pc = fromIntegral addr, q1CycleTime = q1CycleTime st + q1Cost qi }
       else advanceQ1Pure (q1Cost qi) st
  Loop rc addr ->
    let c = q1Registers st VU.! fromIntegral rc
        c' = if c == 0 then 0 else c - 1
        st' = st { q1Registers = (q1Registers st) VU.// [(fromIntegral rc, c')] }
    in if c' /= 0
       then st' { q1Pc = fromIntegral addr, q1CycleTime = q1CycleTime st' + q1Cost qi }
       else advanceQ1Pure (q1Cost qi) st'
  Jr offset ->
    st { q1Pc = q1Pc st + 1 + fromIntegral offset, q1CycleTime = q1CycleTime st + q1Cost qi }
  Nop -> advanceQ1Pure (q1Cost qi) st
  Stop -> st { q1Running = False, q1CycleTime = q1CycleTime st + q1Cost qi }

evalOperandPure :: Operand -> Q1CoreState -> Word32
evalOperandPure (Imm v) _ = v
evalOperandPure (Reg r) st = q1Registers st VU.! fromIntegral r

advanceQ1Pure :: Word64 -> Q1CoreState -> Q1CoreState
advanceQ1Pure cost st = st { q1Pc = q1Pc st + 1, q1CycleTime = q1CycleTime st + cost }

resolveOperand :: Q1CoreState -> Operand -> Operand
resolveOperand _ (Imm v) = Imm v
resolveOperand st (Reg r) = Imm (q1Registers st VU.! fromIntegral r)

resolveRtInstr :: Q1CoreState -> RtInstr -> RtInstr
resolveRtInstr st ri = case ri of
  Wait d -> Wait d
  WaitSync d -> WaitSync d
  Play w0 w1 d -> Play (resolveOperand st w0) (resolveOperand st w1) d
  Acquire a b d -> Acquire (resolveOperand st a) (resolveOperand st b) d
  AcquireWeighed a b d w0 w1 -> AcquireWeighed (resolveOperand st a) (resolveOperand st b) d (resolveOperand st w0) (resolveOperand st w1)
  AcquireTtl a b e d -> AcquireTtl (resolveOperand st a) (resolveOperand st b) (resolveOperand st e) d
  SetFreq op path -> SetFreq (resolveOperand st op) (resolveOperand st path)
  SetPhase op path -> SetPhase (resolveOperand st op) (resolveOperand st path)
  ResetPhase -> ResetPhase
  SetAwgOffs i q -> SetAwgOffs (resolveOperand st i) (resolveOperand st q)
  SetAwgGain i q -> SetAwgGain (resolveOperand st i) (resolveOperand st q)
  SetMrk op -> SetMrk (resolveOperand st op)
  SetScopeEn op -> SetScopeEn (resolveOperand st op)
  SetTimeRef -> SetTimeRef
  SetDigital op -> SetDigital (resolveOperand st op)
  UpdParam d -> UpdParam d
  SetCond en addr mode d -> SetCond (resolveOperand st en) addr mode d
  LatchRst d -> LatchRst d
  SetLatchEn e d -> SetLatchEn (resolveOperand st e) d
  WaitTrigger addr d -> WaitTrigger addr d
