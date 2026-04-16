module Q1Asm.RtCore
  ( initRtEnv
  , rtInitialState
  , rtLoop
  , runRtStepPure
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Word (Word8, Word32, Word64)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Complex
import GHC.Float (castWord32ToFloat)
import Q1Asm.Types
import Q1Asm.Queue
import Q1Asm.Nco
import Q1Asm.Renderer

--------------------------------------------------------------------------------
-- * Environment Helpers
--------------------------------------------------------------------------------

initRtEnv :: Vector (Vector Float) -> Vector (Vector Float) -> Map.Map Word8 Bool -> RtEnvStatic
initRtEnv wfs wts trigs = RtEnvStatic wfs wts trigs

rtInitialState :: RtCoreState
rtInitialState = RtCoreState
  { rtTime = 0
  , rtRunning = True
  , rtLatched = LatchedState Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  , rtActiveParams = ActiveParams 0.0 0.0 0.0 0.0 1.0 1.0
  , rtNcoState = NcoState 0.0 0.0
  , rtAcqBins = Map.empty
  , rtOutputI = mempty
  , rtOutputQ = mempty
  , rtMarker = False
  , rtCondEnabled = False
  , rtCondTrigAddr = 0
  , rtCondOpMode = 0
  }

--------------------------------------------------------------------------------
-- * Execution Loop
--------------------------------------------------------------------------------

rtLoop :: RtQueue q => q -> RtM ()
rtLoop q = do
  st <- get
  when (rtRunning st) $ do
    stepRt q
    rtLoop q

stepRt :: RtQueue q => q -> RtM ()
stepRt q = do
  t <- gets rtTime
  mInstr <- liftIO $ qPop q t
  case mInstr of
    Nothing -> do
      done <- liftIO $ qIsDone q
      if done
        then modify $ \s -> s { rtRunning = False }
        else throwError (RtUnderflow t)
    Just instr -> execRt instr

--------------------------------------------------------------------------------
-- * Instruction Execution
--------------------------------------------------------------------------------

validateDuration :: Word32 -> RtM ()
validateDuration d
  | d < 4 = throwError (InvalidDuration ("duration too short: " ++ show d ++ " ns (minimum 4 ns)"))
  | d > 65535 = throwError (InvalidDuration ("duration too long: " ++ show d ++ " ns (maximum 65535 ns)"))
  | d `mod` 4 /= 0 = throwError (InvalidDuration ("duration not a multiple of 4 ns: " ++ show d))
  | otherwise = return ()

advanceRtTime :: Word32 -> RtM ()
advanceRtTime d = modify $ \s -> s
  { rtTime = rtTime s + fromIntegral d
  , rtNcoState = ncoAdvance (rtNcoState s) d
  }

checkConditional :: RtM Bool
checkConditional = do
  st <- get
  if not (rtCondEnabled st)
    then return True
    else do
      env <- ask
      let addr = rtCondTrigAddr st
          triggerMet = Map.findWithDefault False addr (rtTriggers env)
      return triggerMet

execRtCond :: Word32 -> RtM () -> RtM ()
execRtCond d action = do
  validateDuration d
  ok <- checkConditional
  if ok
    then do action
    else advanceRtTime d

execRt :: RtInstr -> RtM ()
execRt (Wait d) =
  execRtCond d (advanceRtTime d)
execRt (WaitSync d) =
  execRtCond d (advanceRtTime d)
execRt (Play w0 w1 d) =
  execRtCond d $ do
    commitLatched True
    renderPlay w0 w1 d
execRt (Acquire a b d) =
  execRtCond d $ do
    commitLatched True
    recordAcquisition a b d
    advanceRtTime d
execRt (AcquireWeighed a b d w0 w1) =
  execRtCond d $ do
    commitLatched True
    recordAcquisitionWeighed a b d w0 w1
    advanceRtTime d
execRt (AcquireTtl a b _e d) =
  execRtCond d $ do
    commitLatched True
    recordAcquisition a b d
    advanceRtTime d
execRt (SetFreq op _path) =
  execRtCond 4 $ do
    let v = fromIntegral (evalOperandRt op)
    modify $ \s -> s
      { rtLatched = (rtLatched s) { latFreq = Just v }
      }
    advanceRtTime 4
execRt (SetPhase op _path) =
  execRtCond 4 $ do
    let v = fromIntegral (evalOperandRt op)
    modify $ \s -> s
      { rtLatched = (rtLatched s) { latPhase = Just v }
      }
    advanceRtTime 4
execRt ResetPhase =
  execRtCond 4 $ do
    modify $ \s -> s
      { rtNcoState = (rtNcoState s) { ncoPhase = 0.0 }
      }
    advanceRtTime 4
execRt (SetAwgOffs i q) =
  execRtCond 4 $ do
    let vi = castWord32ToFloat (evalOperandRt i)
        vq = castWord32ToFloat (evalOperandRt q)
    modify $ \s -> s
      { rtLatched = (rtLatched s) { latAwgOffsI = Just vi, latAwgOffsQ = Just vq }
      }
    advanceRtTime 4
execRt (SetAwgGain i q) =
  execRtCond 4 $ do
    let vi = fromIntegral (evalOperandRt i) :: Float
        vq = fromIntegral (evalOperandRt q) :: Float
    modify $ \s -> s
      { rtLatched = (rtLatched s) { latAwgGainI = Just vi, latAwgGainQ = Just vq }
      }
    advanceRtTime 4
execRt (SetMrk op) =
  execRtCond 4 $ do
    let v = evalOperandRt op
    modify $ \s -> s
      { rtMarker = v /= 0
      }
    advanceRtTime 4
execRt (UpdParam d) =
  execRtCond d $ do
    st <- get
    let cond = case latCondFlag (rtLatched st) of
          Nothing -> True
          Just c  -> c
    commitLatched cond
    advanceRtTime d
execRt (SetCond enOp addr mode d) =
  execRtCond d $ do
    let en = evalOperandRt enOp
    modify $ \s -> s
      { rtCondEnabled = en /= 0
      , rtCondTrigAddr = addr
      , rtCondOpMode = mode
      }
    advanceRtTime d
execRt (LatchRst d) =
  execRtCond d $ do
    -- Clears any mock trigger latch (no-op in current mock model)
    advanceRtTime d
execRt (SetLatchEn _e d) =
  execRtCond d (advanceRtTime d)
execRt (WaitTrigger _addr d) =
  execRtCond d (advanceRtTime d)
execRt (SetScopeEn op) =
  execRtCond 4 $ do
    let v = evalOperandRt op
    modify $ \s -> s
      { rtLatched = (rtLatched s) { latScopeEn = Just (v /= 0) }
      }
    advanceRtTime 4
execRt SetTimeRef =
  execRtCond 4 (advanceRtTime 4)
execRt (SetDigital op) =
  execRtCond 4 $ do
    let _v = evalOperandRt op
    -- QTM digital output is a no-op in current single-sequencer simulator
    advanceRtTime 4

--------------------------------------------------------------------------------
-- * Helpers
--------------------------------------------------------------------------------

evalOperandRt :: Operand -> Word32
evalOperandRt (Imm v) = v
evalOperandRt (Reg _) = error "RT Core should never see a register operand"

commitLatched :: Bool -> RtM ()
commitLatched conditionMet = do
  lat <- gets rtLatched
  let shouldCommit = case latCondFlag lat of
        Nothing -> True
        Just c  -> c && conditionMet
  when shouldCommit $ do
    st <- get
    let oldAp = rtActiveParams st
        oldNco = rtNcoState st
        newAp = oldAp
          { actFreq     = maybe (actFreq oldAp)     id (latFreq lat)
          , actPhase    = maybe (actPhase oldAp)    id (latPhase lat)
          , actAwgOffsI = maybe (actAwgOffsI oldAp) id (latAwgOffsI lat)
          , actAwgOffsQ = maybe (actAwgOffsQ oldAp) id (latAwgOffsQ lat)
          , actAwgGainI = maybe (actAwgGainI oldAp) id (latAwgGainI lat)
          , actAwgGainQ = maybe (actAwgGainQ oldAp) id (latAwgGainQ lat)
          }
        newNco = oldNco
          { ncoFreq  = maybe (ncoFreq oldNco)  id (latFreq lat)
          , ncoPhase = maybe (ncoPhase oldNco) id (latPhase lat)
          }
    modify $ \s -> s { rtActiveParams = newAp, rtNcoState = newNco }
  modify $ \s -> s { rtLatched = LatchedState Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing }

recordAcquisition :: Operand -> Operand -> Word32 -> RtM ()
recordAcquisition opA opB duration = do
  st <- get
  let acqIdx = fromIntegral (evalOperandRt opA)
      binIdx = fromIntegral (evalOperandRt opB)
      key = (acqIdx, binIdx)
      bin = AcqBin (rtTime st) duration Nothing
      bins = Map.insertWith (++) key [bin] (rtAcqBins st)
  modify $ \s -> s { rtAcqBins = bins }

recordAcquisitionWeighed :: Operand -> Operand -> Word32 -> Operand -> Operand -> RtM ()
recordAcquisitionWeighed opA opB duration opW0 opW1 = do
  env <- ask
  st <- get
  let acqIdx = fromIntegral (evalOperandRt opA)
      binIdx = fromIntegral (evalOperandRt opB)
      w0Idx = fromIntegral (evalOperandRt opW0)
      w1Idx = fromIntegral (evalOperandRt opW1)
      w0 = if w0Idx >= 0 && w0Idx < V.length (rtWeights env) then rtWeights env V.! w0Idx else V.empty
      w1 = if w1Idx >= 0 && w1Idx < V.length (rtWeights env) then rtWeights env V.! w1Idx else V.empty
      d = fromIntegral duration
      iVal = V.sum (V.take d w0)
      qVal = V.sum (V.take d w1)
      key = (acqIdx, binIdx)
      bin = AcqBin (rtTime st) duration (Just (iVal :+ qVal))
      bins = Map.insertWith (++) key [bin] (rtAcqBins st)
  modify $ \s -> s { rtAcqBins = bins }

runRtStepPure :: RtEnvStatic -> RtCoreState -> RtInstr -> Either SimError RtCoreState
-- | Pure wrapper around 'runRtM'. Safe because 'execRt' performs no actual IO.
runRtStepPure env st instr = unsafePerformIO $ runRtM env st $ execRt instr >> get
