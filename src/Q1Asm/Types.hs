{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Q1Asm.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Complex (Complex)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- * Instruction Set
--------------------------------------------------------------------------------

-- | Register index: R0 .. R63.
type RegIdx = Word8

-- | An operand is either an immediate unsigned 32-bit value or a register reference.
data Operand = Imm Word32 | Reg RegIdx
  deriving (Show, Eq, Generic)

-- | Duration of an RT instruction in nanoseconds. Valid range: [4, 65535].
type Duration = Word32

-- | Q1 Core instructions (classical control, zero wall-time).
data Q1Instr
  = Move  Operand RegIdx          -- ^ move imm/reg, Rd
  | Add   RegIdx Operand RegIdx   -- ^ add Ra, op, Rd
  | Sub   RegIdx Operand RegIdx   -- ^ sub Ra, op, Rd
  | And   RegIdx Operand RegIdx   -- ^ and Ra, op, Rd
  | Or    RegIdx Operand RegIdx   -- ^ or Ra, op, Rd
  | Xor   RegIdx Operand RegIdx   -- ^ xor Ra, op, Rd
  | Not   RegIdx RegIdx           -- ^ not a, dest
  | Asl   RegIdx Operand RegIdx   -- ^ asl a, b, dest
  | Asr   RegIdx Operand RegIdx   -- ^ asr a, b, dest
  | Jmp   Int32                   -- ^ jmp addr (absolute)
  | Jge   RegIdx Operand Int32    -- ^ jge a, b, addr
  | Jlt   RegIdx Operand Int32    -- ^ jlt a, b, addr
  | Loop  RegIdx Int32            -- ^ loop Rc, addr (absolute address)
  | Jr    Int32                   -- ^ jr offset (relative to next instruction)
  | Nop
  | Stop
  deriving (Show, Eq, Generic)

-- | RT Core instructions (deterministic physical control).
data RtInstr
  = Wait            Duration
  | WaitSync        Duration
  | Play            Operand Operand Duration   -- ^ play wave0, wave1, duration
  | Acquire         Operand Operand Duration   -- ^ acquire acq_idx, bin_idx, duration
  | AcquireWeighed  Operand Operand Duration Operand Operand  -- ^ idx, bin, dur, w0, w1
  | AcquireTtl      Operand Operand Operand Duration
  | SetFreq         Operand Operand  -- ^ freq, path
  | SetPhase        Operand Operand            -- ^ phase, path
  | ResetPhase
  | SetAwgOffs      Operand Operand            -- ^ I and Q offsets
  | SetAwgGain      Operand Operand            -- ^ gain_I, gain_Q
  | SetMrk          Operand                    -- ^ marker state
  | UpdParam        Duration                   -- ^ atomic commit with duration
  | SetCond         Operand Word8 Word8 Duration -- ^ en, trig_addr, op_mode, wait_time
  | LatchRst        Duration                   -- ^ wait_time
  | SetLatchEn      Operand Duration
  | WaitTrigger     Word8 Duration             -- ^ wait_trigger addr, duration
  | SetScopeEn      Operand                    -- ^ enable
  | SetTimeRef
  | SetDigital      Operand                    -- ^ value (QTM)
  deriving (Show, Eq, Generic)

-- | A full Q1ASM instruction is either Q1-only or an RT instruction.
data Instruction
  = Q1Only Q1Instr
  | Rt     RtInstr
  deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- * Q1 Core State
--------------------------------------------------------------------------------

data Q1CoreState = Q1CoreState
  { q1Pc        :: !Int              -- ^ Program counter (instruction index)
  , q1Registers :: !(VU.Vector Word32)  -- ^ 64 general-purpose registers
  , q1Running   :: !Bool             -- ^ True until stop/halt/error
  , q1CycleTime :: !Word64           -- ^ Accumulated Q1 core cycles in ns
  , q1Program   :: ![Instruction]    -- ^ The instruction memory
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- * RT Core State
--------------------------------------------------------------------------------

data LatchedState = LatchedState
  { latFreq      :: !(Maybe Double)
  , latPhase     :: !(Maybe Double)
  , latAwgOffsI  :: !(Maybe Float)
  , latAwgOffsQ  :: !(Maybe Float)
  , latAwgGainI  :: !(Maybe Float)
  , latAwgGainQ  :: !(Maybe Float)
  , latScopeEn   :: !(Maybe Bool)
  , latCondFlag  :: !(Maybe Bool)
  }
  deriving (Show, Eq)

data ActiveParams = ActiveParams
  { actFreq     :: !Double
  , actPhase    :: !Double
  , actAwgOffsI :: !Float
  , actAwgOffsQ :: !Float
  , actAwgGainI :: !Float
  , actAwgGainQ :: !Float
  }
  deriving (Show, Eq)

data NcoState = NcoState
  { ncoFreq  :: !Double
  , ncoPhase :: !Double
  }
  deriving (Show)

data AcqBin = AcqBin
  { acqBinStartTime :: !Word64
  , acqBinDuration  :: !Word32
  , acqBinValue     :: !(Maybe (Complex Float))
  }
  deriving (Show)

data RtCoreState = RtCoreState
  { rtTime         :: !Word64
  , rtRunning      :: !Bool
  , rtLatched      :: !LatchedState
  , rtActiveParams :: !ActiveParams
  , rtNcoState     :: !NcoState
  , rtAcqBins      :: !(Map (Int, Int) [AcqBin])
  , rtOutputI      :: !(Seq Float)
  , rtOutputQ      :: !(Seq Float)
  , rtMarker       :: !Bool
  , rtCondEnabled  :: !Bool
  , rtCondTrigAddr :: !Word8
  , rtCondOpMode   :: !Word8
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- * RT Core Monad
--------------------------------------------------------------------------------

data RtEnvStatic = RtEnvStatic
  { rtWaveforms :: !(Vector (Vector Float))
  , rtWeights   :: !(Vector (Vector Float))
  , rtTriggers  :: !(Map Word8 Bool)
  }
  deriving (Show)

newtype RtM a = RtM (StateT RtCoreState (ReaderT RtEnvStatic (ExceptT SimError IO)) a)
  deriving (Functor, Applicative, Monad, MonadState RtCoreState, MonadReader RtEnvStatic, MonadError SimError, MonadIO)

runRtM :: RtEnvStatic -> RtCoreState -> RtM a -> IO (Either SimError a)
runRtM env st (RtM m) = runExceptT (runReaderT (evalStateT m st) env)

--------------------------------------------------------------------------------
-- * Simulation Environment & Errors
--------------------------------------------------------------------------------

data SimError
  = RtUnderflow !Word64
  | InvalidProgramCounter
  | InvalidDuration String
  | DecodeError String
  | ParseError String
  | LoaderError String
  | ReplError String
  deriving (Show, Eq)

data ModuleType = Qcm | Qrm | Qtm
  deriving (Show, Eq)

data SimConfig = SimConfig
  { simModuleType     :: !ModuleType
  , simWaveforms      :: !(Vector (Vector Float))
  , simWeights        :: !(Vector (Vector Float))
  , simMockData       :: !(Maybe [Complex Float])
  , simTriggerLatency :: !Word64
  , simTriggers       :: !(Map Word8 Bool)
  }
  deriving (Show)

data SimResult = SimResult
  { simFinalQ1State  :: !Q1CoreState
  , simFinalRtState  :: !RtCoreState
  , simErrors        :: ![SimError]
  , simOutputI       :: !(Vector Float)
  , simOutputQ       :: !(Vector Float)
  , simAcquisitions  :: !(Map (Int, Int) [AcqBin])
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- * Sequence Dictionary (JSON)
--------------------------------------------------------------------------------

data AcqConfig = AcqConfig
  { acqNumBins :: !Int
  }
  deriving (Show)

data SequenceDict = SequenceDict
  { sdWaveforms    :: !(Map Text (Vector Float))
  , sdWeights      :: !(Map Text (Vector Float))
  , sdAcquisitions :: !(Map Text AcqConfig)
  , sdProgram      :: ![Instruction]
  }
  deriving (Show)
