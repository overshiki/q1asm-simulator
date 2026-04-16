{-# LANGUAGE BangPatterns #-}

module Q1Asm.Renderer
  ( renderPlay
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Sequence as Seq
import Data.Word (Word32, Word64)
import Q1Asm.Types

operandValue :: Operand -> Word32
operandValue (Imm v) = v
operandValue (Reg _) = error "Renderer should never see a register operand"

renderPlay :: Operand -> Operand -> Word32 -> RtM ()
renderPlay opW0 opW1 duration = do
  env <- ask
  let wfs = rtWaveforms env
      w0Idx = fromIntegral (operandValue opW0)
      w1Idx = fromIntegral (operandValue opW1)
      wf0 = if w0Idx >= 0 && w0Idx < V.length wfs then wfs V.! w0Idx else V.empty
      wf1 = if w1Idx >= 0 && w1Idx < V.length wfs then wfs V.! w1Idx else V.empty
  st <- get
  let params = rtActiveParams st
      nco = rtNcoState st
      t0 = rtTime st
      d = fromIntegral duration
      freq = ncoFreq nco
      phase0 = ncoPhase nco
      len0 = V.length wf0
      len1 = V.length wf1

  -- Render samples one by one
  let go !t !outI !outQ !ph
        | t >= d    = (outI, outQ, ph)
        | otherwise =
            let sample0 = if t < len0 then wf0 V.! t else 0
                sample1 = if t < len1 then wf1 V.! t else 0
                phVal = phase0 + 2 * pi * freq * fromIntegral t
                iVal = sample0 * realToFrac (cos phVal) * actAwgGainI params + actAwgOffsI params
                qVal = sample1 * realToFrac (sin phVal) * actAwgGainQ params + actAwgOffsQ params
            in go (t + 1) (outI Seq.|> iVal) (outQ Seq.|> qVal) phVal

  let (newOutI, newOutQ, finalPh) = go 0 (rtOutputI st) (rtOutputQ st) phase0
      newPhase = phase0 + 2 * pi * freq * fromIntegral d

  modify $ \s -> s
    { rtOutputI = newOutI
    , rtOutputQ = newOutQ
    , rtNcoState = nco { ncoPhase = newPhase }
    , rtTime = t0 + fromIntegral d
    }
