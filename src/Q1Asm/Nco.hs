module Q1Asm.Nco
  ( ncoAdvance
  , ncoPhaseAt
  ) where

import Data.Word (Word32, Word64)
import Q1Asm.Types (NcoState(..))

ncoAdvance :: NcoState -> Word32 -> NcoState
ncoAdvance nco duration = nco
  { ncoPhase = ncoPhase nco + 2 * pi * ncoFreq nco * fromIntegral duration
  }

ncoPhaseAt :: NcoState -> Word64 -> Word64 -> Double
ncoPhaseAt nco rtBase tOffset =
  ncoPhase nco + 2 * pi * ncoFreq nco * fromIntegral (rtBase + tOffset)
