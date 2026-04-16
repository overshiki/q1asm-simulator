{-# LANGUAGE BinaryLiterals #-}

module Q1Asm.Encode
  ( RtPacket(..)
  , encodeRtInstr
  , decodeRtPacket
  , putRtPacket
  , getRtPacket
  ) where

import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import Q1Asm.Types

--------------------------------------------------------------------------------
-- * Packet Structure
--------------------------------------------------------------------------------

-- | Native 64-bit header plus optional 32-bit extended words.
data RtPacket = RtPacket
  { pktOpcode  :: !Word8
  , pktFlags   :: !Word8
  , pktArgA    :: !Word16
  , pktArgB    :: !Word16
  , pktExtArgs :: ![Word32]
  }
  deriving (Show, Eq)

putRtPacket :: RtPacket -> Put
putRtPacket (RtPacket op fl a b exts) = do
  putWord8 op
  putWord8 fl
  putWord16le a
  putWord16le b
  mapM_ putWord32le exts

getRtPacket :: Get RtPacket
getRtPacket = do
  op <- getWord8
  fl <- getWord8
  a  <- getWord16le
  b  <- getWord16le
  -- Determine number of extended args from flags.
  let nExt = countExtWords fl
  exts <- sequence (replicate nExt getWord32le)
  return (RtPacket op fl a b exts)

countExtWords :: Word8 -> Int
countExtWords flags = go 0 0 where
  go acc slot | slot > 3  = acc
              | otherwise =
                  let typ = (flags `shiftR` (2 * slot)) .&. 0x03
                      needsExt = (typ == 0b11) || (slot >= 2 && typ /= 0b00)
                  in go (if needsExt then acc + 1 else acc) (slot + 1)

--------------------------------------------------------------------------------
-- * Operand Encoding Helpers
--------------------------------------------------------------------------------

-- | Encode an operand for slot 0 or 1 (may fit in header).
encodeOpHeader :: Int -> Operand -> (Word8, Word16, [Word32])
encodeOpHeader slot (Reg r) =
  (0b01 `shiftL` (2 * slot), fromIntegral r, [])
encodeOpHeader slot (Imm v)
  | v <= 0xFFFF =
      (0b10 `shiftL` (2 * slot), fromIntegral v, [])
  | otherwise   =
      (0b11 `shiftL` (2 * slot), 0, [v])

-- | Encode an operand for slot 2 or 3 (always forced to extended words).
encodeOpExt :: Int -> Operand -> (Word8, Word16, [Word32])
encodeOpExt slot (Reg r) =
  (0b01 `shiftL` (2 * slot), 0, [fromIntegral r])
encodeOpExt slot (Imm v) =
  (0b11 `shiftL` (2 * slot), 0, [v])

--------------------------------------------------------------------------------
-- * Instruction -> Packet
--------------------------------------------------------------------------------

encodeRtInstr :: RtInstr -> RtPacket
encodeRtInstr (Wait d) =
  RtPacket 0x10 0b00000011 0 0 [d]
encodeRtInstr (WaitSync d) =
  RtPacket 0x11 0b00000011 0 0 [d]
encodeRtInstr (Play w0 w1 d) =
  let (f0, a0, e0) = encodeOpHeader 0 w0
      (f1, a1, e1) = encodeOpHeader 1 w1
      flags = f0 .|. f1 .|. (0b10 `shiftL` 4)  -- slot2 = duration (Imm32)
  in RtPacket 0x20 flags a0 a1 (e0 ++ e1 ++ [d])
encodeRtInstr (Acquire a b d) =
  let (f0, a0, e0) = encodeOpHeader 0 a
      (f1, a1, e1) = encodeOpHeader 1 b
      flags = f0 .|. f1 .|. (0b10 `shiftL` 4)
  in RtPacket 0x30 flags a0 a1 (e0 ++ e1 ++ [d])
encodeRtInstr (AcquireWeighed a b d w0 w1) =
  let (f0, a0, e0) = encodeOpHeader 0 a
      (f1, a1, e1) = encodeOpHeader 1 b
      (f2, _, e2)  = encodeOpExt 2 w0
      (f3, _, e3)  = encodeOpExt 3 w1
      flags = f0 .|. f1 .|. f2 .|. f3 .|. (0b10 `shiftL` 4)
  in RtPacket 0x31 flags a0 a1 (e0 ++ e1 ++ e2 ++ e3 ++ [d])
encodeRtInstr (AcquireTtl a b e d) =
  let (f0, a0, e0) = encodeOpHeader 0 a
      (f1, a1, e1) = encodeOpHeader 1 b
      (f2, _, e2)  = encodeOpExt 2 e
      flags = f0 .|. f1 .|. f2 .|. (0b10 `shiftL` 4)
  in RtPacket 0x32 flags a0 a1 (e0 ++ e1 ++ e2 ++ [d])
encodeRtInstr (SetFreq op path) =
  let (f0, a0, e0) = encodeOpHeader 0 op
      (f1, a1, e1) = encodeOpHeader 0 path
      flags = f0 .|. f1
  in RtPacket 0x40 flags a0 a1 (e0 ++ e1)
encodeRtInstr (SetPhase op path) =
  let (f0, a0, e0) = encodeOpHeader 0 op
      (f1, a1, e1) = encodeOpHeader 0 path
      flags = f0 .|. f1
  in RtPacket 0x41 flags a0 a1 (e0 ++ e1)
encodeRtInstr ResetPhase =
  RtPacket 0x42 0 0 0 []
encodeRtInstr (SetAwgOffs i q) =
  let (f0, a0, e0) = encodeOpHeader 0 i
      (f1, a1, e1) = encodeOpHeader 1 q
      flags = f0 .|. f1
  in RtPacket 0x43 flags a0 a1 (e0 ++ e1)  -- placeholder
encodeRtInstr (SetAwgGain i q) =
  let (f0, a0, e0) = encodeOpHeader 0 i
      (f1, a1, e1) = encodeOpHeader 1 q
      flags = f0 .|. f1
  in RtPacket 0x46 flags a0 a1 (e0 ++ e1)
encodeRtInstr (SetMrk op) =
  let (f0, a0, e0) = encodeOpHeader 0 op
  in RtPacket 0x45 f0 a0 0 e0
encodeRtInstr (UpdParam d) =
  RtPacket 0x44 0b00000011 0 0 [d]
encodeRtInstr (SetScopeEn op) =
  let (f0, a0, e0) = encodeOpHeader 0 op
  in RtPacket 0x47 f0 a0 0 e0
encodeRtInstr SetTimeRef =
  RtPacket 0x48 0 0 0 []
encodeRtInstr (SetDigital op) =
  let (f0, a0, e0) = encodeOpHeader 0 op
  in RtPacket 0x49 f0 a0 0 e0
encodeRtInstr (SetCond en addr mode d) =
  let (f0, a0, e0) = encodeOpHeader 0 en
      (f1, a1, e1) = encodeOpHeader 1 (Imm (fromIntegral addr))
      flags = f0 .|. f1 .|. (0b10 `shiftL` 4) .|. (0b10 `shiftL` 6)
  in RtPacket 0x52 flags a0 a1 (e0 ++ e1 ++ [fromIntegral mode, d])
encodeRtInstr (LatchRst d) =
  RtPacket 0x53 0b00000011 0 0 [d]
encodeRtInstr (SetLatchEn e d) =
  let (f0, a0, e0) = encodeOpHeader 0 e
      flags = f0 .|. (0b10 `shiftL` 4)
  in RtPacket 0x50 flags a0 0 (e0 ++ [d])
encodeRtInstr (WaitTrigger addr d) =
  RtPacket 0x51 0b00000010 (fromIntegral addr) 0 [d]

--------------------------------------------------------------------------------
-- * Packet -> Instruction
--------------------------------------------------------------------------------

decodeRtPacket :: RtPacket -> Either String RtInstr
decodeRtPacket (RtPacket op fl a b exts) =
  case op of
    0x10 -> Wait <$> decodeDuration fl exts
    0x11 -> WaitSync <$> decodeDuration fl exts
    0x20 -> decodePlay fl a b exts
    0x30 -> decodeAcquire fl a b exts
    0x31 -> decodeAcquireWeighed fl a b exts
    0x32 -> decodeAcquireTtl fl a b exts
    0x40 -> SetFreq <$> decodeSlot0Only fl a b exts <*> decodeSlot1Only fl a b exts
    0x41 -> SetPhase <$> decodeSlot0Only fl a b exts <*> decodeSlot1Only fl a b exts
    0x42 -> Right ResetPhase
    0x43 -> decodeSetAwgOffs fl a b exts
    0x44 -> UpdParam <$> decodeDuration fl exts
    0x45 -> SetMrk <$> decodeSlot0Only fl a b exts
    0x46 -> decodeSetAwgGain fl a b exts
    0x47 -> SetScopeEn <$> decodeSlot0Only fl a b exts
    0x48 -> Right SetTimeRef
    0x49 -> SetDigital <$> decodeSlot0Only fl a b exts
    0x52 -> decodeSetCond fl a b exts
    0x53 -> LatchRst <$> decodeDuration fl exts
    0x50 -> decodeSetLatchEn fl a b exts
    0x51 -> case exts of
               (d:_) -> Right $ WaitTrigger (fromIntegral a) d
               _     -> Left "wait_trigger: missing duration"
    _    -> Left $ "unknown opcode: 0x" ++ showHex op ""

showHex :: Word8 -> ShowS
showHex w = showString (map toHex [w `shiftR` 4, w .&. 0x0F])
  where toHex n | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
                | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)

decodeDuration :: Word8 -> [Word32] -> Either String Word32
decodeDuration fl exts = do
  (op, rest) <- decodeSlot0 fl 0 0 exts
  case op of
    Imm d -> Right d
    Reg _ -> Left "duration cannot be a register in this encoding"

decodePlay :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String RtInstr
decodePlay fl a b exts = do
  (w0, r0) <- decodeSlot0 fl a b exts
  (w1, r1) <- decodeSlot1 fl a b r0
  case r1 of
    (d:_) -> Right $ Play w0 w1 d
    _     -> Left "play: missing duration"

decodeAcquire :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String RtInstr
decodeAcquire fl a b exts = do
  (ai, r0) <- decodeSlot0 fl a b exts
  (bi, r1) <- decodeSlot1 fl a b r0
  case r1 of
    (d:_) -> Right $ Acquire ai bi d
    _     -> Left "acquire: missing duration"

decodeAcquireWeighed :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String RtInstr
decodeAcquireWeighed fl a b exts = do
  (ai, r0) <- decodeSlot0 fl a b exts
  (bi, r1) <- decodeSlot1 fl a b r0
  case r1 of
    (d:r2) -> do
      (w0, r3) <- decodeSlot2 fl r2
      (w1, _)  <- decodeSlot3 fl r3
      Right $ AcquireWeighed ai bi d w0 w1
    _     -> Left "acquire_weighed: missing duration"

decodeAcquireTtl :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String RtInstr
decodeAcquireTtl fl a b exts = do
  (ai, r0) <- decodeSlot0 fl a b exts
  (bi, r1) <- decodeSlot1 fl a b r0
  (en, r2) <- decodeSlot2 fl r1
  case r2 of
    (d:_) -> Right $ AcquireTtl ai bi en d
    _     -> Left "acquire_ttl: missing duration"

decodeSetLatchEn :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String RtInstr
decodeSetLatchEn fl a b exts = do
  (en, r0) <- decodeSlot0 fl a b exts
  case r0 of
    (d:_) -> Right $ SetLatchEn en d
    _     -> Left "set_latch_en: missing duration"

decodeSetAwgOffs :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String RtInstr
decodeSetAwgOffs fl a b exts = do
  (i, r0) <- decodeSlot0 fl a b exts
  (q, _)  <- decodeSlot1 fl a b r0
  return $ SetAwgOffs i q

decodeSetAwgGain :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String RtInstr
decodeSetAwgGain fl a b exts = do
  (i, r0) <- decodeSlot0 fl a b exts
  (q, _)  <- decodeSlot1 fl a b r0
  return $ SetAwgGain i q

decodeSetCond :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String RtInstr
decodeSetCond fl a b exts = do
  (en, r0) <- decodeSlot0 fl a b exts
  (addr, r1) <- decodeSlot1 fl a b r0
  case r1 of
    (mode:d:_) -> do
      let addrVal = case addr of Imm v -> fromIntegral v; Reg r -> fromIntegral r
      return $ SetCond en addrVal (fromIntegral mode) d
    _ -> Left "set_cond: missing mode or duration"

decodeSlot0Only :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String Operand
decodeSlot0Only fl a b exts = do
  (op, _rest) <- decodeSlot0 fl a b exts
  return op

decodeSlot1Only :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String Operand
decodeSlot1Only fl a b exts = do
  (op, _rest) <- decodeSlot1 fl a b exts
  return op

-- | Decode slot 0 (uses ArgA when fitting).
decodeSlot0 :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String (Operand, [Word32])
decodeSlot0 fl a b exts = case fl .&. 0b11 of
  0b00 -> Right (Imm 0, exts)
  0b01 -> Right (Reg (fromIntegral a), exts)
  0b10 -> Right (Imm (fromIntegral a), exts)
  0b11 -> case exts of (x:xs) -> Right (Imm x, xs); [] -> Left "missing ext arg slot0"
  _    -> error "impossible"

-- | Decode slot 1 (uses ArgB when fitting).
decodeSlot1 :: Word8 -> Word16 -> Word16 -> [Word32] -> Either String (Operand, [Word32])
decodeSlot1 fl a b exts = case (fl `shiftR` 2) .&. 0b11 of
  0b00 -> Right (Imm 0, exts)
  0b01 -> Right (Reg (fromIntegral b), exts)
  0b10 -> Right (Imm (fromIntegral b), exts)
  0b11 -> case exts of (x:xs) -> Right (Imm x, xs); [] -> Left "missing ext arg slot1"
  _    -> error "impossible"

-- | Decode slot 2 (always from extended words).
decodeSlot2 :: Word8 -> [Word32] -> Either String (Operand, [Word32])
decodeSlot2 fl exts = case (fl `shiftR` 4) .&. 0b11 of
  0b00 -> Right (Imm 0, exts)
  0b01 -> case exts of (x:xs) -> Right (Reg (fromIntegral x), xs); [] -> Left "missing ext arg slot2"
  0b10 -> case exts of (x:xs) -> Right (Imm x, xs); [] -> Left "missing ext arg slot2"
  0b11 -> case exts of (x:xs) -> Right (Imm x, xs); [] -> Left "missing ext arg slot2"
  _    -> error "impossible"

-- | Decode slot 3 (always from extended words).
decodeSlot3 :: Word8 -> [Word32] -> Either String (Operand, [Word32])
decodeSlot3 fl exts = case (fl `shiftR` 6) .&. 0b11 of
  0b00 -> Right (Imm 0, exts)
  0b01 -> case exts of (x:xs) -> Right (Reg (fromIntegral x), xs); [] -> Left "missing ext arg slot3"
  0b10 -> case exts of (x:xs) -> Right (Imm x, xs); [] -> Left "missing ext arg slot3"
  0b11 -> case exts of (x:xs) -> Right (Imm x, xs); [] -> Left "missing ext arg slot3"
  _    -> error "impossible"
