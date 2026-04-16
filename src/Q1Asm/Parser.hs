{-# LANGUAGE OverloadedStrings #-}

module Q1Asm.Parser
  ( parseProgram
  ) where

import Control.Applicative
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word8)
import Data.Int (Int32)
import GHC.Float (castFloatToWord32)
import Q1Asm.Types

--------------------------------------------------------------------------------
-- * Top-level API
--------------------------------------------------------------------------------

parseProgram :: Text -> Either SimError [Instruction]
parseProgram txt =
  case parseOnly programParser txt of
    Left e  -> Left (ParseError e)
    Right is -> Right is

--------------------------------------------------------------------------------
-- * Attoparsec Grammar
--------------------------------------------------------------------------------

-- | A raw parsed line: optional label, mnemonic, raw arg strings.
type RawLine = (Maybe Text, Text, [Text])

programParser :: Parser [Instruction]
programParser = do
  rawLines <- many' lineParser
  endOfInput
  let merged = mergeLabelLines rawLines
      labels = buildLabelMap merged
  mapM (\x -> either fail return . resolveLine labels $ x) (zip [0..] merged)

-- | Merge label-only lines with the following instruction line.
mergeLabelLines :: [RawLine] -> [RawLine]
mergeLabelLines = go []
  where
    go acc [] = reverse acc
    go acc ((Just lbl, "", []) : rest) =
      -- Label-only line: attach to next instruction
      case rest of
        ((Nothing, mnem, args) : rest') ->
          go ((Just lbl, mnem, args) : acc) rest'
        ((Just _, _, _) : _) ->
          -- Next line also has a label; keep current as-is and let next merge
          go ((Just lbl, "", []) : acc) rest
        [] ->
          -- Label at end of program with no instruction; drop it
          reverse acc
    go acc (x : rest) = go (x : acc) rest

lineParser :: Parser RawLine
lineParser = do
  skipSpace
  mLbl <- optional labelDef
  skipSpace
  -- If we see end of line/end of input here, and we parsed a label,
  -- this is a label-only line.
  isEnd <- lookAhead (endOfLine <|> endOfInput >> return True) <|> return False
  if isEnd && mLbl /= Nothing
    then endOfLine <|> endOfInput >> return (mLbl, "", [])
    else do
      mnem <- mnemonic
      skipSpace
      args <- argsParser <|> pure []
      skipSpace
      optional comment
      endOfLine <|> endOfInput
      return (mLbl, mnem, args)

labelDef :: Parser Text
labelDef = do
  lbl <- takeWhile1 isLabelChar
  char ':'
  return lbl

mnemonic :: Parser Text
mnemonic = takeWhile1 (\c -> isAlphaNum c || c == '_' || c == '-')

argsParser :: Parser [Text]
argsParser = do
  skipSpace
  arg <- takeWhile1 (\c -> c /= ',' && c /= '#' && not (isEndOfLine c))
  let arg' = T.strip arg
  rest <- (char ',' >> argsParser) <|> pure []
  return (arg' : rest)

comment :: Parser Text
comment = char '#' >> takeTill isEndOfLine

isLabelChar :: Char -> Bool
isLabelChar c = isAlphaNum c || c == '_'

--------------------------------------------------------------------------------
-- * Label Resolution
--------------------------------------------------------------------------------

buildLabelMap :: [RawLine] -> Map Text Int
buildLabelMap = snd . foldl go (0, Map.empty)
  where
    go (idx, m) (Just lbl, _, _) = (idx + 1, Map.insert lbl idx m)
    go (idx, m) (Nothing, _, _)  = (idx + 1, m)

resolveLine :: Map Text Int -> (Int, RawLine) -> Either String Instruction
resolveLine labels (idx, (_, mnem, args)) =
  parseInstr idx labels (T.toLower mnem) args

--------------------------------------------------------------------------------
-- * Instruction Parsing
--------------------------------------------------------------------------------

parseInstr :: Int -> Map Text Int -> Text -> [Text] -> Either String Instruction
parseInstr _  _ "move" [a, b] =
  (\v r -> Q1Only (Move v r)) <$> parseOperand a <*> parseReg b
parseInstr _  _ "add" [a, b, c] =
  (\x y z -> Q1Only (Add x y z)) <$> parseReg a <*> parseOperand b <*> parseReg c
parseInstr _  _ "sub" [a, b, c] =
  (\x y z -> Q1Only (Sub x y z)) <$> parseReg a <*> parseOperand b <*> parseReg c
parseInstr _  _ "and" [a, b, c] =
  (\x y z -> Q1Only (And x y z)) <$> parseReg a <*> parseOperand b <*> parseReg c
parseInstr _  _ "or" [a, b, c] =
  (\x y z -> Q1Only (Or x y z)) <$> parseReg a <*> parseOperand b <*> parseReg c
parseInstr _  _ "xor" [a, b, c] =
  (\x y z -> Q1Only (Xor x y z)) <$> parseReg a <*> parseOperand b <*> parseReg c
parseInstr _  _ "not" [a, b] =
  (\x y -> Q1Only (Not x y)) <$> parseReg a <*> parseReg b
parseInstr _  _ "asl" [a, b, c] =
  (\x y z -> Q1Only (Asl x y z)) <$> parseReg a <*> parseOperand b <*> parseReg c
parseInstr _  _ "asr" [a, b, c] =
  (\x y z -> Q1Only (Asr x y z)) <$> parseReg a <*> parseOperand b <*> parseReg c
parseInstr _  labels "jmp" [a] =
  (Q1Only . Jmp) <$> parseAddr labels a
parseInstr _  labels "jge" [a, b, c] =
  (\x y z -> Q1Only (Jge x y z)) <$> parseReg a <*> parseOperand b <*> parseAddr labels c
parseInstr _  labels "jlt" [a, b, c] =
  (\x y z -> Q1Only (Jlt x y z)) <$> parseReg a <*> parseOperand b <*> parseAddr labels c
parseInstr _  labels "loop" [a, b] =
  (\r a -> Q1Only (Loop r a)) <$> parseReg a <*> parseAddr labels b
parseInstr idx labels "jr" [a] =
  (Q1Only . Jr) <$> parseRelAddr idx labels a
parseInstr _  _ "nop" [] =
  Right (Q1Only Nop)
parseInstr _  _ "stop" [] =
  Right (Q1Only Stop)
parseInstr _  _ "halt" [] =
  Right (Q1Only Stop)
parseInstr _  _ "wait" [a] =
  Rt . Wait <$> parseImm a
parseInstr _  _ "wait_sync" [a] =
  Rt . WaitSync <$> parseImm a
parseInstr _  _ "play" [a, b, c] =
  Rt <$> (Play <$> parseOperand a <*> parseOperand b <*> parseImm c)
parseInstr _  _ "acquire" [a, b, c] =
  Rt <$> (Acquire <$> parseOperand a <*> parseOperand b <*> parseImm c)
parseInstr _  _ "acquire_weighed" [a, b, c, d, e] =
  Rt <$> (AcquireWeighed <$> parseOperand a <*> parseOperand b <*> parseImm c <*> parseOperand d <*> parseOperand e)
parseInstr _  _ "acquire_ttl" [a, b, c, d] =
  Rt <$> (AcquireTtl <$> parseOperand a <*> parseOperand b <*> parseOperand c <*> parseImm d)
parseInstr _  _ "set_freq" [a, b] =
  Rt <$> (SetFreq <$> parseOperand a <*> parseOperand b)
parseInstr _  _ "set_freq" [a] =
  Rt <$> (SetFreq <$> parseOperand a <*> pure (Imm 0))
parseInstr _  _ "set_phase" [a, b] =
  Rt <$> (SetPhase <$> parseOperand a <*> parseOperand b)
parseInstr _  _ "set_phase" [a] =
  Rt <$> (SetPhase <$> parseOperand a <*> pure (Imm 0))
parseInstr _  _ "reset_phase" [] =
  Right (Rt ResetPhase)
parseInstr _  _ "set_awg_offs" [a, b] =
  Rt <$> (SetAwgOffs <$> parseAwgOffsOperand a <*> parseAwgOffsOperand b)
  where
    parseAwgOffsOperand t = case parseFloat t of
      Right f -> Right (Imm (castFloatToWord32 f))
      Left  _ -> parseOperand t
parseInstr _  _ "set_awg_gain" [a, b] =
  Rt <$> (SetAwgGain <$> parseOperand a <*> parseOperand b)
parseInstr _  _ "set_mrk" [a] =
  Rt . SetMrk <$> parseOperand a
parseInstr _  _ "set_scope_en" [a] =
  Rt . SetScopeEn <$> parseOperand a
parseInstr _  _ "set_time_ref" [] =
  Right (Rt SetTimeRef)
parseInstr _  _ "set_digital" [a] =
  Rt . SetDigital <$> parseOperand a
parseInstr _  _ "upd_param" [] =
  Right (Rt (UpdParam 4))
parseInstr _  _ "upd_param" [a] =
  Rt . UpdParam <$> parseImm a
parseInstr _  _ "set_cond" [a, b, c, d] =
  Rt <$> (SetCond <$> parseOperand a <*> parseWord8 b <*> parseWord8 c <*> parseImm d)
parseInstr _  _ "latch_rst" [a] =
  Rt . LatchRst <$> parseImm a
parseInstr _  _ "set_latch_en" [a, b] =
  Rt <$> (SetLatchEn <$> parseOperand a <*> parseImm b)
parseInstr _  _ "wait_trigger" [a, b] =
  Rt <$> (WaitTrigger <$> parseWord8 a <*> parseImm b)
parseInstr _  _ m _ =
  Left ("unknown or malformed instruction: " ++ T.unpack m)

--------------------------------------------------------------------------------
-- * Operand Parsers
--------------------------------------------------------------------------------

parseOperand :: Text -> Either String Operand
parseOperand t
  | "R" `T.isPrefixOf` t = Reg <$> parseReg t
  | otherwise            = Imm <$> parseImm t

parseReg :: Text -> Either String RegIdx
parseReg t = case T.uncons t of
  Just ('R', rest) | T.all isDigit rest ->
    let n = read (T.unpack rest) :: Int
    in if n >= 0 && n <= 63
         then Right (fromIntegral n)
         else Left ("register out of range: " ++ T.unpack t)
  _ -> Left ("expected register, got: " ++ T.unpack t)

parseImm :: Text -> Either String Word32
parseImm t
  | "0x" `T.isPrefixOf` t =
      case readHex (T.drop 2 t) of
        Just v  -> Right v
        Nothing -> Left ("invalid hex: " ++ T.unpack t)
  | otherwise =
      case reads (T.unpack t) of
        [(n, "")] -> Right n
        _         -> Left ("invalid immediate: " ++ T.unpack t)

parseWord8 :: Text -> Either String Word8
parseWord8 t = case parseImm t of
  Right v | v <= 255 -> Right (fromIntegral v)
  _ -> Left ("invalid Word8: " ++ T.unpack t)

parseFloat :: Text -> Either String Float
parseFloat t = case reads (T.unpack t) of
  [(f, "")] -> Right f
  _         -> Left ("invalid float: " ++ T.unpack t)

parseAddr :: Map Text Int -> Text -> Either String Int32
parseAddr labels t = case resolveLabel labels t of
  Just addr -> Right (fromIntegral addr)
  Nothing   -> fromIntegral <$> parseImm t

parseRelAddr :: Int -> Map Text Int -> Text -> Either String Int32
parseRelAddr idx labels t = case resolveLabel labels t of
  Just addr -> Right (fromIntegral (addr - idx - 1))
  Nothing   -> fromIntegral <$> parseImm t

resolveLabel :: Map Text Int -> Text -> Maybe Int
resolveLabel labels t
  | "@" `T.isPrefixOf` t = Map.lookup (T.drop 1 t) labels
  | otherwise            = Nothing

readHex :: Text -> Maybe Word32
readHex t
  | T.all isHexDigit t = Just (T.foldl' go 0 t)
  | otherwise          = Nothing
  where
    go acc c = acc * 16 + fromIntegral (hexValue c)
    hexValue c
      | isDigit c       = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = 10 + fromEnum c - fromEnum 'a'
      | c >= 'A' && c <= 'F' = 10 + fromEnum c - fromEnum 'A'
      | otherwise       = 0

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
