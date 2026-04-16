{-# LANGUAGE OverloadedStrings #-}

module Q1Asm.Repl.Loop
  ( runRepl
  , loadReplState
  ) where

import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Read as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Q1Asm.Loader (loadSequenceDict)
import Q1Asm.Repl.Commands
import Q1Asm.Repl.Display
import Q1Asm.Repl.Scheduler
import Q1Asm.Repl.State
import Q1Asm.Types

toDenseVector :: Map.Map Text (V.Vector Float) -> V.Vector (V.Vector Float)
toDenseVector m =
  let intMap = Map.fromList
        [ (n, v)
        | (k, v) <- Map.toList m
        , Right (n, "") <- [T.decimal k]
        ]
      maxIdx = if Map.null intMap then -1 else fst (Map.findMax intMap)
  in V.generate (maxIdx + 1) (\i -> Map.findWithDefault V.empty i intMap)

defaultTriggers :: Map.Map Word8 Bool
defaultTriggers = Map.fromList [(1, True)]

loadReplState :: FilePath -> IO (Either SimError ReplState)
loadReplState path = do
  bs <- BL.readFile path
  case loadSequenceDict bs of
    Left err -> return (Left err)
    Right sd -> return $ Right $ mkReplState (sdProgram sd)
                                               (toDenseVector (sdWaveforms sd))
                                               (toDenseVector (sdWeights sd))
                                               defaultTriggers

runRepl :: IO ()
runRepl = do
  putStrLn "Q1ASM Simulator REPL"
  putStrLn "Type 'help' for commands, 'quit' to exit."
  let emptySt = mkReplState [] V.empty V.empty Map.empty
  replLoop emptySt

replLoop :: ReplState -> IO ()
replLoop st = do
  putStr "q1asm-repl> "
  hFlush stdout
  line <- getLine
  case parseCommand line of
    CmdEmpty -> replLoop st
    CmdQuit -> putStrLn "Goodbye." >> exitSuccess
    CmdLoad path -> do
      eSt <- loadReplState path
      case eSt of
        Left err -> do putStrLn ("Error loading: " ++ show err); replLoop st
        Right st' -> do putStrLn ("Loaded: " ++ path); replLoop st'
    CmdNext -> handleStep runStep st
    CmdNextQ1 -> handleStep runStepQ1Force st
    CmdNextRt -> handleStep runStepRtForce st
    CmdContinue -> handleContinue st
    CmdInfo target -> do putStrLn (displayInfo target st); replLoop st
    CmdReset -> do
      let resetSt = mkReplState (q1Program (rsQ1 st)) (rtWaveforms (rsEnv st)) (rtWeights (rsEnv st)) (rtTriggers (rsEnv st))
      putStrLn "Simulation reset."
      replLoop resetSt
    CmdHelp -> do putStrLn helpText; replLoop st
    CmdUnknown cmd -> do putStrLn ("Unknown command: " ++ cmd); replLoop st

handleStep :: (ReplState -> Either SimError StepResult) -> ReplState -> IO ()
handleStep stepper st = case stepper st of
  Left err -> do putStrLn ("Error: " ++ show err); replLoop st
  Right sr -> do
    putStrLn (displayStep sr)
    replLoop (srNewState sr)

handleContinue :: ReplState -> IO ()
handleContinue st = case runStep st of
  Left err -> do putStrLn ("Error: " ++ show err); replLoop st
  Right sr -> do
    putStrLn (displayStep sr)
    case srEvent sr of
      EvHalt -> replLoop (srNewState sr)
      _ -> handleContinue (srNewState sr)

helpText :: String
helpText = unlines
  [ "Commands:"
  , "  load <file>          Load a JSON sequence dictionary"
  , "  next, n              Step one instruction (auto-scheduled)"
  , "  next q1, nq1         Force step Q1 Core"
  , "  next rt, nrt         Force step RT Core"
  , "  continue, c          Run until halt or error"
  , "  info q1, i q1        Show Q1 Core state"
  , "  info rt, i rt        Show RT Core state"
  , "  info queue, i q      Show queue contents"
  , "  info bins, i b       Show acquisition bins"
  , "  info waveforms, i w  Show loaded waveforms"
  , "  reset, r             Reset simulation"
  , "  help, h              Show this help"
  , "  quit, q              Exit REPL"
  ]
