{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Read as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Q1Asm.Loader (loadSequenceDict, SequenceDict(..))
import Q1Asm.Simulator (runSimulation)
import Q1Asm.Types

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> runFile path
    _      -> do
      putStrLn "Usage: q1asm-simulator <sequence.json>"
      exitFailure

-- | Convert a map with Text keys (expected to be decimal integers)
-- into a dense Vector indexed by those integers.
toDenseVector :: Map.Map Text (Vector Float) -> Vector (Vector Float)
toDenseVector m =
  let intMap = Map.fromList
        [ (n, v)
        | (k, v) <- Map.toList m
        , Right (n, "") <- [T.decimal k]
        ]
      maxIdx = if Map.null intMap then -1 else fst (Map.findMax intMap)
  in V.generate (maxIdx + 1) (\i -> Map.findWithDefault V.empty i intMap)

runFile :: FilePath -> IO ()
runFile path = do
  bs <- BL.readFile path
  case loadSequenceDict bs of
    Left err -> do
      putStrLn $ "Failed to load sequence: " ++ show err
      exitFailure
    Right sd -> do
      let cfg = SimConfig
            { simModuleType = Qcm
            , simWaveforms = toDenseVector (sdWaveforms sd)
            , simWeights = toDenseVector (sdWeights sd)
            , simMockData = Nothing
            , simTriggerLatency = 212
            , simTriggers = Map.fromList [(1, True)]
            }
      result <- runSimulation cfg (sdProgram sd)
      case result of
        Left err -> do
          putStrLn $ "Simulation error: " ++ show err
          exitFailure
        Right res -> do
          let q1s = simFinalQ1State res
              rts = simFinalRtState res
          putStrLn $ "Simulation completed successfully."
          putStrLn $ "Q1 Core final PC: " ++ show (q1Pc q1s)
          putStrLn $ "Q1 Core cycles: " ++ show (q1CycleTime q1s)
          putStrLn $ "RT Core time: " ++ show (rtTime rts) ++ " ns"
          putStrLn $ "Output samples (I): " ++ show (V.length (simOutputI res))
          putStrLn $ "Output samples (Q): " ++ show (V.length (simOutputQ res))
          putStrLn $ "Acquisitions: " ++ show (simAcquisitions res)
