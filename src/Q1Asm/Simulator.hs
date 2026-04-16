module Q1Asm.Simulator
  ( runSimulation
  , initQ1State
  , initRtEnv
  ) where

import Control.Concurrent.Async
import Control.Exception (SomeException)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as V
import Q1Asm.Types
import Q1Asm.Queue
import Q1Asm.Q1Core
import Q1Asm.RtCore

--------------------------------------------------------------------------------
-- * Top-Level Orchestration
--------------------------------------------------------------------------------

runSimulation :: SimConfig -> [Instruction] -> IO (Either SimError SimResult)
runSimulation cfg prog = do
  q <- newStmQueue 32
  let q1St = initQ1State prog
      rtEnv = initRtEnv (simWaveforms cfg) (simWeights cfg) (simTriggers cfg)
      rtSt = rtInitialState

  withAsync (runQ1Core q q1St) $ \q1Async ->
    withAsync (runRtCore q rtEnv rtSt) $ \rtAsync -> do
      result <- waitEitherCatch q1Async rtAsync
      case result of
        Left (Left e) -> do
          cancel rtAsync
          return (Left (ReplError ("Q1 Core exception: " ++ show (e :: SomeException))))
        Left (Right (Left e)) -> do
          cancel rtAsync
          return (Left e)
        Left (Right (Right q1s)) -> do
          ert <- waitCatch rtAsync
          case ert of
            Left e  -> return (Left (ReplError ("RT Core exception: " ++ show (e :: SomeException))))
            Right (Left e)  -> return (Left e)
            Right (Right rts) -> return (buildResult q1s rts)
        Right (Left e) -> do
          cancel q1Async
          return (Left (ReplError ("RT Core exception: " ++ show (e :: SomeException))))
        Right (Right (Left e)) -> do
          cancel q1Async
          return (Left e)
        Right (Right (Right rts)) -> do
          eq1 <- waitCatch q1Async
          case eq1 of
            Left e  -> return (Left (ReplError ("Q1 Core exception: " ++ show (e :: SomeException))))
            Right (Left e)  -> return (Left e)
            Right (Right q1s) -> return (buildResult q1s rts)
  where
    buildResult q1s rts = Right SimResult
      { simFinalQ1State = q1s
      , simFinalRtState = rts
      , simErrors = []
      , simOutputI = V.fromList (toList (rtOutputI rts))
      , simOutputQ = V.fromList (toList (rtOutputQ rts))
      , simAcquisitions = rtAcqBins rts
      }

--------------------------------------------------------------------------------
-- * Runners
--------------------------------------------------------------------------------

runQ1Core :: RtQueue q => q -> Q1CoreState -> IO (Either SimError Q1CoreState)
runQ1Core q st =
  runQ1M st $ do
    (q1Loop q >> return ()) `catchError` \e -> do
      liftIO (qSetQ1Done q)
      throwError e
    liftIO (qSetQ1Done q)
    get

runRtCore :: RtQueue q => q -> RtEnvStatic -> RtCoreState -> IO (Either SimError RtCoreState)
runRtCore q env st =
  runRtM env st $ do
    rtLoop q
    get
