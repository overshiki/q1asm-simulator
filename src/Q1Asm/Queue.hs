module Q1Asm.Queue
  ( RtQueue(..)
  , StmQueue
  , newStmQueue
  ) where

import Control.Concurrent.STM
import Data.Word (Word64)
import Q1Asm.Types


--------------------------------------------------------------------------------
-- * Queue Abstraction
--------------------------------------------------------------------------------

class RtQueue q where
  qPush      :: q -> RtInstr -> IO ()
  -- | Pop an instruction.  'Nothing' means either the producer is done
  -- or an underflow would occur (checked against the producer's cycle time).
  qPop       :: q -> Word64 -> IO (Maybe RtInstr)
  qSetQ1Time :: q -> Word64 -> IO ()
  qSetQ1Done :: q -> IO ()
  qIsDone    :: q -> IO Bool

--------------------------------------------------------------------------------
-- * STM-Based Queue
--------------------------------------------------------------------------------

-- | A bounded queue backed by 'TBQueue' with shared status variables for
-- precise underflow detection.
data StmQueue = StmQueue
  { stmQueue  :: !(TBQueue RtInstr)
  , stmQ1Time :: !(TVar Word64)
  , stmQ1Done :: !(TVar Bool)
  }

newStmQueue :: Int -> IO StmQueue
newStmQueue capacity = atomically $ do
  q  <- newTBQueue (fromIntegral capacity)
  t  <- newTVar 0
  d  <- newTVar False
  return (StmQueue q t d)

instance RtQueue StmQueue where
  qPush sq instr = atomically $ do
    writeTBQueue (stmQueue sq) instr

  qPop sq rtTime = atomically $ do
    mInstr <- tryReadTBQueue (stmQueue sq)
    case mInstr of
      Just instr -> return (Just instr)
      Nothing  -> do
        done <- readTVar (stmQ1Done sq)
        if done
          then return Nothing
          else retry            -- wait for Q1 to push more or finish

  qSetQ1Time sq t = atomically $ writeTVar (stmQ1Time sq) t
  qSetQ1Done sq   = atomically $ writeTVar (stmQ1Done sq) True
  qIsDone sq      = atomically $ readTVar (stmQ1Done sq)
