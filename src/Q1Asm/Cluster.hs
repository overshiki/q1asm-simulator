module Q1Asm.Cluster
  ( ClusterConfig(..)
  , runCluster
  ) where

import Data.Map.Strict (Map)
import Q1Asm.Types

-- | Multi-sequencer cluster configuration.
-- This module is intentionally minimal for the first iteration,
-- acting as a placeholder for future SYNQ/LINQ multi-module support.
data ClusterConfig = ClusterConfig
  { clusterModules :: Map String SimConfig
  }
  deriving (Show)

-- | Run a cluster simulation.
-- Currently unimplemented and returns an error.
runCluster :: ClusterConfig -> Map String [Instruction] -> IO (Either SimError (Map String SimResult))
runCluster _ _ =
  return (Left (LoaderError "multi-sequencer cluster simulation not yet implemented"))
