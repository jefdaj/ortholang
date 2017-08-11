module ShortCut.Core.Slurm where

-- import ShortCut.Core.Types
import System.Environment (lookupEnv)
import Development.Shake  (Resource, newResourceIO)

slurmNodesResource :: IO (Maybe Resource)
slurmNodesResource = do
  snn <- lookupEnv "SLURM_NUM_NODES"
  case snn of
    Nothing -> return Nothing
    Just ns -> do
      r <- newResourceIO "slurm_nodes" $ read ns
      return $ Just r
