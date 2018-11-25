module ShortCut.Modules.Diamond
  where

import System.Command (readProcess)
import ShortCut.Core.Types

import ShortCut.Modules.SeqIO (fna, faa)
import ShortCut.Core.Locks (withReadLock)

cutModule :: CutModule
cutModule = CutModule
  { mName = "Diamond"
  , mDesc = "Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree"
  , mTypes = [fna, faa, dmnd]
  , mFunctions = []
      -- [ diamondMakedb
      -- , diamondBlastp
      -- , diamondBlastx
      -- , diamondBlastp_sensitive
      -- , diamondBlastx_sensitive
      -- , diamondBlastp_more_sensitive
      -- , diamondBlastx_more_sensitive
      -- ]
  }

dmnd :: CutType
dmnd = CutType
  { tExt  = "dmnd"
  , tDesc = "DIAMOND database"
  , tShow = \_ ref path -> withReadLock ref path $ readProcess "diamond" ["dbinfo", path] []
  }


-- TODO are these needed, or should we convert to blast output immediately?
-- daa :: CutType
-- daa = CutType
--   { tExt  = "daa"
--   , tDesc = "DIAMOND alignment archive"
--   , tShow = \_ ref path -> do
--       undefined
--       -- txt <- readFileStrict ref path
--       -- return $ unlines $ take 17 $ lines txt
--   }

--------------------
-- diamond_makedb --
--------------------



--------------------
-- diamond_blastp --
--------------------
