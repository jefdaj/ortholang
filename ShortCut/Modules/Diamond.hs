module ShortCut.Modules.Diamond
  where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Modules.SeqIO (faa)

cutModule :: CutModule
cutModule = CutModule
  { mName = "Diamond"
  , mDesc = "Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree"
  , mTypes = [faa]
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
-- diamond_blastp --
--------------------
