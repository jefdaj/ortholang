module ShortCut.Modules.Diamond
  where


import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimpleScriptPar, aSimpleScriptPar,
                                    rExpr, debugRules)
import ShortCut.Core.Locks         (withReadLock)
import ShortCut.Core.Util          (resolveSymlinks)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath, exprPath)
import ShortCut.Core.Actions       (readPaths)
import ShortCut.Modules.SeqIO      (fna, faa)
import System.Command              (readProcess)

cutModule :: CutModule
cutModule = CutModule
  { mName = "Diamond"
  , mDesc = "Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree"
  , mTypes = [fna, faa, dmnd]
  , mFunctions =
      [ diamondmakedb
      , diamondmakedbAll
      -- TODO any point in a diamond_makedb_each fn?
      -- , diamondBlastp
      -- , diamondBlastx
      -- , diamondBlastp_sensitive
      -- , diamondBlastx_sensitive
      -- , diamondBlastp_more_sensitive
      -- , diamondBlastx_more_sensitive
      ]
  }

dmnd :: CutType
dmnd = CutType
  { tExt  = "dmnd"
  , tDesc = "DIAMOND database"
  , tShow = \_ ref path -> do
      path' <- resolveSymlinks Nothing path
      out <- withReadLock ref path' $ readProcess "diamond" ["dbinfo", "--db", path'] []
      let desc = unlines $ ("DIAMOND database " ++ path) : (drop 4 $ lines out)
      return desc
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

diamondmakedb :: CutFunction
diamondmakedb = let name = "diamond_makedb" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name  [faa] dmnd 
  , fTypeCheck = defaultTypeCheck [faa] dmnd
  , fDesc      = Just "Create a DIAMOND database from a protein FASTA file."
  , fFixity    = Prefix
  , fRules     = rSimpleScriptPar "diamond_makedb.sh"
  }
 
------------------------
-- diamond_makedb_all --
------------------------

-- TODO need to figure out how to pass it the individual paths rather than a shortcut list

diamondmakedbAll :: CutFunction
diamondmakedbAll = let name = "diamond_makedb_all" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name  [ListOf faa] dmnd 
  , fTypeCheck = defaultTypeCheck [ListOf faa] dmnd
  , fDesc      = Just "Create one DIAMOND database from mutliple protein FASTA files."
  , fFixity    = Prefix
  , fRules     = rDiamondmakedbAll
  }

-- TODO should the reading the list + paths thing be included in rSimpleScript?
rDiamondmakedbAll :: RulesFn
rDiamondmakedbAll s@(_, cfg, ref) e@(CutFun _ _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr s fas
  let out      = exprPath s e
      out'     = debugRules cfg "rDiamondmakedbAll" e $ fromCutPath cfg out
      fasPath' = toCutPath cfg fasPath
  out' %> \_ -> do
    faPaths <- readPaths cfg ref fasPath
    let faPaths' = map (fromCutPath cfg) faPaths
    aSimpleScriptPar "diamond_makedb_all.sh" cfg ref (out:faPaths)
  return (ExprPath out')
rDiamondmakedbAll _ e = error $ "bad argument to rDiamondmakedbAll: " ++ show e

 
--------------------
-- diamond_blastp --
--------------------
