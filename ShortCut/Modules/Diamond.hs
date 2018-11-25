module ShortCut.Modules.Diamond
  where


import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimpleScriptPar, aSimpleScriptPar,
                                    rExpr, debugRules, rSimple)
import ShortCut.Core.Locks         (withReadLock)
import ShortCut.Core.Util          (resolveSymlinks)
import ShortCut.Core.Paths         (CutPath, fromCutPath, exprPath)
import ShortCut.Core.Actions       (readPaths, readLit, debugA, wrappedCmdWrite)
import ShortCut.Modules.SeqIO      (fna, faa)
import ShortCut.Modules.Blast      (bht)
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
      , diamondblastpdb
      , diamondblastxdb
      , diamondblastp
      , diamondblastx
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
  let out  = exprPath s e
      out' = debugRules cfg "rDiamondmakedbAll" e $ fromCutPath cfg out
  out' %> \_ -> do
    faPaths <- readPaths cfg ref fasPath
    aSimpleScriptPar "diamond_makedb_all.sh" cfg ref (out:faPaths)
  return (ExprPath out')
rDiamondmakedbAll _ e = error $ "bad argument to rDiamondmakedbAll: " ++ show e
 
-----------------------
-- diamond_blastp_db --
-----------------------

diamondblastpdb :: CutFunction
diamondblastpdb = let name = "diamond_blastp_db" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name  [num, faa, dmnd] bht 
  , fTypeCheck = defaultTypeCheck [num, faa, dmnd] bht
  , fDesc      = Just "Like blastp_db, but uses DIAMOND for speed."
  , fFixity    = Prefix
  , fRules     = rSimple $ aDiamondFromDb "blastp"
  }

aDiamondFromDb :: String -> (CutConfig -> Locks -> [CutPath] -> Action ())
aDiamondFromDb dCmd cfg ref [o, e, q, db] = do
  eStr <- readLit  cfg ref e'
  wrappedCmdWrite True True cfg ref o'' [] [] [] "diamond" [dCmd, "-q", q', "-o", o'', "-e", eStr, "-d", db']
  where
    o'  = fromCutPath cfg o
    e'  = fromCutPath cfg e
    q'  = fromCutPath cfg q
    db' = fromCutPath cfg db
    o'' = debugA cfg "aDiamondblastpdb" o' [dCmd, e', o', q', db']
aDiamondFromDb _ _ _ _ = error $ "bad argument to aDiamondFromDb"

-----------------------
-- diamond_blastx_db --
-----------------------

diamondblastxdb :: CutFunction
diamondblastxdb = let name = "diamond_blastx_db" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name  [num, fna, dmnd] bht 
  , fTypeCheck = defaultTypeCheck [num, fna, dmnd] bht
  , fDesc      = Just "Like blastx_db, but uses DIAMOND for speed."
  , fFixity    = Prefix
  , fRules     = rSimple $ aDiamondFromDb "blastx"
  }

--------------------
-- diamond_blastp --
--------------------

diamondblastp :: CutFunction
diamondblastp = let name = "diamond_blastp" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name  [num, faa, faa] bht 
  , fTypeCheck = defaultTypeCheck [num, faa, faa] bht
  , fDesc      = Just "Like blastp, but uses DIAMOND for speed."
  , fFixity    = Prefix
  , fRules     = rDiamondFromFa "blastp"
  }

-- inserts a "makedb" call and reuses the _db compiler from above
-- based on the version in Blast.hs but a little simpler
rDiamondFromFa :: String -> RulesFn
rDiamondFromFa dCmd st (CutFun rtn salt deps _ [e, q, s])
  = rules st (CutFun rtn salt deps name1 [e, q, dbExpr])
  where
    rules  = rSimple $ aDiamondFromDb dCmd
    name1  = "diamond_" ++ dCmd
    dbExpr = CutFun dmnd salt (depsOf s) "diamond_makedb" [s]
rDiamondFromFa _ _ _ = error "bad argument to rDiamondFromFa"

--------------------
-- diamond_blastx --
--------------------

diamondblastx :: CutFunction
diamondblastx = let name = "diamond_blastx" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name  [num, fna, faa] bht 
  , fTypeCheck = defaultTypeCheck [num, fna, faa] bht
  , fDesc      = Just "Like blastx, but uses DIAMOND for speed."
  , fFixity    = Prefix
  , fRules     = rDiamondFromFa "blastx"
  }
