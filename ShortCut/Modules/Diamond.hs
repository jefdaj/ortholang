module ShortCut.Modules.Diamond
  where

-- TODO any point in a diamond_makedb_each fn?
-- TODO any point in adding a daa type?

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
-- import System.Command              (readProcess)
import System.Process              (readProcess)

cutModule :: CutModule
cutModule = CutModule
  { mName = "Diamond"
  , mDesc = "Accelerated BLAST compatible local sequence aligner."
  , mTypes = [fna, faa, dmnd]
  , mFunctions =
      [ diamondmakedb
      , diamondmakedbAll
      ]
      ++ map mkDiamondBlast variants
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
rDiamondmakedbAll s@(_, cfg, ref, ids) e@(CutFun _ _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr s fas
  let out  = exprPath s e
      out' = debugRules cfg "rDiamondmakedbAll" e $ fromCutPath cfg out
  out' %> \_ -> do
    faPaths <- readPaths cfg ref fasPath
    aSimpleScriptPar "diamond_makedb_all.sh" cfg ref ids (out:faPaths)
  return (ExprPath out')
rDiamondmakedbAll _ e = error $ "bad argument to rDiamondmakedbAll: " ++ show e
 
--------------------
-- diamond_blast* --
--------------------

type DiamondBlastDesc = (String, [String] -> RulesFn, [String], CutType, CutType)

-- TODO can some of these be replaced by a numeric sensitivity arg?
variants :: [DiamondBlastDesc]
variants =
  [ ("blastp"                  , rDiamondFromFa, ["blastp"                    ], faa, faa )
  , ("blastp_sensitive"        , rDiamondFromFa, ["blastp", "--sensitive"     ], faa, faa )
  , ("blastp_more_sensitive"   , rDiamondFromFa, ["blastp", "--more-sensitive"], faa, faa )
  , ("blastp_db"               , rDiamondFromDb, ["blastp"                    ], faa, dmnd)
  , ("blastp_db_sensitive"     , rDiamondFromDb, ["blastp", "--sensitive"     ], faa, dmnd)
  , ("blastp_db_more_sensitive", rDiamondFromDb, ["blastp", "--more-sensitive"], faa, dmnd)
  , ("blastx"                  , rDiamondFromFa, ["blastx"                    ], fna, faa )
  , ("blastx_sensitive"        , rDiamondFromFa, ["blastx", "--sensitive"     ], fna, faa )
  , ("blastx_more_sensitive"   , rDiamondFromFa, ["blastx", "--more-sensitive"], fna, faa )
  , ("blastx_db"               , rDiamondFromDb, ["blastx"                    ], fna, dmnd)
  , ("blastx_db_sensitive"     , rDiamondFromDb, ["blastx", "--sensitive"     ], fna, dmnd)
  , ("blastx_db_more_sensitive", rDiamondFromDb, ["blastx", "--more-sensitive"], fna, dmnd)
  ]

mkDiamondBlast :: DiamondBlastDesc -> CutFunction
mkDiamondBlast (name, rFn, dCmd, qType, sType) = let name' = "diamond_" ++ name in CutFunction
  { fName      = name'
  , fTypeDesc  = mkTypeDesc name' [num, qType, sType] bht 
  , fTypeCheck = defaultTypeCheck [num, qType, sType] bht
  , fDesc      = Just $ "Like " ++ head dCmd ++ ", but uses DIAMOND for speed."
  , fFixity    = Prefix
  , fRules     = rFn dCmd
  }

rDiamondFromDb :: [String] -> RulesFn
rDiamondFromDb = rSimple . aDiamondFromDb

aDiamondFromDb :: [String] -> (CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ())
aDiamondFromDb dCmd cfg ref _ [o, e, q, db] = do
  eStr <- readLit  cfg ref e'
  wrappedCmdWrite True True cfg ref o'' [] [] [] "diamond.sh" $ [q', o'', eStr, db'] ++ dCmd
  where
    o'  = fromCutPath cfg o
    e'  = fromCutPath cfg e
    q'  = fromCutPath cfg q
    db' = fromCutPath cfg db
    o'' = debugA cfg "aDiamondblastpdb" o' $ dCmd ++ [e', o', q', db']
aDiamondFromDb _ _ _ _ _ = error $ "bad argument to aDiamondFromDb"

-- inserts a "makedb" call and reuses the _db compiler from above
-- based on the version in Blast.hs but a little simpler
rDiamondFromFa :: [String] -> RulesFn
rDiamondFromFa dCmd st (CutFun rtn salt deps _ [e, q, s])
  = rules st (CutFun rtn salt deps name1 [e, q, dbExpr])
  where
    rules  = rSimple $ aDiamondFromDb dCmd
    name1  = "diamond_" ++ head dCmd
    dbExpr = CutFun dmnd salt (depsOf s) "diamond_makedb" [s]
rDiamondFromFa _ _ _ = fail "bad argument to rDiamondFromFa"
