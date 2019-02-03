module Detourrr.Modules.Diamond
  where

-- TODO any point in a diamond_makedb_each fn?
-- TODO any point in adding a daa type?

import Development.Shake
import Detourrr.Core.Types

import Detourrr.Core.Compile.Basic (defaultTypeCheck, rSimpleScriptPar, aSimpleScriptPar,
                                    rExpr, debugRules, rSimple)
import Detourrr.Core.Locks         (withReadLock)
import Detourrr.Core.Util          (resolveSymlinks)
import Detourrr.Core.Paths         (RrrPath, fromRrrPath, exprPath)
import Detourrr.Core.Actions       (readPaths, readLit, debugA, wrappedCmdWrite)
import Detourrr.Modules.SeqIO      (fna, faa)
import Detourrr.Modules.Blast      (bht)
import System.Command              (readProcess)

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "Diamond"
  , mDesc = "Accelerated BLAST compatible local sequence aligner."
  , mTypes = [fna, faa, dmnd]
  , mFunctions =
      [ diamondmakedb
      , diamondmakedbAll
      ]
      ++ map mkDiamondBlast variants
  }

dmnd :: RrrType
dmnd = RrrType
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

diamondmakedb :: RrrFunction
diamondmakedb = let name = "diamond_makedb" in RrrFunction
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

diamondmakedbAll :: RrrFunction
diamondmakedbAll = let name = "diamond_makedb_all" in RrrFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name  [ListOf faa] dmnd 
  , fTypeCheck = defaultTypeCheck [ListOf faa] dmnd
  , fDesc      = Just "Create one DIAMOND database from mutliple protein FASTA files."
  , fFixity    = Prefix
  , fRules     = rDiamondmakedbAll
  }

-- TODO should the reading the list + paths thing be included in rSimpleScript?
rDiamondmakedbAll :: RulesFn
rDiamondmakedbAll s@(_, cfg, ref, ids) e@(RrrFun _ _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr s fas
  let out  = exprPath s e
      out' = debugRules cfg "rDiamondmakedbAll" e $ fromRrrPath cfg out
  out' %> \_ -> do
    faPaths <- readPaths cfg ref fasPath
    aSimpleScriptPar "diamond_makedb_all.sh" cfg ref ids (out:faPaths)
  return (ExprPath out')
rDiamondmakedbAll _ e = error $ "bad argument to rDiamondmakedbAll: " ++ show e
 
--------------------
-- diamond_blast* --
--------------------

type DiamondBlastDesc = (String, [String] -> RulesFn, [String], RrrType, RrrType)

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

mkDiamondBlast :: DiamondBlastDesc -> RrrFunction
mkDiamondBlast (name, rFn, dCmd, qType, sType) = let name' = "diamond_" ++ name in RrrFunction
  { fName      = name'
  , fTypeDesc  = mkTypeDesc name' [num, qType, sType] bht 
  , fTypeCheck = defaultTypeCheck [num, qType, sType] bht
  , fDesc      = Just $ "Like " ++ head dCmd ++ ", but uses DIAMOND for speed."
  , fFixity    = Prefix
  , fRules     = rFn dCmd
  }

rDiamondFromDb :: [String] -> RulesFn
rDiamondFromDb = rSimple . aDiamondFromDb

aDiamondFromDb :: [String] -> (RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ())
aDiamondFromDb dCmd cfg ref _ [o, e, q, db] = do
  eStr <- readLit  cfg ref e'
  wrappedCmdWrite True True cfg ref o'' [] [] [] "diamond" $ dCmd ++ ["-q", q', "-o", o'', "-e", eStr, "-d", db']
  where
    o'  = fromRrrPath cfg o
    e'  = fromRrrPath cfg e
    q'  = fromRrrPath cfg q
    db' = fromRrrPath cfg db
    o'' = debugA cfg "aDiamondblastpdb" o' $ dCmd ++ [e', o', q', db']
aDiamondFromDb _ _ _ _ _ = error $ "bad argument to aDiamondFromDb"

-- inserts a "makedb" call and reuses the _db compiler from above
-- based on the version in Blast.hs but a little simpler
rDiamondFromFa :: [String] -> RulesFn
rDiamondFromFa dCmd st (RrrFun rtn salt deps _ [e, q, s])
  = rules st (RrrFun rtn salt deps name1 [e, q, dbExpr])
  where
    rules  = rSimple $ aDiamondFromDb dCmd
    name1  = "diamond_" ++ head dCmd
    dbExpr = RrrFun dmnd salt (depsOf s) "diamond_makedb" [s]
rDiamondFromFa _ _ _ = error "bad argument to rDiamondFromFa"
