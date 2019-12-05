module ShortCut.Modules.Diamond
  where

-- TODO any point in adding a daa type?
-- TODO can't just cat the .dmbd files!

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimpleScriptPar, aSimpleScriptPar,
                                    rExpr, debugRules, rSimple)
import ShortCut.Core.Locks         (withReadLock)
import ShortCut.Core.Util          (resolveSymlinks, headOrDie)
import ShortCut.Core.Paths         (CutPath, fromCutPath, exprPath)
import ShortCut.Core.Actions       (readPaths, readLit, traceA, runCmd, CmdDesc(..), sanitizeFileInPlace)
import ShortCut.Modules.SeqIO      (fna, faa)
import ShortCut.Modules.Blast      (bht)
import System.Process              (readProcess)
import System.Exit                 (ExitCode(..))
import System.FilePath             ((<.>))
import ShortCut.Core.Compile.Map   (rMap, rMapSimpleScript)
import Data.List.Utils             (replace)

cutModule :: CutModule
cutModule = CutModule
  { mName = "Diamond"
  , mDesc = "Accelerated BLAST compatible local sequence aligner."
  , mTypes = [fna, faa, dmnd]
  , mFunctions =
      [ diamondmakedb
      , diamondmakedbEach
      , diamondmakedbAll
      ]
      ++ map mkDiamondBlast variants -- includes the _each ones too
  }

dmnd :: CutType
dmnd = CutType
  { tExt  = "dmnd"
  , tDesc = "DIAMOND database"
  , tShow = \_ ref path -> do
      path' <- resolveSymlinks Nothing path
      out <- withReadLock ref path' $ readProcess "diamond_dbinfo.sh" [path'] []
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
  , fFixity    = Prefix
  , fRules     = rSimpleScriptPar "diamond_makedb.sh"
  }

-------------------------
-- diamond_makedb_each --
-------------------------

-- TODO does this only work when --debug is disabled because of a Map .args issue?

diamondmakedbEach :: CutFunction
diamondmakedbEach = let name = "diamond_makedb_each" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name  [ListOf faa] (ListOf dmnd) 
  , fTypeCheck = defaultTypeCheck [ListOf faa] (ListOf dmnd)
  , fFixity    = Prefix
  , fRules     = rMapSimpleScript 1 "diamond_makedb.sh"
  }
 
------------------------
-- diamond_makedb_all --
------------------------

diamondmakedbAll :: CutFunction
diamondmakedbAll = let name = "diamond_makedb_all" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc name  [ListOf faa] dmnd 
  , fTypeCheck = defaultTypeCheck [ListOf faa] dmnd
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

-- type RulesFn     = CutState -> CutExpr -> Rules ExprPath
-- type ActionFn    = CutConfig -> CacheDir -> [ExprPath] -> Action ()
type ActionFn2 = CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
type DiamondBlastDesc = (String, [String] -> RulesFn, [String], CutType, CutType, CutType)

-- TODO can some of these be replaced by a numeric sensitivity arg?
variants :: [DiamondBlastDesc]
variants =
  [ ("blastp"                       , rDiamondFromFa, ["blastp"                    ], faa, faa , bht)
  , ("blastp_sensitive"             , rDiamondFromFa, ["blastp", "--sensitive"     ], faa, faa , bht)
  , ("blastp_more_sensitive"        , rDiamondFromFa, ["blastp", "--more-sensitive"], faa, faa , bht)
  , ("blastx"                       , rDiamondFromFa, ["blastx"                    ], fna, faa , bht)
  , ("blastx_sensitive"             , rDiamondFromFa, ["blastx", "--sensitive"     ], fna, faa , bht)
  , ("blastx_more_sensitive"        , rDiamondFromFa, ["blastx", "--more-sensitive"], fna, faa , bht)

  , ("blastp_rev"                   , rFlip23 . rDiamondFromFa, ["blastp"                    ], faa, faa , bht)
  , ("blastp_sensitive_rev"         , rFlip23 . rDiamondFromFa, ["blastp", "--sensitive"     ], faa, faa , bht)
  , ("blastp_more_sensitive_rev"    , rFlip23 . rDiamondFromFa, ["blastp", "--more-sensitive"], faa, faa , bht)
  , ("blastx_rev"                   , rFlip23 . rDiamondFromFa, ["blastx"                    ], faa, fna , bht)
  , ("blastx_sensitive_rev"         , rFlip23 . rDiamondFromFa, ["blastx", "--sensitive"     ], faa, fna , bht)
  , ("blastx_more_sensitive_rev"    , rFlip23 . rDiamondFromFa, ["blastx", "--more-sensitive"], faa, fna , bht)

  , ("blastp_db"                    , rSimple . aDiamondFromDb, ["blastp"                    ], faa, dmnd, bht)
  , ("blastp_db_sensitive"          , rSimple . aDiamondFromDb, ["blastp", "--sensitive"     ], faa, dmnd, bht)
  , ("blastp_db_more_sensitive"     , rSimple . aDiamondFromDb, ["blastp", "--more-sensitive"], faa, dmnd, bht)
  , ("blastx_db"                    , rSimple . aDiamondFromDb, ["blastx"                    ], fna, dmnd, bht)
  , ("blastx_db_sensitive"          , rSimple . aDiamondFromDb, ["blastx", "--sensitive"     ], fna, dmnd, bht)
  , ("blastx_db_more_sensitive"     , rSimple . aDiamondFromDb, ["blastx", "--more-sensitive"], fna, dmnd, bht)

  , ("blastp_db_rev"                , rFlip23 . rSimple . aDiamondFromDb, ["blastp"                    ], dmnd, faa, bht)
  , ("blastp_db_sensitive_rev"      , rFlip23 . rSimple . aDiamondFromDb, ["blastp", "--sensitive"     ], dmnd, faa, bht)
  , ("blastp_db_more_sensitive_rev" , rFlip23 . rSimple . aDiamondFromDb, ["blastp", "--more-sensitive"], dmnd, faa, bht)
  , ("blastx_db_rev"                , rFlip23 . rSimple . aDiamondFromDb, ["blastx"                    ], dmnd, fna, bht)
  , ("blastx_db_sensitive_rev"      , rFlip23 . rSimple . aDiamondFromDb, ["blastx", "--sensitive"     ], dmnd, fna, bht)
  , ("blastx_db_more_sensitive_rev" , rFlip23 . rSimple . aDiamondFromDb, ["blastx", "--more-sensitive"], dmnd, fna, bht)

  , ("blastp_each"                  , rDiamondFromFaEach, ["blastp"                    ], faa, ListOf faa , ListOf bht)
  , ("blastp_sensitive_each"        , rDiamondFromFaEach, ["blastp", "--sensitive"     ], faa, ListOf faa , ListOf bht)
  , ("blastp_more_sensitive_each"   , rDiamondFromFaEach, ["blastp", "--more-sensitive"], faa, ListOf faa , ListOf bht)
  , ("blastx_each"                  , rDiamondFromFaEach, ["blastx"                    ], fna, ListOf faa , ListOf bht)
  , ("blastx_sensitive_each"        , rDiamondFromFaEach, ["blastx", "--sensitive"     ], fna, ListOf faa , ListOf bht)
  , ("blastx_more_sensitive_each"   , rDiamondFromFaEach, ["blastx", "--more-sensitive"], fna, ListOf faa , ListOf bht)

  -- TODO will this work with the mapping? test!
  , ("blastp_rev_each"                  , rDiamondFromFaRevEach, ["blastp"                    ], faa, ListOf faa , ListOf bht)
  , ("blastp_sensitive_rev_each"        , rDiamondFromFaRevEach, ["blastp", "--sensitive"     ], faa, ListOf faa , ListOf bht)
  , ("blastp_more_sensitive_rev_each"   , rDiamondFromFaRevEach, ["blastp", "--more-sensitive"], faa, ListOf faa , ListOf bht)
  , ("blastx_rev_each"                  , rDiamondFromFaRevEach, ["blastx"                    ], faa, ListOf fna , ListOf bht)
  , ("blastx_sensitive_rev_each"        , rDiamondFromFaRevEach, ["blastx", "--sensitive"     ], faa, ListOf fna , ListOf bht)
  , ("blastx_more_sensitive_rev_each"   , rDiamondFromFaRevEach, ["blastx", "--more-sensitive"], faa, ListOf fna , ListOf bht)

  , ("blastp_db_each"               , rMap 3 . aDiamondFromDb, ["blastp"                    ], faa, ListOf dmnd, ListOf bht)
  , ("blastp_db_sensitive_each"     , rMap 3 . aDiamondFromDb, ["blastp", "--sensitive"     ], faa, ListOf dmnd, ListOf bht)
  , ("blastp_db_more_sensitive_each", rMap 3 . aDiamondFromDb, ["blastp", "--more-sensitive"], faa, ListOf dmnd, ListOf bht)
  , ("blastx_db_each"               , rMap 3 . aDiamondFromDb, ["blastx"                    ], fna, ListOf dmnd, ListOf bht)
  , ("blastx_db_sensitive_each"     , rMap 3 . aDiamondFromDb, ["blastx", "--sensitive"     ], fna, ListOf dmnd, ListOf bht)
  , ("blastx_db_more_sensitive_each", rMap 3 . aDiamondFromDb, ["blastx", "--more-sensitive"], fna, ListOf dmnd, ListOf bht)

  -- TODO these seem to work, but do they make any sense to include?
  -- , ("blastp_db_rev_each"               , rFlip23 . rMap 2 . aDiamondFromDb, ["blastp"                    ], dmnd, ListOf faa, ListOf bht)
  -- , ("blastp_db_sensitive_rev_each"     , rFlip23 . rMap 2 . aDiamondFromDb, ["blastp", "--sensitive"     ], dmnd, ListOf faa, ListOf bht)
  -- , ("blastp_db_more_sensitive_rev_each", rFlip23 . rMap 2 . aDiamondFromDb, ["blastp", "--more-sensitive"], dmnd, ListOf faa, ListOf bht)
  -- , ("blastx_db_rev_each"               , rFlip23 . rMap 2 . aDiamondFromDb, ["blastx"                    ], dmnd, ListOf fna, ListOf bht)
  -- , ("blastx_db_sensitive_rev_each"     , rFlip23 . rMap 2 . aDiamondFromDb, ["blastx", "--sensitive"     ], dmnd, ListOf fna, ListOf bht)
  -- , ("blastx_db_more_sensitive_rev_each", rFlip23 . rMap 2 . aDiamondFromDb, ["blastx", "--more-sensitive"], dmnd, ListOf fna, ListOf bht)
  ]

-- TODO make into a more general utility?
rFlip23 :: RulesFn -> RulesFn
rFlip23 rFn st (CutFun rtn salt deps ids args) = rFn st (CutFun rtn salt deps ids $ fn args)
  where
    fn (one:two:three:rest) = (one:three:two:rest)
    fn as = error $ "bad argument to rFlip23: " ++ show as
rFlip23 _ _ e = error $ "bad argument to rFlip23: " ++ show e

mkDiamondBlast :: DiamondBlastDesc -> CutFunction
mkDiamondBlast (name, rFn, dCmd, qType, sType, rType) = let name' = "diamond_" ++ name in CutFunction
  { fName      = name'
  , fTypeDesc  = mkTypeDesc name' [num, qType, sType] rType 
  , fTypeCheck = defaultTypeCheck [num, qType, sType] rType
  , fFixity    = Prefix
  , fRules     = rFn dCmd
  }

aDiamondFromDb :: [String] -> ActionFn2
aDiamondFromDb dCmd cfg ref _ [o, e, q, db] = do
  eStr <- readLit  cfg ref e'
  -- wrappedCmdWrite True True cfg ref o'' [] [] [] "diamond.sh" $ [o'', q', eStr, db'] ++ dCmd
  runCmd cfg ref $ CmdDesc
    { cmdBinary = "diamond.sh"
    , cmdArguments = [o'', q', eStr, db'] ++ dCmd
    , cmdFixEmpties = True
    , cmdParallel = False -- TODO fix the resource bug
    , cmdOptions = []
    , cmdInPatterns = []
    , cmdOutPath = o''
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [o'' <.> "out"]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [o'', o'' <.> "out"]
    }
  sanitizeFileInPlace cfg ref o'
  where
    o'  = fromCutPath cfg o
    e'  = fromCutPath cfg e
    q'  = fromCutPath cfg q
    db' = fromCutPath cfg db
    o'' = traceA "aDiamondblastpdb" o' $ dCmd ++ [e', o', q', db']
aDiamondFromDb _ _ _ _ _ = error $ "bad argument to aDiamondFromDb"

-- inserts a "makedb" call and reuses the _db compiler from above
-- based on the version in Blast.hs but a little simpler
rDiamondFromFa :: [String] -> RulesFn
rDiamondFromFa dCmd st (CutFun rtn salt deps _ [e, q, s])
  = rules st (CutFun rtn salt deps name1 [e, q, dbExpr])
  where
    rules  = rSimple $ aDiamondFromDb dCmd
    name1  = "diamond_" ++ headOrDie "failed to parse dCmd in rDiamondFromFa" dCmd
    dbExpr = CutFun dmnd salt (depsOf s) "diamond_makedb" [s]
rDiamondFromFa _ _ _ = fail "bad argument to rDiamondFromFa"

-- same, but inserts a "makedb_each" call and maps over it
rDiamondFromFaEach :: [String] -> RulesFn
rDiamondFromFaEach dCmd st (CutFun rtn salt deps name [e, q, ss])
  = rules st (CutFun rtn salt deps name_db [e, q, dbsExpr])
  where
    rules   = rMap 3 $ aDiamondFromDb dCmd
    name_db = replace "_each" "_db_each" name
    dbsExpr = CutFun (ListOf dmnd) salt (depsOf ss) "diamond_makedb_each" [ss]
rDiamondFromFaEach _ _ _ = fail "bad argument to rDiamondFromFa"

-- TODO this is passing the dmnd and faa args backward to diamond.sh, but why?
-- TODO ah, error is in diamond_makedb_each?
-- TODO proper order of transformations should be:
--        diamond_blastp_rev_each e s qs
--        diamond_blastp_rev_db_each e (diamond_makedb s) qs
--        ...
rDiamondFromFaRevEach :: [String] -> RulesFn
rDiamondFromFaRevEach dCmd st (CutFun rtn salt deps name [e, s, qs])
  = rules st (CutFun rtn salt deps name' [e, dbExpr, qs])
  where
    rules   = rFlip23 . rMap 2 $ aDiamondFromDb dCmd
    -- name1   = "diamond_" ++ headOrDie "failed to parse dCmd in rDiamondFromFa" dCmd ++ "_db_each" -- TODO is this right? get explicitly?
    name'  = replace "_rev_each" "_db_rev_each" name
    -- dbsExpr = CutFun (ListOf dmnd) salt (depsOf ss) "diamond_makedb_each" [ss]
    dbExpr = CutFun dmnd salt (depsOf s) "diamond_makedb" [s]
rDiamondFromFaRevEach _ _ _ = fail "bad argument to rDiamondFromFa"
