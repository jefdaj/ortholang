module OrthoLang.Modules.BlastRBH where

import Development.Shake
import OrthoLang.Core.Types

import OrthoLang.Core.Compile.Basic (rExpr, rSimple, defaultTypeCheck, aSimpleScriptNoFix)
import OrthoLang.Core.Compile.Map  (rMap)
import OrthoLang.Core.Actions       (runCmd, CmdDesc(..), traceA, absolutizePaths)
-- import OrthoLang.Core.Debug         (traceA)
import OrthoLang.Core.Paths         (OrthoLangPath, toOrthoLangPath, fromOrthoLangPath, cacheDir)
import OrthoLang.Core.Util          (digest)
import OrthoLang.Modules.Blast      (bht, BlastDesc, blastDescs, mkBlastFromFa,
                                    aMkBlastFromDb)
import OrthoLang.Modules.BlastDB    (ndb, pdb)
import OrthoLang.Modules.SeqIO      (faa)
import System.Exit                 (ExitCode(..))
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             ((</>), (<.>))

-- TODO should the _rev functions also be moved here?
-- TODO test each one: first all the peices, then together

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "BlastRBH"
  , mDesc = "Reciprocal BLAST+ best hits"
  , mTypes = [faa, ndb, pdb, bht]
  , mFunctions =
    -- TODO also work with the non-symmetric ones that have an obvious way to do it?
    map mkBlastFromFaRev     blastDescsRev ++
    map mkBlastFromFaRevEach blastDescsRev ++

    [reciprocalBest, reciprocalBestAll] ++

    map mkBlastRbh     blastDescsRev ++
    map mkBlastRbhEach blastDescsRev
  }

-- note that this just filters. it doesn't "reverse" the descriptions
blastDescsRev :: [BlastDesc]
blastDescsRev = filter isReversible blastDescs
  where
    isReversible (_, qType, sType, _) = qType == sType

-----------------
-- *blast*_rev --
-----------------

mkBlastFromFaRev :: BlastDesc -> OrthoLangFunction
mkBlastFromFaRev d@(bCmd, qType, sType, _) = let name = bCmd ++ "_rev" in OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [num, sType, qType] bht
  , fTypeDesc  = mkTypeDesc name  [num, sType, qType] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastFromFaRev d
  }

-- flips the query and subject arguments and reuses the regular compiler above
rMkBlastFromFaRev :: BlastDesc -> RulesFn
rMkBlastFromFaRev d st (OrthoLangFun rtn salt deps _ [e, q, s])
  = rules st (OrthoLangFun rtn salt deps name [e, s, q])
  where
    rules = fRules $ mkBlastFromFa d
    name  = head $ fNames $ mkBlastFromFa d
rMkBlastFromFaRev _ _ _ = fail "bad argument to rMkBlastFromFaRev"

----------------------
-- *blast*_rev_each --
----------------------

mkBlastFromFaRevEach :: BlastDesc -> OrthoLangFunction
mkBlastFromFaRevEach d@(bCmd, sType, qType, _) = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [num, sType, ListOf qType] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, sType, ListOf qType] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMkBlastFromFaRevEach d
  }
  where
    name = bCmd ++ "_rev_each"

-- The most confusing one! Edits the expression to make the subject into a db,
-- and the action fn to take the query and subject flipped, then maps the new
-- expression over the new action fn.
-- TODO check if all this is right, since it's confusing!
rMkBlastFromFaRevEach :: BlastDesc -> RulesFn
rMkBlastFromFaRevEach (bCmd, qType, _, _) st (OrthoLangFun rtn salt deps _ [e, s, qs])
  = rMap 3 revDbAct st editedExpr
  where
    revDbAct   = aMkBlastFromDbRev bCmd
    sList      = OrthoLangList (typeOf s) salt (depsOf s) [s]
    subjDbExpr = OrthoLangFun dbType salt (depsOf sList) dbFnName [sList]
    editedExpr = OrthoLangFun rtn salt deps editedName [e, subjDbExpr, qs]
    editedName = bCmd ++ "_db_rev_each"
    (dbFnName, dbType) = if qType == faa
                           then ("makeblastdb_prot_all", pdb) -- TODO use non _all version?
                           else ("makeblastdb_nucl_all", ndb) -- TODO use non _all version?
rMkBlastFromFaRevEach _ _ _ = fail "bad argument to rMkBlastFromFaRevEach"

-- TODO which blast commands make sense with this?
aMkBlastFromDbRev :: String -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
aMkBlastFromDbRev bCmd cfg ref ids [oPath, eValue, dbPrefix, queryFa] =
  aMkBlastFromDb  bCmd cfg ref ids [oPath, eValue, queryFa, dbPrefix]
aMkBlastFromDbRev _ _ _ _ _ = fail "bad argument to aMkBlastFromDbRev"

---------------------
-- reciprocal_best --
---------------------

-- TODO move to Tables.hs? And rename that to BlastHits?

reciprocalBest :: OrthoLangFunction
reciprocalBest = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [bht, bht] bht
  , fTypeDesc  = mkTypeDesc name  [bht, bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aReciprocalBest
  }
  where
    name = "reciprocal_best"

-- TODO how are $TMPDIR paths getting through after conversion from cutpaths??
-- TODO why is this the only one that fails, and only when called from repeat??
aReciprocalBest :: OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ()
aReciprocalBest cfg ref _ [out, left, right] = do
  runCmd cfg ref $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [left', right']
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "reciprocal_best.R"
    , cmdArguments = [out', left', right']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out']
    }
  where
    out'   = fromOrthoLangPath cfg out
    left'  = fromOrthoLangPath cfg left
    right' = fromOrthoLangPath cfg right
    out''  = traceA "aReciprocalBest" out' [out', left', right']
aReciprocalBest _ _ _ args = error $ "bad argument to aReciprocalBest: " ++ show args

--------------------------
-- reciprocal_best_each --
--------------------------

reciprocalBestAll :: OrthoLangFunction
reciprocalBestAll = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [ListOf bht, ListOf bht] bht
  , fTypeDesc  = mkTypeDesc name  [ListOf bht, ListOf bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aReciprocalBestAll
  }
  where
    name = "reciprocal_best_all"

aReciprocalBestAll :: OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ()
aReciprocalBestAll cfg ref ids (out:ins) = do
  let cDir = fromOrthoLangPath cfg $ cacheDir cfg "blastrbh"
      tmpPath p = cDir </> digest p <.> "bht"
      ins' = map (\p -> (p, tmpPath p)) $ map (fromOrthoLangPath cfg) ins
  liftIO $ createDirectoryIfMissing True cDir
  mapM_ (\(inPath, outPath) -> absolutizePaths cfg ref inPath outPath) ins'
  aSimpleScriptNoFix "reciprocal_best_all.R" cfg ref ids (out:map (toOrthoLangPath cfg . snd) ins')
aReciprocalBestAll _ _ _ ps = error $ "bad argument to aReciprocalBestAll: " ++ show ps

-----------------
-- *blast*_rbh --
-----------------

mkBlastRbh :: BlastDesc -> OrthoLangFunction
mkBlastRbh d@(bCmd, qType, sType, _) = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [num, qType, sType] bht
  , fTypeDesc  = mkTypeDesc name  [num, qType, sType] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastRbh d
  }
  where
    name = bCmd ++ "_rbh"

-- TODO this only works with symmetric fns so far... either fix or restrict to those!
rMkBlastRbh :: BlastDesc -> RulesFn
rMkBlastRbh (bCmd, _, _, _) s (OrthoLangFun _ salt deps _ [e, l, r]) = rExpr s main
  where
    main  = OrthoLangFun bht salt deps "reciprocal_best" [lHits, rHits]
    lHits = OrthoLangFun bht salt deps  bCmd            [e, l, r]
    rHits = OrthoLangFun bht salt deps (bCmd ++ "_rev") [e, l, r]
rMkBlastRbh _ _ _ = fail "bad argument to rMkBlastRbh"

----------------------
-- *blast*_rbh_each --
----------------------

mkBlastRbhEach :: BlastDesc -> OrthoLangFunction
mkBlastRbhEach d@(bCmd, qType, sType, _) = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, qType, ListOf sType] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMkBlastRbhEach d
  }
  where
    name = bCmd ++ "_rbh_each"

rMkBlastRbhEach :: BlastDesc -> RulesFn
rMkBlastRbhEach (bCmd, _, _, _) s (OrthoLangFun _ salt deps _ [e, l, rs]) = rExpr s main
  where
    main  = OrthoLangFun (ListOf bht) salt deps "reciprocal_best_each" [lHits, rHits]
    lHits = OrthoLangFun (ListOf bht) salt deps (bCmd ++ "_each"    )  [e, l, rs]
    rHits = OrthoLangFun (ListOf bht) salt deps (bCmd ++ "_rev_each")  [e, l, rs]
rMkBlastRbhEach _ _ _ = fail "bad argument to rMkBlastRbh"
