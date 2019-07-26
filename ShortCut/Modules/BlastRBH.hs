module ShortCut.Modules.BlastRBH where

-- TODO ncbi_blast_blastp_rbh.cut looks like it works, so this must be an issue with the _each versions only?
--      yeah, blastn_db_rev does it too. and blastn_db_rev_each
--      but run by itself blastn_rev works properly: only the _each version is written
-- TODO hold up, blastn is making separate tables from blastn_db when run via blastn_each too
-- TODO so are both these broken?
--      blastn_rev_each broken
--      blastn_each     broken (but this is in Blast.hs)

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile.Basic (rExpr, rSimple, defaultTypeCheck, aSimpleScriptNoFix, debug)
import ShortCut.Core.Compile.Map  (rMap)
import ShortCut.Core.Actions       (runCmd, CmdDesc(..), debugA, absolutizePaths)
-- import ShortCut.Core.Debug         (debugA)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath, cacheDir)
import ShortCut.Core.Util          (digest)
import ShortCut.Modules.Blast      (bht, BlastDesc, blastDescs, mkBlastFromFa,
                                    aMkBlastFromDb)
import ShortCut.Modules.BlastDB    (ndb, pdb)
import ShortCut.Modules.SeqIO      (faa)
import System.Exit                 (ExitCode(..))
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             ((</>), (<.>))
import Data.List.Utils             (replace)

-- TODO should the _rev functions also be moved here?
-- TODO test each one: first all the peices, then together

-- for tracking down non-deduplicating blastp functions
debugNames :: CutConfig -> String -> CutExpr -> CutExpr -> a -> a
debugNames cfg fnName (CutFun _ _ _ bname _) (CutFun _ _ _ aname _) rtn = debug cfg msg rtn
  where
    msg = fnName ++ " translated " ++ bname ++ " -> " ++ aname
debugNames _ fnName _ _ _ = error $ "bad argument to debugNames from " ++ fnName

cutModule :: CutModule
cutModule = CutModule
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

mkBlastFromFaRev :: BlastDesc -> CutFunction
mkBlastFromFaRev d@(bCmd, qType, sType, _) = let name = bCmd ++ "_rev" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, sType, qType] bht
  , fTypeDesc  = mkTypeDesc name  [num, sType, qType] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastFromFaRev d
  }

-- flips the query and subject arguments and reuses the regular compiler above
rMkBlastFromFaRev :: BlastDesc -> RulesFn
rMkBlastFromFaRev d st@(_, cfg, _, _) b@(CutFun rtn salt deps name [e, q, s]) = rules st expr
  where
    expr = (CutFun rtn salt deps name_norev [e, s, q])
    rules = fRules $ mkBlastFromFa d
    name_norev = replace "_rev" "" name
    name_norev' = debugNames cfg "rMkBlastFromFaRev" b expr name_norev
rMkBlastFromFaRev _ _ _ = fail "bad argument to rMkBlastFromFaRev"

----------------------
-- *blast*_rev_each --
----------------------

-- TODO fix expression paths!
mkBlastFromFaRevEach :: BlastDesc -> CutFunction
mkBlastFromFaRevEach d@(bCmd, sType, qType, _) = CutFunction
  { fName      = name
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
rMkBlastFromFaRevEach (bCmd, qType, _, _) st (CutFun rtn salt deps _ [e, s, qs])
  = rMap 3 revDbAct st editedExpr
  where
    revDbAct   = aMkBlastFromDbRev bCmd
    sList      = CutList (typeOf s) salt (depsOf s) [s]
    subjDbExpr = CutFun dbType salt (depsOf sList) dbFnName [sList]
    editedExpr = CutFun rtn salt deps editedName [e, subjDbExpr, qs]
    editedName = bCmd ++ "_db_each" -- TODO is this right? i think so now
    (dbFnName, dbType) = if qType == faa
                           then ("makeblastdb_prot_all", pdb) -- TODO use non _all version?
                           else ("makeblastdb_nucl_all", ndb) -- TODO use non _all version?
rMkBlastFromFaRevEach _ _ _ = fail "bad argument to rMkBlastFromFaRevEach"

-- TODO which blast commands make sense with this?
-- TODO is this deduplicating properly with the fn name?
aMkBlastFromDbRev :: String -> (CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ())
aMkBlastFromDbRev bCmd cfg ref ids [oPath, eValue, dbPrefix, queryFa] =
  aMkBlastFromDb  bCmd cfg ref ids [oPath, eValue, queryFa, dbPrefix]
aMkBlastFromDbRev _ _ _ _ _ = fail "bad argument to aMkBlastFromDbRev"

---------------------
-- reciprocal_best --
---------------------

-- TODO move to Tables.hs? And rename that to BlastHits?

reciprocalBest :: CutFunction
reciprocalBest = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [bht, bht] bht
  , fTypeDesc  = mkTypeDesc name  [bht, bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aReciprocalBest
  }
  where
    name = "reciprocal_best"

-- TODO how are $TMPDIR paths getting through after conversion from cutpaths??
-- TODO why is this the only one that fails, and only when called from repeat??
aReciprocalBest :: CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
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
    out'   = fromCutPath cfg out
    left'  = fromCutPath cfg left
    right' = fromCutPath cfg right
    out''  = debugA cfg "aReciprocalBest" out' [out', left', right']
aReciprocalBest _ _ _ args = error $ "bad argument to aReciprocalBest: " ++ show args

--------------------------
-- reciprocal_best_each --
--------------------------

reciprocalBestAll :: CutFunction
reciprocalBestAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf bht, ListOf bht] bht
  , fTypeDesc  = mkTypeDesc name  [ListOf bht, ListOf bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aReciprocalBestAll
  }
  where
    name = "reciprocal_best_all"

aReciprocalBestAll :: CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
aReciprocalBestAll cfg ref ids (out:ins) = do
  let cDir = fromCutPath cfg $ cacheDir cfg "blastrbh"
      tmpPath p = cDir </> digest p <.> "bht"
      ins' = map (\p -> (p, tmpPath p)) $ map (fromCutPath cfg) ins
  liftIO $ createDirectoryIfMissing True cDir
  mapM_ (\(inPath, outPath) -> absolutizePaths cfg ref inPath outPath) ins'
  aSimpleScriptNoFix "reciprocal_best_all.R" cfg ref ids (out:map (toCutPath cfg . snd) ins')
aReciprocalBestAll _ _ _ ps = error $ "bad argument to aReciprocalBestAll: " ++ show ps

-----------------
-- *blast*_rbh --
-----------------

mkBlastRbh :: BlastDesc -> CutFunction
mkBlastRbh d@(bCmd, qType, sType, _) = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, qType, sType] bht
  , fTypeDesc  = mkTypeDesc name  [num, qType, sType] bht
  , fFixity    = Prefix
  , fRules     = rMkBlastRbh d
  }
  where
    name = bCmd ++ "_rbh"

-- TODO this only works with symmetric fns so far... either fix or restrict to those!
rMkBlastRbh :: BlastDesc -> RulesFn
rMkBlastRbh (bCmd, _, _, _) s (CutFun _ salt deps _ [e, l, r]) = rExpr s main
  where
    main  = CutFun bht salt deps "reciprocal_best" [lHits, rHits]
    lHits = CutFun bht salt deps  bCmd            [e, l, r]
    rHits = CutFun bht salt deps (bCmd ++ "_rev") [e, l, r]
rMkBlastRbh _ _ _ = fail "bad argument to rMkBlastRbh"

----------------------
-- *blast*_rbh_each --
----------------------

mkBlastRbhEach :: BlastDesc -> CutFunction
mkBlastRbhEach d@(bCmd, qType, sType, _) = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, qType, ListOf sType] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMkBlastRbhEach d
  }
  where
    name = bCmd ++ "_rbh_each"

rMkBlastRbhEach :: BlastDesc -> RulesFn
rMkBlastRbhEach (bCmd, _, _, _) s (CutFun _ salt deps _ [e, l, rs]) = rExpr s main
  where
    main  = CutFun (ListOf bht) salt deps "reciprocal_best_each" [lHits, rHits]
    lHits = CutFun (ListOf bht) salt deps (bCmd ++ "_each"    )  [e, l, rs]
    rHits = CutFun (ListOf bht) salt deps (bCmd ++ "_rev_each")  [e, l, rs]
rMkBlastRbhEach _ _ _ = fail "bad argument to rMkBlastRbh"
