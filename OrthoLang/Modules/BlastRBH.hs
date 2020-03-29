module OrthoLang.Modules.BlastRBH where

import Development.Shake
import OrthoLang.Core.Types

import OrthoLang.Core.Compile (rExpr, defaultTypeCheck, debug)
import OrthoLang.Core.Compile (rSimple, aSimpleScriptNoFix)
import OrthoLang.Core.Compile  (rMap)
import OrthoLang.Core.Actions       (runCmd, CmdDesc(..), traceA, absolutizePaths)
-- import OrthoLang.Core.Debug         (traceA)
import OrthoLang.Core.Paths         (Path, toPath, fromPath, cacheDir)
import OrthoLang.Core.Util          (digest)
import OrthoLang.Modules.Blast      (bht, BlastDesc, blastDescs, mkBlastFromFa,
                                    aMkBlastFromDb)
import OrthoLang.Modules.BlastDB    (ndb, pdb)
import OrthoLang.Modules.SeqIO      (faa)
import System.Exit                 (ExitCode(..))
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             ((</>), (<.>))
import Data.List.Utils             (replace)

-- TODO should the _rev functions also be moved here?
-- TODO test each one: first all the peices, then together

-- for tracking down non-deduplicating blastp functions
debugNames :: Config -> String -> Expr -> Expr -> a -> a
debugNames cfg fnName (Fun _ _ _ bname _) (Fun _ _ _ aname _) rtn = debug cfg fnName msg rtn
  where
    msg = "\"" ++ bname ++ "' -> \"" ++ aname ++ "\""
debugNames _ fnName _ _ _ = error $ "bad argument to debugNames from " ++ fnName

orthoLangModule :: Module
orthoLangModule = Module
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

mkBlastFromFaRev :: BlastDesc -> Function
mkBlastFromFaRev d@(bCmd, qType, sType, _) = let name = bCmd ++ "_rev" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [num, sType, qType] bht
  , fTypeDesc  = mkTypeDesc name  [num, sType, qType] bht
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMkBlastFromFaRev d
  }

-- flips the query and subject arguments and reuses the regular compiler above
rMkBlastFromFaRev :: BlastDesc -> RulesFn
rMkBlastFromFaRev d st (Fun rtn salt deps name [e, q, s]) = rules st expr
  where
    expr  = Fun rtn salt deps name_norev [e, s, q]
    rules = fOldRules $ mkBlastFromFa d
    name_norev  = replace "_rev" "" name
    -- name_norev' = debugNames cfg "rMkBlastFromFaRev" b expr name_norev
rMkBlastFromFaRev _ _ _ = fail "bad argument to rMkBlastFromFaRev"

----------------------
-- *blast*_rev_each --
----------------------

-- TODO fix expression paths!
mkBlastFromFaRevEach :: BlastDesc -> Function
mkBlastFromFaRevEach d@(bCmd, sType, qType, _) = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [num, sType, ListOf qType] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, sType, ListOf qType] (ListOf bht)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMkBlastFromFaRevEach d
  }
  where
    name = bCmd ++ "_rev_each"

-- The most confusing one! Edits the expression to make the subject into a db,
-- and the action fn to take the query and subject flipped, then maps the new
-- expression over the new action fn.
-- TODO check if all this is right, since it's confusing!
rMkBlastFromFaRevEach :: BlastDesc -> RulesFn
rMkBlastFromFaRevEach (bCmd, qType, _, _) st (Fun rtn salt deps _ [e, s, qs])
  = rMap 3 revDbAct st editedExpr
  where
    revDbAct   = aMkBlastFromDbRev bCmd
    sList      = Lst (typeOf s) salt (depsOf s) [s]
    subjDbExpr = Fun dbType salt (depsOf sList) dbFnName [sList]
    editedExpr = Fun rtn salt deps editedName [e, subjDbExpr, qs]
    editedName = bCmd ++ "_db_each" -- TODO is this right? i think so now
    (dbFnName, dbType) = if qType == faa
                           then ("makeblastdb_prot_all", pdb) -- TODO use non _all version?
                           else ("makeblastdb_nucl_all", ndb) -- TODO use non _all version?
rMkBlastFromFaRevEach _ _ _ = fail "bad argument to rMkBlastFromFaRevEach"

-- TODO which blast commands make sense with this?
-- TODO is it deduplicating properly with the fn name?
aMkBlastFromDbRev :: String -> (Config -> LocksRef -> IDsRef -> [Path] -> Action ())
aMkBlastFromDbRev bCmd cfg ref ids [oPath, eValue, dbPrefix, queryFa] =
  aMkBlastFromDb  bCmd cfg ref ids [oPath, eValue, queryFa, dbPrefix]
aMkBlastFromDbRev _ _ _ _ _ = fail "bad argument to aMkBlastFromDbRev"

---------------------
-- reciprocal_best --
---------------------

-- TODO move to Tables.hs? And rename that to BlastHits?

reciprocalBest :: Function
reciprocalBest = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [bht, bht] bht
  , fTypeDesc  = mkTypeDesc name  [bht, bht] bht
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple aReciprocalBest
  }
  where
    name = "reciprocal_best"

-- TODO how are $TMPDIR paths getting through after conversion from cutpaths??
-- TODO why is this the only one that fails, and only when called from repeat??
aReciprocalBest :: Config -> LocksRef -> IDsRef -> [Path] -> Action ()
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
    out'   = fromPath cfg out
    left'  = fromPath cfg left
    right' = fromPath cfg right
    out''  = traceA "aReciprocalBest" out' [out', left', right']
aReciprocalBest _ _ _ args = error $ "bad argument to aReciprocalBest: " ++ show args

--------------------------
-- reciprocal_best_each --
--------------------------

reciprocalBestAll :: Function
reciprocalBestAll = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [ListOf bht, ListOf bht] bht
  , fTypeDesc  = mkTypeDesc name  [ListOf bht, ListOf bht] bht
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple aReciprocalBestAll
  }
  where
    name = "reciprocal_best_all"

aReciprocalBestAll :: Config -> LocksRef -> IDsRef -> [Path] -> Action ()
aReciprocalBestAll cfg ref ids (out:ins) = do
  let cDir = fromPath cfg $ cacheDir cfg "blastrbh"
      tmpPath p = cDir </> digest p <.> "bht"
      ins' = map (\p -> (p, tmpPath p)) $ map (fromPath cfg) ins
  liftIO $ createDirectoryIfMissing True cDir
  mapM_ (\(inPath, outPath) -> absolutizePaths cfg ref inPath outPath) ins'
  aSimpleScriptNoFix "reciprocal_best_all.R" cfg ref ids (out:map (toPath cfg . snd) ins')
aReciprocalBestAll _ _ _ ps = error $ "bad argument to aReciprocalBestAll: " ++ show ps

-----------------
-- *blast*_rbh --
-----------------

mkBlastRbh :: BlastDesc -> Function
mkBlastRbh d@(bCmd, qType, sType, _) = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [num, qType, sType] bht
  , fTypeDesc  = mkTypeDesc name  [num, qType, sType] bht
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMkBlastRbh d
  }
  where
    name = bCmd ++ "_rbh"

-- TODO this only works with symmetric fns so far... either fix or restrict to those!
rMkBlastRbh :: BlastDesc -> RulesFn
rMkBlastRbh (bCmd, _, _, _) s (Fun _ salt deps _ [e, l, r]) = rExpr s main
  where
    main  = Fun bht salt deps "reciprocal_best" [lHits, rHits]
    lHits = Fun bht salt deps  bCmd            [e, l, r]
    rHits = Fun bht salt deps (bCmd ++ "_rev") [e, l, r]
rMkBlastRbh _ _ _ = fail "bad argument to rMkBlastRbh"

----------------------
-- *blast*_rbh_each --
----------------------

mkBlastRbhEach :: BlastDesc -> Function
mkBlastRbhEach d@(bCmd, qType, sType, _) = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [num, qType, ListOf sType] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, qType, ListOf sType] (ListOf bht)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMkBlastRbhEach d
  }
  where
    name = bCmd ++ "_rbh_each"

rMkBlastRbhEach :: BlastDesc -> RulesFn
rMkBlastRbhEach (bCmd, _, _, _) s (Fun _ salt deps _ [e, l, rs]) = rExpr s main
  where
    main  = Fun (ListOf bht) salt deps "reciprocal_best_each" [lHits, rHits]
    lHits = Fun (ListOf bht) salt deps (bCmd ++ "_each"    )  [e, l, rs]
    rHits = Fun (ListOf bht) salt deps (bCmd ++ "_rev_each")  [e, l, rs]
rMkBlastRbhEach _ _ _ = fail "bad argument to rMkBlastRbh"
