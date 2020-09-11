module OrthoLang.Modules.BlastRBH where

import Development.Shake

import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Interpreter.Compile as C
import OrthoLang.Modules.Blast   (bht, BlastDesc, blastDescs, aMkBlastFromDb)
import OrthoLang.Modules.BlastDB (blastdb)
import OrthoLang.Modules.SeqIO   (fna, faa)

import Data.List.Utils           (replace)
import System.Directory          (createDirectoryIfMissing)
import System.Exit               (ExitCode(..))
import System.FilePath           ((</>), (<.>))
import Data.Maybe (fromJust)

-- TODO should the _rev functions also be moved here?
-- TODO test each one: first all the peices, then together

-- for tracking down non-deduplicating blastp functions
debugNames :: String -> Expr -> Expr -> a -> a
debugNames fnName (Fun _ _ _ bname _) (Fun _ _ _ aname _) rtn = C.debugC fnName msg rtn
  where
    msg = "\"" ++ bname ++ "' -> \"" ++ aname ++ "\""
debugNames fnName _ _ _ = error $ "bad argument to debugNames from " ++ fnName

olModule :: Module
olModule = Module
  { mName = "BlastRBH"
  , mDesc = "Reciprocal BLAST+ best hits"
  , mTypes = [fna, faa, bht]
  , mGroups = []
  , mEncodings = [blastdb]
  , mRules = []
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
mkBlastFromFaRev (bCmd, qType, sType, _) = newExprExpansion
  (bCmd ++ "_rev")
  [Exactly num, Exactly sType, Exactly qType]
  (Exactly bht)
  mBlastFromFaRev
  []

-- flips the query and subject arguments and reuses the regular compiler above
mBlastFromFaRev :: ExprExpansion
mBlastFromFaRev _ _ (Fun r ms ds n [e, q, s]) = Fun r ms ds (replace "_rev" "" n) [e, s, q]
mBlastFromFaRev _ _ e = error "ortholang.modules.blastrbh.mBlastFromFaRev" $ "bad argument: " ++ show e

----------------------
-- *blast*_rev_each --
----------------------

-- TODO fix expression paths!
mkBlastFromFaRevEach :: BlastDesc -> Function
mkBlastFromFaRevEach d@(bCmd, sType, qType, _) = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly num, Exactly sType, Exactly (ListOf qType)]
  , fOutput = Exactly (ListOf bht)
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMkBlastFromFaRevEach d
  }
  where
    name = bCmd ++ "_rev_each"

-- The most confusing one! Edits the expression to make the subject into a db,
-- and the action fn to take the query and subject flipped, then maps the new
-- expression over the new action fn.
-- TODO check if all this is right, since it's confusing!
rMkBlastFromFaRevEach :: BlastDesc -> RulesFn
rMkBlastFromFaRevEach (bCmd, qType, _, _) scr (Fun rtn seed deps _ [e, s, qs]) = do
  let revDbAct   = aMkBlastFromDbRev bCmd
      sList      = Lst (typeOf s) (seedOf s) (depsOf s) [s]
      subjDbExpr = Fun dbType seed (depsOf sList) dbFnName [sList]
      editedExpr = Fun rtn seed deps editedName [e, subjDbExpr, qs]
      editedName = bCmd ++ "_db_each" -- TODO is this right? i think so now
      dbFnName   = "makeblastdb_" ++ ext qType ++ "_all"
      dbType     = EncodedAs blastdb qType
  rMap 3 revDbAct scr editedExpr
rMkBlastFromFaRevEach _ _ _ = fail "bad argument to rMkBlastFromFaRevEach"

-- TODO which blast commands make sense with this?
-- TODO is it deduplicating properly with the fn name?
aMkBlastFromDbRev :: String -> ([Path] -> Action ())
aMkBlastFromDbRev bCmd [oPath, eValue, dbPrefix, queryFa] =
  aMkBlastFromDb  bCmd [oPath, eValue, queryFa, dbPrefix]
aMkBlastFromDbRev _ _ = fail "bad argument to aMkBlastFromDbRev"

---------------------
-- reciprocal_best --
---------------------

reciprocalBest :: Function
reciprocalBest = newFnA2
  "reciprocal_best"
  (Exactly bht, Exactly bht)
  (Exactly bht)
  aReciprocalBest
  []

-- TODO how are $TMPDIR paths getting through after conversion from cutpaths??
-- TODO why is this the only one that fails, and only when called from repeat??
aReciprocalBest :: NewAction2
aReciprocalBest (ExprPath out') left' right' = do
  let loc   = "modules.blastrbh.aReciprocalBest"
      out'' = traceA loc out' [out', left', right']
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [left', right']
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "reciprocal_best.R"
    , cmdArguments = [out', left', right']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out']
    }

-------------------------
-- reciprocal_best_all --
-------------------------

reciprocalBestAll :: Function
reciprocalBestAll = newFnA2
  "reciprocal_best_all"
  (Exactly $ ListOf bht, Exactly $ ListOf bht)
  (Exactly bht)
  aReciprocalBestAll
  []

aReciprocalBestAll :: NewAction2
aReciprocalBestAll (ExprPath out') lefts' rights' = do
  cfg <- fmap fromJust getShakeExtra
  let cDir = fromPath loc cfg $ cacheDir cfg "blastrbh"
      loc = "modules.blastrbh.aReciprocalBestAll"
      tmpPath p = cDir </> digest loc p <.> "bht"
      ins' = map (\p -> (p, tmpPath p)) [lefts', rights']
      ins  = map (toPath loc cfg . snd) ins'
      out  = toPath loc cfg out'
  liftIO $ createDirectoryIfMissing True cDir
  mapM_ (\(inPath, outPath) -> absolutizePaths loc inPath outPath) ins'
  aSimpleScriptNoFix "reciprocal_best_all.R" (out:ins)

-----------------
-- *blast*_rbh --
-----------------

mkBlastRbh :: BlastDesc -> Function
mkBlastRbh d@(bCmd, qType, sType, _) = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly num, Exactly qType, Exactly sType]
  , fOutput = Exactly bht
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMkBlastRbh d
  }
  where
    name = bCmd ++ "_rbh"

-- TODO this only works with symmetric fns so far... either fix or restrict to those!
rMkBlastRbh :: BlastDesc -> RulesFn
rMkBlastRbh (bCmd, _, _, _) s (Fun _ seed deps _ [e, l, r]) = rExpr s main
  where
    main  = Fun bht seed deps "reciprocal_best" [lHits, rHits]
    lHits = Fun bht seed deps  bCmd            [e, l, r]
    rHits = Fun bht seed deps (bCmd ++ "_rev") [e, l, r]
rMkBlastRbh _ _ _ = fail "bad argument to rMkBlastRbh"

----------------------
-- *blast*_rbh_each --
----------------------

mkBlastRbhEach :: BlastDesc -> Function
mkBlastRbhEach d@(bCmd, qType, sType, _) = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly num, Exactly qType, Exactly (ListOf sType)] -- TODO any ht would work right?
  , fOutput = Exactly (ListOf bht)
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMkBlastRbhEach d
  }
  where
    name = bCmd ++ "_rbh_each"

rMkBlastRbhEach :: BlastDesc -> RulesFn
rMkBlastRbhEach (bCmd, _, _, _) s (Fun _ seed deps _ [e, l, rs]) = rExpr s main
  where
    main  = Fun (ListOf bht) seed deps "reciprocal_best_each" [lHits, rHits]
    lHits = Fun (ListOf bht) seed deps (bCmd ++ "_each"    )  [e, l, rs]
    rHits = Fun (ListOf bht) seed deps (bCmd ++ "_rev_each")  [e, l, rs]
rMkBlastRbhEach _ _ _ = fail "bad argument to rMkBlastRbh"
