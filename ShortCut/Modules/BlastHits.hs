module ShortCut.Modules.BlastHits where

-- TODO are crb files *exactly* in blast format? if so, no need for separate types
-- TODO rename all extract_ functions to not have the extract part? or replace it with list_?

import Development.Shake
import ShortCut.Core.Types

import System.FilePath             ((<.>))
import ShortCut.Core.Util          (unlessExists)
import ShortCut.Core.Compile.Basic (rSimple, defaultTypeCheck)
import ShortCut.Core.Compile.Map  (rMap)
import ShortCut.Core.Actions       (runCmd, CmdDesc(..), debugA, symlink)
-- import ShortCut.Core.Debug         (debugA )
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath)
import ShortCut.Modules.Blast      (bht)
import ShortCut.Modules.CRBBlast   (crb)
import System.Exit                 (ExitCode(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "BlastHits"
  , mDesc = "Work with BLAST hit tables"
  , mTypes = [bht, crb, hittable]
  , mFunctions =
    [ extractQueries, extractQueriesEach
    , extractTargets, extractTargetsEach
    , filterEvalue  , filterEvalueEach
    , bestHits      , bestHitsEach
    ]
  }

hittable :: CutType
hittable = CutTypeGroup
  { tgShort = "hittable"
  , tgLong  = "files in "
  , tgMember = \t -> t `elem` [bht, crb] -- TODO mms too
  }

----------------------
-- extract_*(_each) --
----------------------

-- tExtract :: TypeChecker
-- tExtract [x] | elem x [crb, bht] = Right $ ListOf str
-- tExtract  _ = Left "expected a blast hits table"

-- tExtractEach :: [CutType] -> Either String CutType
-- tExtractEach [ListOf x] | elem x [crb, bht] = Right $ ListOf $ ListOf str
-- tExtractEach  _ = Left "expected a list of blast hits tables"

extractQueries :: CutFunction
extractQueries = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [hittable] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [hittable] (ListOf str)
  , fDesc = Nothing
  , fFixity    = Prefix
  , fRules     = rSimple $ aCutCol True 1
  }
  where
    name = "extract_queries"

-- TODO this should have a typeclass
extractQueriesEach :: CutFunction
extractQueriesEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf hittable] (ListOf (ListOf str))
  , fTypeDesc  = mkTypeDesc name  [ListOf hittable] (ListOf (ListOf str))
  , fDesc = Nothing
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aCutCol True 1
  }
  where
    name = "extract_queries_each"

-- TODO this should have a typeclass
extractTargets :: CutFunction
extractTargets = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [hittable] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [hittable] (ListOf str)
  , fDesc = Nothing
  , fFixity    = Prefix
  , fRules     = rSimple $ aCutCol True 2
  }
  where
    name = "extract_targets"

extractTargetsEach :: CutFunction
extractTargetsEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf hittable] (ListOf (ListOf str))
  , fTypeDesc  = mkTypeDesc name  [ListOf hittable] (ListOf (ListOf str))
  , fDesc = Nothing
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aCutCol True 2
  }
  where
    name = "extract_targets_each"

-- TODO remove uniq, unless it's used somewhere?
aCutCol :: Bool -> Int -> CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aCutCol _ n cfg ref _ [outPath, tsvPath] = do
  runCmd cfg ref $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = outPath''
    , cmdInPatterns = [tsvPath']
    , cmdExtraOutPaths = []
    , cmdOptions =[]
    , cmdBinary = "cut_tsv.sh"
    , cmdArguments = [outPath', tsvPath', show n]
    , cmdExitCode = ExitSuccess
    }

  -- TODO remove this? why does it need to be here at all?
  -- let outOut = outPath' <.> "out"
  -- unlessExists outOut $ do
  --   symlink cfg ref outPath $ toCutPath cfg outOut

  where
    outPath'  = fromCutPath cfg outPath
    outPath'' = debugA cfg "aCutCol" outPath' [show n, outPath', tsvPath']
    tsvPath'  = fromCutPath cfg tsvPath
aCutCol _ _ _ _ _ _ = fail "bad arguments to aCutCol"

--------------------------
-- filter_evalue(_each) --
--------------------------

filterEvalue :: CutFunction
filterEvalue = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, hittable] bht
  , fTypeDesc  = mkTypeDesc name  [num, hittable] bht
  , fDesc = Nothing
  , fFixity    = Prefix
  , fRules     = rSimple aFilterEvalue
  }
  where
    name = "filter_evalue"

filterEvalueEach :: CutFunction
filterEvalueEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf hittable] (ListOf hittable)
  , fTypeDesc  = mkTypeDesc name  [num, ListOf hittable] (ListOf hittable)
  , fDesc = Nothing
  , fFixity    = Prefix
  , fRules     = rMap 2 aFilterEvalue
  }
  where
    name = "filter_evalue_each"

aFilterEvalue :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aFilterEvalue cfg ref _ [out, evalue, hits] = do
  runCmd cfg ref $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [evalue', hits']
    , cmdExtraOutPaths = []
    , cmdOptions =[]
    , cmdBinary = "filter_evalue.R"
    , cmdArguments = [out', evalue', hits']
    , cmdExitCode = ExitSuccess
    }
  where
    out'    = fromCutPath cfg out
    out''   = debugA cfg "aFilterEvalue" out' [out', evalue', hits']
    evalue' = fromCutPath cfg evalue
    hits'   = fromCutPath cfg hits
aFilterEvalue _ _ _ args = error $ "bad argument to aFilterEvalue: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

-- TODO move to BlastRBH?
-- TODO rename to just "best" and "best_each"?

bestHits :: CutFunction
bestHits =  CutFunction
  { fName      = name 
  , fTypeCheck = defaultTypeCheck [hittable] hittable
  , fTypeDesc  = mkTypeDesc name  [hittable] hittable
  , fDesc = Nothing
  , fFixity    = Prefix
  , fRules     = rSimple aBestExtract
  }
  where
    name = "best_hits"

bestHitsEach :: CutFunction
bestHitsEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf hittable] (ListOf hittable)
  , fTypeDesc  = mkTypeDesc name  [ListOf hittable] (ListOf hittable)
  , fDesc = Nothing
  , fFixity    = Prefix
  , fRules     = rMap 1 aBestExtract
  }
  where
    name = "best_hits_each"

aBestExtract :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aBestExtract cfg ref _ [out, hits] = do
  runCmd cfg ref $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [hits']
    , cmdExtraOutPaths = []
    , cmdOptions = []
    , cmdBinary = "best_hits.R"
    , cmdArguments = [out', hits']
    , cmdExitCode = ExitSuccess
    }
  where
    out'  = fromCutPath cfg out
    out'' = debugA cfg "aBestExtract" out' [out', hits']
    hits' = fromCutPath cfg hits
aBestExtract _ _ _ args = error $ "bad argument to aBestExtract: " ++ show args
