module OrthoLang.Modules.BlastHits where

-- TODO are crb files *exactly* in blast format? if so, no need for separate types
-- TODO rename all extract_ functions to not have the extract part? or replace it with list_?

import Development.Shake
import OrthoLang.Core.Types

import System.FilePath             (replaceBaseName)
import OrthoLang.Core.Compile (defaultTypeCheck)
import OrthoLang.Core.Compile (rSimple)
import OrthoLang.Core.Compile  (rMap)
import OrthoLang.Core.Actions       (runCmd, CmdDesc(..), traceA, writeCachedVersion, trackWrite')
import OrthoLang.Core.Paths         (Path, fromPath)
import OrthoLang.Modules.Blast      (bht)
import OrthoLang.Modules.CRBBlast   (crb)
import System.Exit                 (ExitCode(..))

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "BlastHits"
  , mDesc = "Work with BLAST hit tables"
  , mTypes = [bht, crb, hittable]
  , mFunctions =
    [ extractQueries, extractQueriesEach
    , extractTargets, extractTargetsEach
    , mkFilterHits "evalue"  , mkFilterHitsEach "evalue"
    , mkFilterHits "bitscore", mkFilterHitsEach "bitscore"
    , mkFilterHits "pident"  , mkFilterHitsEach "pident"
    -- TODO mkFilterHits "rawscore", mkFilterHitsEach "rawscore"
    , bestHits, bestHitsEach
    ]
  }

hittable :: Type
hittable = TypeGroup
  { tgExt = "hittable"
  , tgDesc  = "BLAST hit table-like"
  , tgMember = \t -> t `elem` [bht, crb] -- TODO mms too
  }

----------------------
-- extract_*(_each) --
----------------------

-- tExtract :: TypeChecker
-- tExtract [x] | elem x [crb, bht] = Right $ ListOf str
-- tExtract  _ = Left "expected a blast hits table"

-- tExtractEach :: [Type] -> Either String Type
-- tExtractEach [ListOf x] | elem x [crb, bht] = Right $ ListOf $ ListOf str
-- tExtractEach  _ = Left "expected a list of blast hits tables"

extractQueries :: Function
extractQueries = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [hittable] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [hittable] (ListOf str)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple $ aCutCol True 1
  }
  where
    name = "extract_queries"

-- TODO this should have a typeclass
extractQueriesEach :: Function
extractQueriesEach = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [ListOf hittable] (ListOf (ListOf str))
  , fTypeDesc  = mkTypeDesc name  [ListOf hittable] (ListOf (ListOf str))
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMap 1 $ aCutCol True 1
  }
  where
    name = "extract_queries_each"

-- TODO this should have a typeclass
extractTargets :: Function
extractTargets = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [hittable] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [hittable] (ListOf str)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple $ aCutCol True 2
  }
  where
    name = "extract_targets"

extractTargetsEach :: Function
extractTargetsEach = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [ListOf hittable] (ListOf (ListOf str))
  , fTypeDesc  = mkTypeDesc name  [ListOf hittable] (ListOf (ListOf str))
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMap 1 $ aCutCol True 2
  }
  where
    name = "extract_targets_each"

-- TODO remove uniq, unless it's used somewhere?
aCutCol :: Bool -> Int -> Config -> LocksRef -> IDsRef -> [Path] -> Action ()
aCutCol _ n cfg ref _ [outPath, tsvPath] = do
  runCmd cfg ref $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = tmpPath'
    , cmdInPatterns = [tsvPath']
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "cut_tsv.sh"
    , cmdArguments = [tmpPath', tsvPath', show n]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath']
    }
  trackWrite' cfg [tmpPath']
  writeCachedVersion cfg ref outPath'' tmpPath'

  -- TODO remove this? why does it need to be here at all?
  -- let outOut = outPath' <.> "out"
  -- unlessExists outOut $ do
  --   symlink cfg ref outPath $ toPath cfg outOut

  where
    outPath'  = fromPath cfg outPath
    outPath'' = traceA "aCutCol" outPath' [show n, outPath', tsvPath']
    tsvPath'  = fromPath cfg tsvPath
    tmpPath'  = replaceBaseName outPath'' "tmp" -- the non-deduped version
aCutCol _ _ _ _ _ _ = fail "bad arguments to aCutCol"

---------------------
-- filter_*(_each) --
---------------------

filterEvalue :: Function
filterEvalue = mkFilterHits "evalue"

mkFilterHits :: String -> Function
mkFilterHits colname = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [num, hittable] bht
  , fTypeDesc  = mkTypeDesc name  [num, hittable] bht
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple $ aFilterHits colname
  }
  where
    name = "filter_" ++ colname

filterEvalueEach :: Function
filterEvalueEach = mkFilterHitsEach "evalue"

mkFilterHitsEach :: String -> Function
mkFilterHitsEach colname = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [num, ListOf hittable] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, ListOf hittable] (ListOf bht)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMap 2 $ aFilterHits colname
  }
  where
    name = "filter_" ++ colname ++ "_each"

aFilterHits :: String -> (Config -> LocksRef -> IDsRef -> [Path] -> Action ())
aFilterHits colname cfg ref _ [out, cutoff, hits] = do
  runCmd cfg ref $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [cutoff', hits']
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "filter_hits.R"
    , cmdArguments = [out', colname, cutoff', hits']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out'']
    }
  where
    out'    = fromPath cfg out
    out''   = traceA "aFilterHits" out' [out', cutoff', hits']
    cutoff' = fromPath cfg cutoff
    hits'   = fromPath cfg hits
aFilterHits _ _ _ _ args = error $ "bad argument to aFilterHits: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

-- TODO move to BlastRBH?
-- TODO rename to just "best" and "best_each"?

-- TODO should this return whatever hittable type it's given?
-- TODO split into best_hits_evalue and best_hits_bitscore?
bestHits :: Function
bestHits =  Function
  { fOpChar = Nothing, fName = name 
  , fTypeCheck = defaultTypeCheck [hittable] bht -- TODO is bht right?
  , fTypeDesc  = mkTypeDesc name  [hittable] bht -- TODO is bht right?
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple aBestExtract
  }
  where
    name = "best_hits"

bestHitsEach :: Function
bestHitsEach = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [ListOf hittable] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [ListOf hittable] (ListOf bht)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMap 1 aBestExtract
  }
  where
    name = "best_hits_each"

aBestExtract :: Config -> LocksRef -> IDsRef -> [Path] -> Action ()
aBestExtract cfg ref _ [out, hits] = do
  runCmd cfg ref $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [hits']
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdBinary = "best_hits.R"
    , cmdArguments = [out', hits']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out']
    }
  where
    out'  = fromPath cfg out
    out'' = traceA "aBestExtract" out' [out', hits']
    hits' = fromPath cfg hits
aBestExtract _ _ _ args = error $ "bad argument to aBestExtract: " ++ show args
