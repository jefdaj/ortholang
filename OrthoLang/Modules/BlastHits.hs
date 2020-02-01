module OrthoLang.Modules.BlastHits where

-- TODO are crb files *exactly* in blast format? if so, no need for separate types
-- TODO rename all extract_ functions to not have the extract part? or replace it with list_?

import Development.Shake
import OrthoLang.Core.Types

import System.FilePath             ((<.>))
import OrthoLang.Core.Compile.Basic (rSimple, defaultTypeCheck)
import OrthoLang.Core.Compile.Map  (rMap)
import OrthoLang.Core.Actions       (runCmd, CmdDesc(..), traceA, writeCachedVersion, trackWrite')
import OrthoLang.Core.Paths         (OrthoLangPath, fromOrthoLangPath)
import OrthoLang.Modules.Blast      (bht)
import OrthoLang.Modules.CRBBlast   (crb)
import System.Exit                 (ExitCode(..))

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
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

hittable :: OrthoLangType
hittable = OrthoLangTypeGroup
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

-- tExtractEach :: [OrthoLangType] -> Either String OrthoLangType
-- tExtractEach [ListOf x] | elem x [crb, bht] = Right $ ListOf $ ListOf str
-- tExtractEach  _ = Left "expected a list of blast hits tables"

extractQueries :: OrthoLangFunction
extractQueries = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [hittable] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [hittable] (ListOf str)
  , fFixity    = Prefix
  , fRules     = rSimple $ aOrthoLangCol True 1
  }
  where
    name = "extract_queries"

-- TODO this should have a typeclass
extractQueriesEach :: OrthoLangFunction
extractQueriesEach = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [ListOf hittable] (ListOf (ListOf str))
  , fTypeDesc  = mkTypeDesc name  [ListOf hittable] (ListOf (ListOf str))
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aOrthoLangCol True 1
  }
  where
    name = "extract_queries_each"

-- TODO this should have a typeclass
extractTargets :: OrthoLangFunction
extractTargets = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [hittable] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [hittable] (ListOf str)
  , fFixity    = Prefix
  , fRules     = rSimple $ aOrthoLangCol True 2
  }
  where
    name = "extract_targets"

extractTargetsEach :: OrthoLangFunction
extractTargetsEach = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [ListOf hittable] (ListOf (ListOf str))
  , fTypeDesc  = mkTypeDesc name  [ListOf hittable] (ListOf (ListOf str))
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aOrthoLangCol True 2
  }
  where
    name = "extract_targets_each"

-- TODO remove uniq, unless it's used somewhere?
aOrthoLangCol :: Bool -> Int -> OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ()
aOrthoLangCol _ n cfg ref _ [outPath, tsvPath] = do
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
  --   symlink cfg ref outPath $ toOrthoLangPath cfg outOut

  where
    outPath'  = fromOrthoLangPath cfg outPath
    outPath'' = traceA "aOrthoLangCol" outPath' [show n, outPath', tsvPath']
    tsvPath'  = fromOrthoLangPath cfg tsvPath
    tmpPath'  = outPath'' <.> "tmp" -- the non-deduped version
aOrthoLangCol _ _ _ _ _ _ = fail "bad arguments to aOrthoLangCol"

---------------------
-- filter_*(_each) --
---------------------

filterEvalue :: OrthoLangFunction
filterEvalue = mkFilterHits "evalue"

mkFilterHits :: String -> OrthoLangFunction
mkFilterHits colname = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [num, hittable] bht
  , fTypeDesc  = mkTypeDesc name  [num, hittable] bht
  , fFixity    = Prefix
  , fRules     = rSimple $ aFilterHits colname
  }
  where
    name = "filter_" ++ colname

filterEvalueEach :: OrthoLangFunction
filterEvalueEach = mkFilterHitsEach "evalue"

mkFilterHitsEach :: String -> OrthoLangFunction
mkFilterHitsEach colname = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [num, ListOf hittable] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, ListOf hittable] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMap 2 $ aFilterHits colname
  }
  where
    name = "filter_" ++ colname ++ "_each"

aFilterHits :: String -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
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
    out'    = fromOrthoLangPath cfg out
    out''   = traceA "aFilterHits" out' [out', cutoff', hits']
    cutoff' = fromOrthoLangPath cfg cutoff
    hits'   = fromOrthoLangPath cfg hits
aFilterHits _ _ _ _ args = error $ "bad argument to aFilterHits: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

-- TODO move to BlastRBH?
-- TODO rename to just "best" and "best_each"?

-- TODO should this return whatever hittable type it's given?
-- TODO split into best_hits_evalue and best_hits_bitscore?
bestHits :: OrthoLangFunction
bestHits =  OrthoLangFunction
  { fNames     = [name] 
  , fTypeCheck = defaultTypeCheck [hittable] bht -- TODO is bht right?
  , fTypeDesc  = mkTypeDesc name  [hittable] bht -- TODO is bht right?
  , fFixity    = Prefix
  , fRules     = rSimple aBestExtract
  }
  where
    name = "best_hits"

bestHitsEach :: OrthoLangFunction
bestHitsEach = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [ListOf hittable] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [ListOf hittable] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMap 1 aBestExtract
  }
  where
    name = "best_hits_each"

aBestExtract :: OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ()
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
    out'  = fromOrthoLangPath cfg out
    out'' = traceA "aBestExtract" out' [out', hits']
    hits' = fromOrthoLangPath cfg hits
aBestExtract _ _ _ args = error $ "bad argument to aBestExtract: " ++ show args
