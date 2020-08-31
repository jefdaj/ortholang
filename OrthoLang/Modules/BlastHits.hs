module OrthoLang.Modules.BlastHits where

-- TODO are crb files *exactly* in blast format? if so, no need for separate types
-- TODO rename all extract_ functions to not have the extract part? or replace it with list_?

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

import OrthoLang.Modules.Blast    (bht)
import OrthoLang.Modules.CRBBlast (crb)
import System.Exit                (ExitCode(..))
import System.FilePath            ((</>), takeDirectory)
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "BlastHits"
  , mDesc = "Work with BLAST hit tables"
  , mTypes = [bht, crb]
  , mGroups = [ht]
  , mEncodings = []
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

-- TODO use this more!
ht :: TypeGroup
ht = TypeGroup
  { tgExt = "ht"
  , tgDesc  = "hit table"
  , tgMembers = [Exactly bht, Exactly crb] -- TODO mms too
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
  , fInputs = [Some ht "a hit table"]
  , fOutput = Exactly (ListOf str)
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aCutCol True 1
  }
  where
    name = "extract_queries"

-- TODO this should have a typeclass
extractQueriesEach :: Function
extractQueriesEach = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [ListSigs (Some ht "a hit table")]
  , fOutput = Exactly (ListOf (ListOf str))
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMap 1 $ aCutCol True 1
  }
  where
    name = "extract_queries_each"

-- TODO this should have a typeclass
extractTargets :: Function
extractTargets = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Some ht "a hit table"]
  , fOutput = Exactly (ListOf str)
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rSimple $ aCutCol True 2
  }
  where
    name = "extract_targets"

extractTargetsEach :: Function
extractTargetsEach = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [ListSigs (Some ht "a hit table")]
  , fOutput = Exactly (ListOf (ListOf str))
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMap 1 $ aCutCol True 2
  }
  where
    name = "extract_targets_each"

-- TODO remove uniq, unless it's used somewhere?
aCutCol :: Bool -> Int -> [Path] -> Action ()
aCutCol _ n [outPath, tsvPath] = do
  cfg <- fmap fromJust getShakeExtra
  let outPath'  = fromPath loc cfg outPath
      loc = "modules.blasthits.aCutCol"
      outPath'' = traceA loc outPath' [show n, outPath', tsvPath']
      tsvPath'  = fromPath loc cfg tsvPath
      tmpPath'  = takeDirectory outPath'' </> "tmp" -- the non-deduped version
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = tmpPath'
    , cmdInPatterns = [tsvPath']
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "cut_tsv.sh"
    , cmdArguments = [tmpPath', tsvPath', show n]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath']
    }
  trackWrite' [tmpPath']
  writeCachedVersion loc outPath'' tmpPath'

  -- TODO remove this? why does it need to be here at all?
  -- let outOut = outPath' <.> "out"
  -- unlessExists outOut $ do
  --   symlink outPath $ toPath loc cfg outOut

aCutCol _ _ _ = fail "bad arguments to aCutCol"

---------------------
-- filter_*(_each) --
---------------------

filterEvalue :: Function
filterEvalue = mkFilterHits "evalue"

mkFilterHits :: String -> Function
mkFilterHits colname = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly num, Some ht "a hit table"]
  , fOutput = Some ht "a hit table" -- TODO or bht like before?
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rSimple $ aFilterHits colname
  }
  where
    name = "filter_" ++ colname

filterEvalueEach :: Function
filterEvalueEach = mkFilterHitsEach "evalue"

mkFilterHitsEach :: String -> Function
mkFilterHitsEach colname = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly num, ListSigs (Some ht "a hit table")]
  , fOutput = ListSigs (Some ht "a hit table")
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMap 2 $ aFilterHits colname
  }
  where
    name = "filter_" ++ colname ++ "_each"

aFilterHits :: String -> ([Path] -> Action ())
aFilterHits colname [out, cutoff, hits] = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.blasthits.aFilterHits"
      out'    = fromPath loc cfg out
      out''   = traceA "aFilterHits" out' [out', cutoff', hits']
      cutoff' = fromPath loc cfg cutoff
      hits'   = fromPath loc cfg hits
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [cutoff', hits']
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "filter_hits.R"
    , cmdArguments = [out', colname, cutoff', hits']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out'']
    }
aFilterHits _ args = error $ "bad argument to aFilterHits: " ++ show args

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
  , fInputs = [Some ht "a hit table"]
  , fOutput =  Some ht "a hit table"
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rSimple aBestExtract
  }
  where
    name = "best_hits"

bestHitsEach :: Function
bestHitsEach = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [ListSigs (Some ht "a hit table")]
  , fOutput =  ListSigs (Some ht "a hit table")
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMap 1 aBestExtract
  }
  where
    name = "best_hits_each"

aBestExtract :: [Path] -> Action ()
aBestExtract [out, hits] = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.blasthits.aBestExtract"
      out'  = fromPath loc cfg out
      out'' = traceA "aBestExtract" out' [out', hits']
      hits' = fromPath loc cfg hits
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out''
    , cmdInPatterns = [hits']
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdBinary = "best_hits.R"
    , cmdArguments = [out', hits']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out']
    }
aBestExtract args = error $ "bad argument to aBestExtract: " ++ show args
