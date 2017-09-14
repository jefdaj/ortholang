module ShortCut.Modules.Tables where

-- TODO rename to BlastTables? BlastHits?

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Config      (wrappedCmd)
import ShortCut.Core.Compile.Rules       (rSimpleTmp, rMapLastTmp, defaultTypeCheck)
import ShortCut.Modules.Blast    (bht)
import ShortCut.Modules.BlastCRB (crb)
import Data.List                  (nub, sort)
import ShortCut.Core.Debug        (debugWriteLines)

cutModule :: CutModule
cutModule = CutModule
  { mName = "tables"
  , mFunctions =
    [ extractQueries
    , extractQueriesEach
    , extractTargets
    , extractTargetsEach
    , filterEvalue
    , bestHits
    ]
  }

---------------------------------
-- extract results from tables --
---------------------------------

tExtract :: TypeChecker
tExtract [x] | elem x [crb, bht] = Right $ SetOf str
tExtract  _ = Left "expected a blast hits table"

tExtractEach :: [CutType] -> Either String CutType
tExtractEach [SetOf x] | elem x [crb, bht] = Right $ SetOf $ SetOf str
tExtractEach  _ = Left "expected a list of blast hits tables"

extractQueries :: CutFunction
extractQueries = CutFunction
  { fName      = "extract_queries"
  , fTypeCheck = tExtract
  , fFixity    = Prefix
  , fRules  = rSimpleTmp (aTsvColumn 1) "tables" (SetOf str)
  }

extractQueriesEach :: CutFunction
extractQueriesEach = CutFunction
  { fName      = "extract_queries_each"
  , fTypeCheck = tExtractEach
  , fFixity    = Prefix
  , fRules  = rMapLastTmp (aTsvColumn 1) "tables" (SetOf str)
  }

extractTargets :: CutFunction
extractTargets = CutFunction
  { fName      = "extract_targets"
  , fTypeCheck = tExtract
  , fFixity    = Prefix
  , fRules  = rSimpleTmp (aTsvColumn 2) "tables" (SetOf str)
  }

extractTargetsEach :: CutFunction
extractTargetsEach = CutFunction
  { fName      = "extract_targets_each"
  , fTypeCheck = tExtractEach
  , fFixity    = Prefix
  , fRules  = rMapLastTmp (aTsvColumn 2) "tables" (SetOf str)
  }

-- TODO rewrite this awk -> haskell, and using wrappedCmd
aTsvColumn :: Int -> ActionFn
aTsvColumn n cfg _ [ExprPath outPath, ExprPath tsvPath] = do
  let awkCmd = "awk '{print $" ++ show n ++ "}'"
  Stdout out <- quietly $ cmd Shell awkCmd tsvPath
  let out' = sort $ nub $ lines out
  -- toShortCutSetStr cfg str outPath out'
  debugWriteLines cfg outPath out'
aTsvColumn _ _ _ _ = error "bad arguments to aTsvColumn"


---------------------------
-- filter hits by evalue --
---------------------------

filterEvalue :: CutFunction
filterEvalue = CutFunction
  { fName      = "filter_evalue"
  , fTypeCheck = defaultTypeCheck [num, bht] bht
  , fFixity    = Prefix
  , fRules  = rSimpleTmp aFilterEvalue "blast" bht
  }

aFilterEvalue :: ActionFn
aFilterEvalue cfg (CacheDir tmp) [ExprPath out, ExprPath evalue, ExprPath hits] = do
  unit $ quietly $ wrappedCmd cfg [out] [Cwd tmp]
                     "filter_evalue.R" [out, evalue, hits]
aFilterEvalue _ _ args = error $ "bad argument to aFilterEvalue: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

bestHits :: CutFunction
bestHits = CutFunction
  { fName      = "best_hits"
  , fTypeCheck = defaultTypeCheck [bht] bht
  , fFixity    = Prefix
  , fRules  = rSimpleTmp aBestHits "blast" bht
  }

aBestHits :: ActionFn
aBestHits cfg (CacheDir tmp) [ExprPath out, ExprPath hits] = do
  unit $ quietly $ wrappedCmd cfg [out] [Cwd tmp] "best_hits.R" [out, hits]
aBestHits _ _ args = error $ "bad argument to aBestHits: " ++ show args
