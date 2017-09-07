module ShortCut.Modules.Tables where

-- TODO rename to BlastTables? BlastHits?

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Config      (wrappedCmd)
import ShortCut.Core.ModuleAPI   (aTsvColumn)
import ShortCut.Core.ModuleAPI   (rSimpleTmp, rMapLastTmp, defaultTypeCheck)
import ShortCut.Modules.Blast    (bht)
import ShortCut.Modules.BlastCRB (crb)

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
tExtract [x] | elem x [crb, bht] = Right $ ListOf str
tExtract  _ = Left "expected a blast hits table"

tExtractAll :: TypeChecker
tExtractAll [ListOf x] | elem x [crb, bht] = Right $ ListOf $ ListOf str
tExtractAll  _ = Left "expected a list of blast hits tables"

extractQueries :: CutFunction
extractQueries = CutFunction
  { fName      = "extract_queries"
  , fTypeCheck = tExtract
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp (aTsvColumn 1) "tables" (ListOf str)
  }

extractQueriesEach :: CutFunction
extractQueriesEach = CutFunction
  { fName      = "extract_queries_each"
  , fTypeCheck = tExtractAll
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmp (aTsvColumn 1) "tables" (ListOf str)
  }

extractTargets :: CutFunction
extractTargets = CutFunction
  { fName      = "extract_targets"
  , fTypeCheck = tExtract
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp (aTsvColumn 2) "tables" (ListOf str)
  }

extractTargetsEach :: CutFunction
extractTargetsEach = CutFunction
  { fName      = "extract_targets_each"
  , fTypeCheck = tExtractAll
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmp (aTsvColumn 2) "tables" (ListOf str)
  }

---------------------------
-- filter hits by evalue --
---------------------------

filterEvalue :: CutFunction
filterEvalue = CutFunction
  { fName      = "filter_evalue"
  , fTypeCheck = defaultTypeCheck [num, bht] bht
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp aFilterEvalue "blast" bht
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
  , fCompiler  = rSimpleTmp aBestHits "blast" bht
  }

aBestHits :: ActionFn
aBestHits cfg (CacheDir tmp) [ExprPath out, ExprPath hits] = do
  unit $ quietly $ wrappedCmd cfg [out] [Cwd tmp] "best_hits.R" [out, hits]
aBestHits _ _ args = error $ "bad argument to aBestHits: " ++ show args
