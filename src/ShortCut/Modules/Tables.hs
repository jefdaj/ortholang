module ShortCut.Modules.Tables where

import ShortCut.Core.Types
import ShortCut.Modules.Blast    (bht)
import ShortCut.Modules.BlastCRB (crb)
import ShortCut.Core.ModuleAPI   (aTsvColumn, rSimpleTmp, rMapLastTmp)

cutModule :: CutModule
cutModule = CutModule
  { mName = "tables"
  , mFunctions =
    [ extractQueries
    , extractAllQueries
    , extractTargets
    , extractAllTargets
    ]
  }

---------------------------------
-- extract results from tables --
---------------------------------

tExtract :: [CutType] -> Either String CutType
tExtract [x] | elem x [crb, bht] = Right $ ListOf str
tExtract  _ = Left "expected a blast hits table"

tExtractAll :: [CutType] -> Either String CutType
tExtractAll [ListOf x] | elem x [crb, bht] = Right $ ListOf $ ListOf str
tExtractAll  _ = Left "expected a list of blast hits tables"

extractQueries :: CutFunction
extractQueries = CutFunction
  { fName      = "extract_queries"
  , fTypeCheck = tExtract
  , fFixity    = Prefix
  , fCompiler  = rSimpleTmp (aTsvColumn 1) "tables" (ListOf str)
  }

extractAllQueries :: CutFunction
extractAllQueries = CutFunction
  { fName      = "extract_all_queries"
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

extractAllTargets :: CutFunction
extractAllTargets = CutFunction
  { fName      = "extract_all_targets"
  , fTypeCheck = tExtractAll
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmp (aTsvColumn 2) "tables" (ListOf str)
  }
