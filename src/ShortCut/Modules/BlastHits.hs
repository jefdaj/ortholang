module ShortCut.Modules.BlastHits where

-- TODO rename to BlastHits?

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Config      (wrappedCmd)
import ShortCut.Core.Compile.Basic       (rSimple, defaultTypeCheck)
import ShortCut.Core.Compile.Each       (rEach)
import ShortCut.Modules.Blast    (bht)
import ShortCut.Modules.BlastCRB (crb)
import Data.List                  (nub, sort)
import ShortCut.Core.Paths (CutPath, fromCutPath, writeLits)
import ShortCut.Core.Debug        (debugAction, debugTrackWrite)

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
    , bestHitsEach
    ]
  }

---------------------------------
-- extract results from tables --
---------------------------------

tExtract :: TypeChecker
tExtract [x] | elem x [crb, bht] = Right $ ListOf str
tExtract  _ = Left "expected a blast hits table"

tExtractEach :: [CutType] -> Either String CutType
tExtractEach [ListOf x] | elem x [crb, bht] = Right $ ListOf $ ListOf str
tExtractEach  _ = Left "expected a list of blast hits tables"

extractQueries :: CutFunction
extractQueries = CutFunction
  { fName      = "extract_queries"
  , fTypeCheck = tExtract
  , fFixity    = Prefix
  , fRules     = rSimple $ aTsvColumn 1
  }

extractQueriesEach :: CutFunction
extractQueriesEach = CutFunction
  { fName      = "extract_queries_each"
  , fTypeCheck = tExtractEach
  , fFixity    = Prefix
  , fRules     = rEach $ aTsvColumn 1
  }

extractTargets :: CutFunction
extractTargets = CutFunction
  { fName      = "extract_targets"
  , fTypeCheck = tExtract
  , fFixity    = Prefix
  , fRules     = rSimple $ aTsvColumn 2
  }

extractTargetsEach :: CutFunction
extractTargetsEach = CutFunction
  { fName      = "extract_targets_each"
  , fTypeCheck = tExtractEach
  , fFixity    = Prefix
  , fRules     = rEach $ aTsvColumn 2
  }

-- TODO rewrite this awk -> haskell, and using wrappedCmd
aTsvColumn :: Int -> CutConfig -> [CutPath] -> Action ()
aTsvColumn n cfg [outPath, tsvPath] = do
  let awkCmd = "awk '{print $" ++ show n ++ "}'"
  Stdout out <- quietly $ cmd Shell awkCmd tsvPath'
  let out' = sort $ nub $ lines out
  writeLits cfg outPath'' out'
  where
    outPath'  = fromCutPath cfg outPath
    outPath'' = debugAction cfg "aTsvColumn" outPath' [show n, outPath', tsvPath']
    tsvPath'  = fromCutPath cfg tsvPath
aTsvColumn _ _ _ = error "bad arguments to aTsvColumn"

---------------------------
-- filter hits by evalue --
---------------------------

filterEvalue :: CutFunction
filterEvalue = CutFunction
  { fName      = "filter_evalue"
  , fTypeCheck = defaultTypeCheck [num, bht] bht
  , fFixity    = Prefix
  , fRules  = rSimple aFilterEvalue
  }

aFilterEvalue :: CutConfig -> [CutPath] -> Action ()
aFilterEvalue cfg [out, evalue, hits] = do
  unit $ quietly $ wrappedCmd cfg [out'] []
                     "filter_evalue.R" [out', evalue', hits']
  debugTrackWrite cfg [out'']
  where
    out'    = fromCutPath cfg out
    out''   = debugAction cfg "aFilterEvalue" out' [out', evalue', hits']
    evalue' = fromCutPath cfg evalue
    hits'   = fromCutPath cfg hits
aFilterEvalue _ args = error $ "bad argument to aFilterEvalue: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

-- TODO move to BlastRBH?
-- TODO rename to just "best" and "best_each"?

bestHits :: CutFunction
bestHits = CutFunction
  { fName      = "best_hits"
  , fTypeCheck = defaultTypeCheck [bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aBestHits
  }

bestHitsEach :: CutFunction
bestHitsEach = CutFunction
  { fName      = "best_hits_each"
  , fTypeCheck = defaultTypeCheck [ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rEach aBestHits
  }

aBestHits :: CutConfig -> [CutPath] -> Action ()
aBestHits cfg [out, hits] = do
  unit $ quietly $ wrappedCmd cfg [out'] [] "best_hits.R" [out', hits']
  debugTrackWrite cfg [out'']
  where
    out'  = fromCutPath cfg out
    out'' = debugAction cfg "aBestHits" out' [out', hits']
    hits' = fromCutPath cfg hits
aBestHits _ args = error $ "bad argument to aBestHits: " ++ show args
