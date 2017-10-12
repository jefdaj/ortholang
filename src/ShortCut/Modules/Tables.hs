module ShortCut.Modules.Tables where

-- TODO rename to BlastTables? BlastHits?

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Config      (wrappedCmd)
import ShortCut.Core.Compile.Basic       (rSimple, rSimpleTmp, defaultTypeCheck)
import ShortCut.Core.Compile.Map       (rMap)
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
  , fRules  = rSimple $ aTsvColumn 1
  }

extractQueriesEach :: CutFunction
extractQueriesEach = CutFunction
  { fName      = "extract_queries_each"
  , fTypeCheck = tExtractEach
  , fFixity    = Prefix
  , fRules  = rMap $ aTsvColumn 1
  }

extractTargets :: CutFunction
extractTargets = CutFunction
  { fName      = "extract_targets"
  , fTypeCheck = tExtract
  , fFixity    = Prefix
  , fRules  = rSimple $ aTsvColumn 2
  }

extractTargetsEach :: CutFunction
extractTargetsEach = CutFunction
  { fName      = "extract_targets_each"
  , fTypeCheck = tExtractEach
  , fFixity    = Prefix
  , fRules  = rMap $ aTsvColumn 2
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
  , fRules  = rSimpleTmp "blast" aFilterEvalue -- TODO remove tmpdir?
  }

aFilterEvalue :: CutConfig -> CutPath -> [CutPath] -> Action ()
aFilterEvalue cfg tmp [out, evalue, hits] = do
  unit $ quietly $ wrappedCmd cfg [out'] [Cwd tmp']
                     "filter_evalue.R" [out', evalue', hits']
  debugTrackWrite cfg [out'']
  where
    tmp'    = fromCutPath cfg tmp
    out'    = fromCutPath cfg out
    out''   = debugAction cfg "aFilterEvalue" out' [tmp', out', evalue', hits']
    evalue' = fromCutPath cfg evalue
    hits'   = fromCutPath cfg hits
aFilterEvalue _ _ args = error $ "bad argument to aFilterEvalue: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

bestHits :: CutFunction
bestHits = CutFunction
  { fName      = "best_hits"
  , fTypeCheck = defaultTypeCheck [bht] bht
  , fFixity    = Prefix
  , fRules  = rSimpleTmp "blast" aBestHits
  }

aBestHits :: CutConfig -> CutPath -> [CutPath] -> Action ()
aBestHits cfg tmp [out, hits] = do
  unit $ quietly $ wrappedCmd cfg [out'] [Cwd tmp'] "best_hits.R" [out', hits']
  debugTrackWrite cfg [out'']
  where
    tmp'  = fromCutPath cfg tmp
    out'  = fromCutPath cfg out
    out'' = debugAction cfg "aBestHits" out' [tmp', out', hits']
    hits' = fromCutPath cfg hits
aBestHits _ _ args = error $ "bad argument to aBestHits: " ++ show args
