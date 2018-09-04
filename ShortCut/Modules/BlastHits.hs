module ShortCut.Modules.BlastHits where

import Development.Shake
import ShortCut.Core.Types

import Data.List                   (nub, sort)
import ShortCut.Core.Compile.Basic (rSimple, defaultTypeCheck)
import ShortCut.Core.Compile.Vectorize  (rVectorize)
import ShortCut.Core.Actions       (wrappedCmdOut, wrappedCmdWrite, writeLits, debugA)
-- import ShortCut.Core.Debug         (debugA )
import ShortCut.Core.Paths         (CutPath, fromCutPath)
import ShortCut.Modules.Blast      (bht)
import ShortCut.Modules.BlastCRB   (crb)

cutModule :: CutModule
cutModule = CutModule
  { mName = "tables"
  , mFunctions =
    [ extractQueries, extractQueriesEach
    , extractTargets, extractTargetsEach
    , filterEvalue  , filterEvalueEach
    , bestHits      , bestHitsEach
    ]
  }

----------------------
-- extract_*(_each) --
----------------------

tExtract :: TypeChecker
tExtract [x] | elem x [crb, bht] = Right $ ListOf str
tExtract  _ = Left "expected a blast hits table"

tExtractEach :: [CutType] -> Either String CutType
tExtractEach [ListOf x] | elem x [crb, bht] = Right $ ListOf $ ListOf str
tExtractEach  _ = Left "expected a list of blast hits tables"

extractQueries :: CutFunction
extractQueries = let name = "extract_queries" in CutFunction
  { fName      = name
  , fTypeCheck = tExtract
  , fTypeDesc  = name ++ " : <crb/bht> -> str.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aCutCol True 1
  }

extractQueriesEach :: CutFunction
extractQueriesEach = let name = "extract_queries_each" in CutFunction
  { fName      = name
  , fTypeCheck = tExtractEach
  , fTypeDesc  = name ++ " : <crb/bht>.list -> str.list.list"
  , fFixity    = Prefix
  , fRules     = rVectorize 1 $ aCutCol True 1
  }

extractTargets :: CutFunction
extractTargets = let name = "extract_targets" in CutFunction
  { fName      = name
  , fTypeCheck = tExtract
  , fTypeDesc  = name ++ " : <crb/bht> -> str.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aCutCol True 2
  }

extractTargetsEach :: CutFunction
extractTargetsEach = let name = "extract_targets_each" in CutFunction
  { fName      = name
  , fTypeCheck = tExtractEach
  , fTypeDesc  = name ++ " : <crb/bht>.list -> str.list.list"
  , fFixity    = Prefix
  , fRules     = rVectorize 1 $ aCutCol True 2
  }

aCutCol :: Bool -> Int -> CutConfig -> Locks -> [CutPath] -> Action ()
aCutCol uniq n cfg ref [outPath, tsvPath] = do
  out <- wrappedCmdOut False True cfg ref [tsvPath'] [] [] "cut" ["-f", show n, tsvPath']
  let results = if uniq then sort $ nub $ lines out else lines out
  writeLits cfg ref outPath'' results
  where
    outPath'  = fromCutPath cfg outPath
    outPath'' = debugA cfg "aCutCol" outPath' [show n, outPath', tsvPath']
    tsvPath'  = fromCutPath cfg tsvPath
aCutCol _ _ _ _ _ = error "bad arguments to aCutCol"

--------------------------
-- filter_evalue(_each) --
--------------------------

filterEvalue :: CutFunction
filterEvalue = let name = "filter_evalue" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, bht] bht
  , fTypeDesc  = mkTypeDesc name  [num, bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aFilterEvalue
  }

filterEvalueEach :: CutFunction
filterEvalueEach = let name = "filter_evalue_each" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf bht] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rVectorize 2 aFilterEvalue
  }

aFilterEvalue :: CutConfig -> Locks -> [CutPath] -> Action ()
aFilterEvalue cfg ref [out, evalue, hits] = do
  wrappedCmdWrite False True cfg ref out'' [evalue', hits'] [] []
    "filter_evalue.R" [out', evalue', hits']
  where
    out'    = fromCutPath cfg out
    out''   = debugA cfg "aFilterEvalue" out' [out', evalue', hits']
    evalue' = fromCutPath cfg evalue
    hits'   = fromCutPath cfg hits
aFilterEvalue _ _ args = error $ "bad argument to aFilterEvalue: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

-- TODO move to BlastRBH?
-- TODO rename to just "best" and "best_each"?

bestHits :: CutFunction
bestHits = let name = "best_hits" in CutFunction
  { fName      = name 
  , fTypeCheck = defaultTypeCheck [bht] bht
  , fTypeDesc  = mkTypeDesc name  [bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aBestExtract
  }

bestHitsEach :: CutFunction
bestHitsEach = let name = "best_hits_each" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf bht] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rVectorize 1 aBestExtract
  }

aBestExtract :: CutConfig -> Locks -> [CutPath] -> Action ()
aBestExtract cfg ref [out, hits] = do
  wrappedCmdWrite False True cfg ref out'' [hits'] [] [] "best_hits.R" [out', hits']
  where
    out'  = fromCutPath cfg out
    out'' = debugA cfg "aBestExtract" out' [out', hits']
    hits' = fromCutPath cfg hits
aBestExtract _ _ args = error $ "bad argument to aBestExtract: " ++ show args
