module Detourrr.Modules.BlastHits where

import Development.Shake
import Detourrr.Core.Types

import Data.List                   (nub, sort)
import Detourrr.Core.Compile.Basic (rSimple, defaultTypeCheck)
import Detourrr.Core.Compile.Map  (rMap)
import Detourrr.Core.Actions       (wrappedCmdOut, wrappedCmdWrite, writeLits, debugA)
-- import Detourrr.Core.Debug         (debugA )
import Detourrr.Core.Paths         (DtrPath, fromDtrPath)
import Detourrr.Modules.Blast      (bht)
import Detourrr.Modules.BlastCRB   (crb)

dtrModule :: DtrModule
dtrModule = DtrModule
  { mName = "BlastHits"
  , mDesc = "Work with BLAST hit tables"
  , mTypes = [bht, crb]
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

tExtractEach :: [DtrType] -> Either String DtrType
tExtractEach [ListOf x] | elem x [crb, bht] = Right $ ListOf $ ListOf str
tExtractEach  _ = Left "expected a list of blast hits tables"

extractQueries :: DtrFunction
extractQueries = let name = "extract_queries" in DtrFunction
  { fName      = name
  , fTypeCheck = tExtract
  , fDesc = Nothing, fTypeDesc  = name ++ " : <crb/bht> -> str.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aDtrCol True 1
  }

extractQueriesEach :: DtrFunction
extractQueriesEach = let name = "extract_queries_each" in DtrFunction
  { fName      = name
  , fTypeCheck = tExtractEach
  , fDesc = Nothing, fTypeDesc  = name ++ " : <crb/bht>.list -> str.list.list"
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aDtrCol True 1
  }

extractTargets :: DtrFunction
extractTargets = let name = "extract_targets" in DtrFunction
  { fName      = name
  , fTypeCheck = tExtract
  , fDesc = Nothing, fTypeDesc  = name ++ " : <crb/bht> -> str.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aDtrCol True 2
  }

extractTargetsEach :: DtrFunction
extractTargetsEach = let name = "extract_targets_each" in DtrFunction
  { fName      = name
  , fTypeCheck = tExtractEach
  , fDesc = Nothing, fTypeDesc  = name ++ " : <crb/bht>.list -> str.list.list"
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aDtrCol True 2
  }

aDtrCol :: Bool -> Int -> DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ()
aDtrCol uniq n cfg ref _ [outPath, tsvPath] = do
  out <- wrappedCmdOut False True cfg ref [tsvPath'] [] [] "dtr" ["-f", show n, tsvPath']
  let results = if uniq then sort $ nub $ lines out else lines out
  writeLits cfg ref outPath'' results
  where
    outPath'  = fromDtrPath cfg outPath
    outPath'' = debugA cfg "aDtrCol" outPath' [show n, outPath', tsvPath']
    tsvPath'  = fromDtrPath cfg tsvPath
aDtrCol _ _ _ _ _ _ = error "bad arguments to aDtrCol"

--------------------------
-- filter_evalue(_each) --
--------------------------

filterEvalue :: DtrFunction
filterEvalue = let name = "filter_evalue" in DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, bht] bht
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aFilterEvalue
  }

filterEvalueEach :: DtrFunction
filterEvalueEach = let name = "filter_evalue_each" in DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf bht] (ListOf bht)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMap 2 aFilterEvalue
  }

aFilterEvalue :: DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ()
aFilterEvalue cfg ref _ [out, evalue, hits] = do
  wrappedCmdWrite False True cfg ref out'' [evalue', hits'] [] []
    "filter_evalue.R" [out', evalue', hits']
  where
    out'    = fromDtrPath cfg out
    out''   = debugA cfg "aFilterEvalue" out' [out', evalue', hits']
    evalue' = fromDtrPath cfg evalue
    hits'   = fromDtrPath cfg hits
aFilterEvalue _ _ _ args = error $ "bad argument to aFilterEvalue: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

-- TODO move to BlastRBH?
-- TODO rename to just "best" and "best_each"?

bestHits :: DtrFunction
bestHits = let name = "best_hits" in DtrFunction
  { fName      = name 
  , fTypeCheck = defaultTypeCheck [bht] bht
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aBestExtract
  }

bestHitsEach :: DtrFunction
bestHitsEach = let name = "best_hits_each" in DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf bht] (ListOf bht)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMap 1 aBestExtract
  }

aBestExtract :: DtrConfig -> Locks -> HashedSeqIDsRef -> [DtrPath] -> Action ()
aBestExtract cfg ref _ [out, hits] = do
  wrappedCmdWrite False True cfg ref out'' [hits'] [] [] "best_hits.R" [out', hits']
  where
    out'  = fromDtrPath cfg out
    out'' = debugA cfg "aBestExtract" out' [out', hits']
    hits' = fromDtrPath cfg hits
aBestExtract _ _ _ args = error $ "bad argument to aBestExtract: " ++ show args
