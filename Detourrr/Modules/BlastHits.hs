module Detourrr.Modules.BlastHits where

import Development.Shake
import Detourrr.Core.Types

import Data.List                   (nub, sort)
import Detourrr.Core.Compile.Basic (rSimple, defaultTypeCheck)
import Detourrr.Core.Compile.Map  (rMap)
import Detourrr.Core.Actions       (wrappedCmdOut, wrappedCmdWrite, writeLits, debugA)
-- import Detourrr.Core.Debug         (debugA )
import Detourrr.Core.Paths         (RrrPath, fromRrrPath)
import Detourrr.Modules.Blast      (bht)
import Detourrr.Modules.CRBBlast   (crb)

rrrModule :: RrrModule
rrrModule = RrrModule
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

tExtractEach :: [RrrType] -> Either String RrrType
tExtractEach [ListOf x] | elem x [crb, bht] = Right $ ListOf $ ListOf str
tExtractEach  _ = Left "expected a list of blast hits tables"

extractQueries :: RrrFunction
extractQueries = let name = "extract_queries" in RrrFunction
  { fName      = name
  , fTypeCheck = tExtract
  , fDesc = Nothing, fTypeDesc  = name ++ " : <crb/bht> -> str.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aRrrCol True 1
  }

extractQueriesEach :: RrrFunction
extractQueriesEach = let name = "extract_queries_each" in RrrFunction
  { fName      = name
  , fTypeCheck = tExtractEach
  , fDesc = Nothing, fTypeDesc  = name ++ " : <crb/bht>.list -> str.list.list"
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aRrrCol True 1
  }

extractTargets :: RrrFunction
extractTargets = let name = "extract_targets" in RrrFunction
  { fName      = name
  , fTypeCheck = tExtract
  , fDesc = Nothing, fTypeDesc  = name ++ " : <crb/bht> -> str.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aRrrCol True 2
  }

extractTargetsEach :: RrrFunction
extractTargetsEach = let name = "extract_targets_each" in RrrFunction
  { fName      = name
  , fTypeCheck = tExtractEach
  , fDesc = Nothing, fTypeDesc  = name ++ " : <crb/bht>.list -> str.list.list"
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aRrrCol True 2
  }

aRrrCol :: Bool -> Int -> RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aRrrCol uniq n cfg ref _ [outPath, tsvPath] = do
  out <- wrappedCmdOut False True cfg ref [tsvPath'] [] [] "cut" ["-f", show n, tsvPath']
  let results = if uniq then sort $ nub $ lines out else lines out
  writeLits cfg ref outPath'' results
  where
    outPath'  = fromRrrPath cfg outPath
    outPath'' = debugA cfg "aRrrCol" outPath' [show n, outPath', tsvPath']
    tsvPath'  = fromRrrPath cfg tsvPath
aRrrCol _ _ _ _ _ _ = error "bad arguments to aRrrCol"

--------------------------
-- filter_evalue(_each) --
--------------------------

filterEvalue :: RrrFunction
filterEvalue = let name = "filter_evalue" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, bht] bht
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aFilterEvalue
  }

filterEvalueEach :: RrrFunction
filterEvalueEach = let name = "filter_evalue_each" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf bht] (ListOf bht)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMap 2 aFilterEvalue
  }

aFilterEvalue :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aFilterEvalue cfg ref _ [out, evalue, hits] = do
  wrappedCmdWrite False True cfg ref out'' [evalue', hits'] [] []
    "filter_evalue.R" [out', evalue', hits']
  where
    out'    = fromRrrPath cfg out
    out''   = debugA cfg "aFilterEvalue" out' [out', evalue', hits']
    evalue' = fromRrrPath cfg evalue
    hits'   = fromRrrPath cfg hits
aFilterEvalue _ _ _ args = error $ "bad argument to aFilterEvalue: " ++ show args

-------------------------------
-- get the best hit per gene --
-------------------------------

-- TODO move to BlastRBH?
-- TODO rename to just "best" and "best_each"?

bestHits :: RrrFunction
bestHits = let name = "best_hits" in RrrFunction
  { fName      = name 
  , fTypeCheck = defaultTypeCheck [bht] bht
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [bht] bht
  , fFixity    = Prefix
  , fRules     = rSimple aBestExtract
  }

bestHitsEach :: RrrFunction
bestHitsEach = let name = "best_hits_each" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf bht] (ListOf bht)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [ListOf bht] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rMap 1 aBestExtract
  }

aBestExtract :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aBestExtract cfg ref _ [out, hits] = do
  wrappedCmdWrite False True cfg ref out'' [hits'] [] [] "best_hits.R" [out', hits']
  where
    out'  = fromRrrPath cfg out
    out'' = debugA cfg "aBestExtract" out' [out', hits']
    hits' = fromRrrPath cfg hits
aBestExtract _ _ _ args = error $ "bad argument to aBestExtract: " ++ show args
