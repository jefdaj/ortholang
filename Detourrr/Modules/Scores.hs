module Detourrr.Modules.Scores where

{- Scores are lists of pairs of num and some other type "zipped" together.
 - They're are a little odd: not quite a core type because you only create and
 - use them with functions (no source code literal), but not quite modular
 - because they require some minor changes in Core to work.
 -
 - I couldn't figure out what a generic compiler should look like, so for now
 - they only have the corresponding Action; other modules do the prep work.
 -}

import Development.Shake
import Detourrr.Core.Types

import Control.Monad (when)
import Detourrr.Core.Actions (readStrings, readLits, writeLits, debugL)
import Detourrr.Core.Compile.Basic (rExpr, debugRules)
import Detourrr.Core.Paths         (RrrPath, toRrrPath, fromRrrPath, exprPath)

-- import Detourrr.Core.Compile.Map  (rMap)
import Detourrr.Modules.BlastHits (aRrrCol)
import Detourrr.Core.Compile.Basic (rSimple)

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "Scores"
  , mDesc = "Score repeated variables for plotting"
  , mTypes = []
  , mFunctions =
    [ scoreRepeats
    , extractScores
    , extractScored
    ]
  }

------------
-- scores --
------------

aScores :: RrrConfig -> Locks -> RrrPath -> RrrPath -> RrrType -> RrrPath -> Action ()
aScores cfg ref scoresPath othersPath othersType outPath = do
  scores <- readLits cfg ref $ fromRrrPath cfg scoresPath
  others <- readStrings othersType cfg ref $ fromRrrPath cfg othersPath
  let out' = fromRrrPath cfg outPath
      rows = map (\(a,b) -> a ++ "\t" ++ b) $ zip scores others
  when (length scores /= length others) $ error $ unlines
     ["mismatched scores and others in aScores:", show scores, show others]
  debugL cfg $ "aScores scores': " ++ show scores
  debugL cfg $ "aScores others': " ++ show others
  debugL cfg $ "aScores rows: "    ++ show rows
  writeLits cfg ref out' rows

-----------------------------------------------------
-- repeat_each and score the inputs by the outputs --
-----------------------------------------------------

-- (No need to score repeatN because it already produces a num.list)

scoreRepeats :: RrrFunction
scoreRepeats = RrrFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tScoreRepeats
  , fDesc = Nothing, fTypeDesc  = name ++ " : <outputnum> <inputvar> <inputlist> -> <input>.scores"
  , fRules     = rScoreRepeats
  }
  where
    name = "score_repeats"

tScoreRepeats :: [RrrType] -> Either String RrrType
tScoreRepeats [n1, _, (ListOf n2)] | n1 == num && n2 == num = Right $ ScoresOf num
tScoreRepeats _ = Left "invalid args to scoreRepeats"

rScoreRepeats :: RrrState -> RrrExpr -> Rules ExprPath
rScoreRepeats s@(_, cfg, ref, _) expr@(RrrFun (ScoresOf t) salt deps _ as@(_:_:subList:[])) = do
  inputs <- rExpr s subList
  scores <- rExpr s repEachExpr
  let hack    = \(ExprPath p) -> toRrrPath cfg p -- TODO remove! but how?
      inputs' = hack inputs
      scores' = hack scores
  outPath' %> \_ -> aScores cfg ref scores' inputs' t outPath
  return $ ExprPath $ outPath'
  where
    repEachExpr = RrrFun (ListOf t) salt deps "repeat_each" as
    outPath  = exprPath s expr
    outPath' = debugRules cfg "rScoreRepeats" expr $ fromRrrPath cfg outPath
rScoreRepeats _ expr = error $ "bad argument to rScoreRepeats: " ++ show expr

----------------------------------
-- extract one col or the other --
----------------------------------

-- TODO deduplicate with extractQueries?
extractScores :: RrrFunction
extractScores = let name = "extract_scores" in RrrFunction
  { fName      = name
  , fTypeCheck = tExtractScores
  , fDesc = Nothing, fTypeDesc  = name ++ " : X.scores -> num.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aRrrCol False 1
  }

-- TODO deduplicate with extractTargets?
extractScored :: RrrFunction
extractScored = let name = "extract_scored" in RrrFunction
  { fName      = name
  , fTypeCheck = tExtractScored
  , fDesc = Nothing, fTypeDesc  = name ++ " : X.scores -> X.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aRrrCol False 2
  }

tExtractScores :: TypeChecker
tExtractScores [(ScoresOf _)]= Right $ ListOf num
tExtractScores _ = Left "extract_scores requires scores"

tExtractScored :: TypeChecker
tExtractScored [(ScoresOf x)] = Right $ ListOf x
tExtractScored _ = Left "extract_scored requires scores"

-- TODO _each versions?