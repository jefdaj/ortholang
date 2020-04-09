module OrthoLang.Modules.Scores where

{- Scores are lists of pairs of num and some other type "zipped" together.
 - They're are a little odd: not quite a core type because you only create and
 - use them with functions (no source code literal), but not quite modular
 - because they require some minor changes in Core to work.
 -
 - I couldn't figure out what a generic compiler should look like, so for now
 - they only have the corresponding Action; other modules do the prep work.
 -}

import Development.Shake
import OrthoLang.Core

import Control.Monad (when)
import OrthoLang.Core (readStrings, readLits, writeLits, debugA)
import OrthoLang.Core (rExpr, debugRules)
import OrthoLang.Core         (Path, toPath, fromPath, exprPath)
import Data.Maybe (fromJust)

-- import OrthoLang.Core  (rMap)
import OrthoLang.Modules.BlastHits (aCutCol)
import OrthoLang.Core (rSimple)

dbg :: String -> String -> Action ()
dbg name = debugA ("ortholang.modules.scores." ++ name)

olModule :: Module
olModule = Module
  { mName = "Scores"
  , mDesc = "Score repeated variables for plotting"
  , mTypes = []
  , mGroups = []
  , mFunctions =
    [ scoreRepeats
    , extractScores
    , extractScored
    ]
  }

------------
-- scores --
------------

aScores :: Path -> Path -> Type -> Path -> Action ()
aScores scoresPath othersPath othersType outPath = do
  cfg <- fmap fromJust getShakeExtra
  scores <- readLits $ fromPath cfg scoresPath
  others <- readStrings othersType $ fromPath cfg othersPath
  let out' = fromPath cfg outPath
      rows = map (\(a,b) -> a ++ "\t" ++ b) $ zip scores others
  when (length scores /= length others) $ error $ unlines
     ["mismatched scores and others in aScores:", show scores, show others]
  debug' $ "aScores scores': " ++ show scores
  debug' $ "aScores others': " ++ show others
  debug' $ "aScores rows: "    ++ show rows
  writeLits out' rows
  where
    debug' = dbg "aScores"

-----------------------------------------------------
-- replace_each and score the inputs by the outputs --
-----------------------------------------------------

-- (No need to score repeatN because it already produces a num.list)

scoreRepeats :: Function
scoreRepeats = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  -- , fTypeCheck = tScoreRepeats
  -- , fTypeDesc  = name ++ " : <outputnum> <inputvar> <inputlist> -> <input>.scores"
  , fInputs = [Exactly num, AnyType "the input type",
                  ListSigs (AnyType "the input type")]
  , fOutput =   ScoresSigs (AnyType "the input type") -- TODO does this break the current flattenAmbigTypes?
  , fNewRules = NewNotImplemented, fOldRules = rScoreRepeats
  }
  where
    name = "score_repeats"

-- (num, Some ot "any type", ListOf num) (ScoresOf num)
-- shown as "num t num.list -> num.scores, where t is any type"
-- tScoreRepeats :: [Type] -> Either String Type
-- tScoreRepeats [n1, _, (ListOf n2)] | n1 == num && n2 == num = Right $ ScoresOf num
-- tScoreRepeats _ = Left "invalid args to scoreRepeats"

rScoreRepeats :: RulesFn
rScoreRepeats scr expr@(Fun (ScoresOf t) salt deps _ as@(_:_:subList:[])) = do
  inputs <- rExpr scr subList
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let repEachExpr = Fun (ListOf t) salt deps "replace_each" as
      outPath  = exprPath cfg dRef scr expr
      outPath' = debugRules cfg "rScoreRepeats" expr $ fromPath cfg outPath
  scores <- rExpr scr repEachExpr
  let hack    = \(ExprPath p) -> toPath cfg p -- TODO remove! but how?
      inputs' = hack inputs
      scores' = hack scores
  outPath' %> \_ -> aScores scores' inputs' t outPath
  return $ ExprPath $ outPath'
rScoreRepeats _ expr = error $ "bad argument to rScoreRepeats: " ++ show expr

----------------------------------
-- extract one col or the other --
----------------------------------

-- TODO deduplicate with extractQueries?
extractScores :: Function
extractScores = let name = "extract_scores" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = tExtractScores
  -- , fTypeDesc  = name ++ " : X.scores -> num.list"
  , fInputs = [ScoresSigs (AnyType "the input type")]
  , fOutput = Exactly (ListOf num)
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rSimple $ aCutCol False 1
  }

-- TODO deduplicate with extractTargets?
extractScored :: Function
extractScored = let name = "extract_scored" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = tExtractScored
  -- , fTypeDesc  = name ++ " : X.scores -> X.list"
  , fInputs = [ScoresSigs (AnyType "the input type")]
  , fOutput =  ListSigs (AnyType "the input type")
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rSimple $ aCutCol False 2
  }

-- (ScoresOf (Some ot "any type")) (ListOf num)
-- shown as "t.scores -> num.list, where t is any type"
-- tExtractScores :: TypeChecker
-- tExtractScores [(ScoresOf _)]= Right $ ListOf num
-- tExtractScores _ = Left "extract_scores requires scores"

-- (ScoresOf (Some ot "any type")) (ListOf (SomeOt "any type"))
-- shown as "t.scores -> t.list, where t is any type"
-- tExtractScored :: TypeChecker
-- tExtractScored [(ScoresOf x)] = Right $ ListOf x
-- tExtractScored _ = Left "extract_scored requires scores"

-- TODO _each versions?
