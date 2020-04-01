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

-- import OrthoLang.Core  (rMap)
import OrthoLang.Modules.BlastHits (aCutCol)
import OrthoLang.Core (rSimple)

dbg :: String -> String -> Action ()
dbg name = debugA ("ortholang.modules.scores." ++ name)

orthoLangModule :: Module
orthoLangModule = Module
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

aScores :: Config -> LocksRef -> Path -> Path -> Type -> Path -> Action ()
aScores cfg ref scoresPath othersPath othersType outPath = do
  scores <- readLits cfg ref $ fromPath cfg scoresPath
  others <- readStrings othersType cfg ref $ fromPath cfg othersPath
  let out' = fromPath cfg outPath
      rows = map (\(a,b) -> a ++ "\t" ++ b) $ zip scores others
  when (length scores /= length others) $ error $ unlines
     ["mismatched scores and others in aScores:", show scores, show others]
  debug' $ "aScores scores': " ++ show scores
  debug' $ "aScores others': " ++ show others
  debug' $ "aScores rows: "    ++ show rows
  writeLits cfg ref out' rows
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
  , fTypeCheck = tScoreRepeats
  , fTypeDesc  = name ++ " : <outputnum> <inputvar> <inputlist> -> <input>.scores"
  , fNewRules = Nothing, fOldRules = rScoreRepeats
  }
  where
    name = "score_repeats"

tScoreRepeats :: [Type] -> Either String Type
tScoreRepeats [n1, _, (ListOf n2)] | n1 == num && n2 == num = Right $ ScoresOf num
tScoreRepeats _ = Left "invalid args to scoreRepeats"

rScoreRepeats :: RulesFn
rScoreRepeats s@(scr, cfg, ref, _, dRef) expr@(Fun (ScoresOf t) salt deps _ as@(_:_:subList:[])) = do
  inputs <- rExpr s subList
  scores <- rExpr s repEachExpr
  let hack    = \(ExprPath p) -> toPath cfg p -- TODO remove! but how?
      inputs' = hack inputs
      scores' = hack scores
  outPath' %> \_ -> aScores cfg ref scores' inputs' t outPath
  return $ ExprPath $ outPath'
  where
    repEachExpr = Fun (ListOf t) salt deps "replace_each" as
    outPath  = exprPath cfg dRef scr expr
    outPath' = debugRules cfg "rScoreRepeats" expr $ fromPath cfg outPath
rScoreRepeats _ expr = error $ "bad argument to rScoreRepeats: " ++ show expr

----------------------------------
-- extract one col or the other --
----------------------------------

-- TODO deduplicate with extractQueries?
extractScores :: Function
extractScores = let name = "extract_scores" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = tExtractScores
  , fTypeDesc  = name ++ " : X.scores -> num.list"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple $ aCutCol False 1
  }

-- TODO deduplicate with extractTargets?
extractScored :: Function
extractScored = let name = "extract_scored" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = tExtractScored
  , fTypeDesc  = name ++ " : X.scores -> X.list"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple $ aCutCol False 2
  }

tExtractScores :: TypeChecker
tExtractScores [(ScoresOf _)]= Right $ ListOf num
tExtractScores _ = Left "extract_scores requires scores"

tExtractScored :: TypeChecker
tExtractScored [(ScoresOf x)] = Right $ ListOf x
tExtractScored _ = Left "extract_scored requires scores"

-- TODO _each versions?
