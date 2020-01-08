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
import OrthoLang.Core.Types

import Control.Monad (when)
import OrthoLang.Core.Actions (readStrings, readLits, writeLits, debugA)
import OrthoLang.Core.Compile.Basic (rExpr, debugRules)
import OrthoLang.Core.Paths         (OrthoLangPath, toOrthoLangPath, fromOrthoLangPath, exprPath)

-- import OrthoLang.Core.Compile.Map  (rMap)
import OrthoLang.Modules.BlastHits (aOrthoLangCol)
import OrthoLang.Core.Compile.Basic (rSimple)

debug :: String -> String -> Action ()
debug name = debugA ("ortholang.modules.scores." ++ name)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
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

aScores :: OrthoLangConfig -> Locks -> OrthoLangPath -> OrthoLangPath -> OrthoLangType -> OrthoLangPath -> Action ()
aScores cfg ref scoresPath othersPath othersType outPath = do
  scores <- readLits cfg ref $ fromOrthoLangPath cfg scoresPath
  others <- readStrings othersType cfg ref $ fromOrthoLangPath cfg othersPath
  let out' = fromOrthoLangPath cfg outPath
      rows = map (\(a,b) -> a ++ "\t" ++ b) $ zip scores others
  when (length scores /= length others) $ error $ unlines
     ["mismatched scores and others in aScores:", show scores, show others]
  debug' $ "aScores scores': " ++ show scores
  debug' $ "aScores others': " ++ show others
  debug' $ "aScores rows: "    ++ show rows
  writeLits cfg ref out' rows
  where
    debug' = debug "aScores"

-----------------------------------------------------
-- replace_each and score the inputs by the outputs --
-----------------------------------------------------

-- (No need to score repeatN because it already produces a num.list)

scoreRepeats :: OrthoLangFunction
scoreRepeats = OrthoLangFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tScoreRepeats
  , fTypeDesc  = name ++ " : <outputnum> <inputvar> <inputlist> -> <input>.scores"
  , fRules     = rScoreRepeats
  }
  where
    name = "score_repeats"

tScoreRepeats :: [OrthoLangType] -> Either String OrthoLangType
tScoreRepeats [n1, _, (ListOf n2)] | n1 == num && n2 == num = Right $ ScoresOf num
tScoreRepeats _ = Left "invalid args to scoreRepeats"

rScoreRepeats :: OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rScoreRepeats s@(_, cfg, ref, _) expr@(OrthoLangFun (ScoresOf t) salt deps _ as@(_:_:subList:[])) = do
  inputs <- rExpr s subList
  scores <- rExpr s repEachExpr
  let hack    = \(ExprPath p) -> toOrthoLangPath cfg p -- TODO remove! but how?
      inputs' = hack inputs
      scores' = hack scores
  outPath' %> \_ -> aScores cfg ref scores' inputs' t outPath
  return $ ExprPath $ outPath'
  where
    repEachExpr = OrthoLangFun (ListOf t) salt deps "replace_each" as
    outPath  = exprPath s expr
    outPath' = debugRules cfg "rScoreRepeats" expr $ fromOrthoLangPath cfg outPath
rScoreRepeats _ expr = error $ "bad argument to rScoreRepeats: " ++ show expr

----------------------------------
-- extract one col or the other --
----------------------------------

-- TODO deduplicate with extractQueries?
extractScores :: OrthoLangFunction
extractScores = let name = "extract_scores" in OrthoLangFunction
  { fName      = name
  , fTypeCheck = tExtractScores
  , fTypeDesc  = name ++ " : X.scores -> num.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aOrthoLangCol False 1
  }

-- TODO deduplicate with extractTargets?
extractScored :: OrthoLangFunction
extractScored = let name = "extract_scored" in OrthoLangFunction
  { fName      = name
  , fTypeCheck = tExtractScored
  , fTypeDesc  = name ++ " : X.scores -> X.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aOrthoLangCol False 2
  }

tExtractScores :: TypeChecker
tExtractScores [(ScoresOf _)]= Right $ ListOf num
tExtractScores _ = Left "extract_scores requires scores"

tExtractScored :: TypeChecker
tExtractScored [(ScoresOf x)] = Right $ ListOf x
tExtractScored _ = Left "extract_scored requires scores"

-- TODO _each versions?
