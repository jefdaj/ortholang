module ShortCut.Modules.Repeat where

-- TODO which parts of this should go in Core/Repeat.hs?
-- TODO debug transformations too!

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Repeat

import Data.Maybe      (fromJust)
import Data.Scientific (Scientific(), toBoundedInteger)

cutModule :: CutModule
cutModule = CutModule
  { mName = "repeat"
  , mFunctions =
    [ repeatEach -- TODO export this from the Core directly?
    , repeatN
    ]
  }

-----------------------------------------------------
-- repeat without permutation (to test robustness) --
-----------------------------------------------------

repeatN :: CutFunction
repeatN = CutFunction
  { fName      = "repeat"
  , fFixity    = Prefix
  , fTypeCheck = tRepeatN
  , fRules  = rRepeatN
  }

-- takes a result type, a starting type, and an int,
-- and returns a list of the result var type. start type can be whatever
-- TODO does num here refer to actual num, or is it shadowing it?
tRepeatN :: [CutType] -> Either String CutType 
tRepeatN [rType, _, n] | n == num = Right $ ListOf rType
tRepeatN _ = Left "invalid args to repeatN"

readSciInt :: String -> Int
readSciInt s = case toBoundedInteger (read s :: Scientific) of
  Nothing -> error $ "Not possible to repeat something " ++ s ++ " times."
  Just n  -> n

-- TODO is the bug here? might need to convert string -> sci -> int
extractNum :: CutScript -> CutExpr -> Int
extractNum _   (CutLit x _ n) | x == num = readSciInt n
extractNum scr (CutRef _ _ _ v) = extractNum scr $ fromJust $ lookup v scr
extractNum _ _ = error "bad argument to extractNum"

-- takes a result expression to re-evaluate, a variable to repeat and start from,
-- and a number of reps. returns a list of the result var re-evaluated that many times
-- can be read as "evaluate resExpr starting from subVar, repsExpr times"
-- TODO error if subVar not in (depsOf resExpr)
rRepeatN :: CutState -> CutExpr -> Rules ExprPath
rRepeatN s@(scr,_) (CutFun t salt deps name [resExpr, subVar@(CutRef _ _ _ v), repsExpr]) =
  rRepeatEach s (CutFun t salt deps name [resExpr, subVar, subList])
  where
    subExpr = fromJust $ lookup v scr
    nReps   = extractNum scr repsExpr
    subs    = zipWith setSalt [1..nReps] (repeat subExpr)
    subList = CutList (typeOf subExpr) 0 (depsOf subExpr) subs
rRepeatN _ _ = error "bad argument to rRepeatN"
