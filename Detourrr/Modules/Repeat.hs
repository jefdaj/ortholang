module Detourrr.Modules.Repeat where

-- TODO which parts of this should go in Core/Repeat.hs?
-- TODO debug transformations too!

import Development.Shake
import Detourrr.Core.Types
import Detourrr.Core.Compile.Repeat

import Data.Maybe      (fromJust)
import Data.Scientific (Scientific(), toBoundedInteger)

-- import Debug.Trace

cutModule :: CutModule
cutModule = CutModule
  { mName = "Repeat"
  , mDesc = "Repeatdly re-calculate variables using different random seeds"
  , mTypes = []
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
  , fDesc = Nothing, fTypeDesc  = "repeat : <outputvar> <inputvar> num -> <output>.list"
  , fRules     = rRepeatN
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
-- TODO is this how the salts should work?
rRepeatN :: CutState -> CutExpr -> Rules ExprPath
rRepeatN s@(scr, _, _, _) (CutFun t salt deps name [resExpr, subVar@(CutRef _ _ _ v), repsExpr]) =
  rRepeatEach s (CutFun t salt deps name [resExpr, subVar, subList])
  where
    subExpr = fromJust $ lookup v scr
    nReps   = extractNum scr repsExpr
    subs    = zipWith setSalt [salt .. salt+nReps-1] (repeat subExpr)
    -- subs'   = trace ("rRepeatN salts: " ++ show (map saltOf subs)) subs
    subList = CutList (typeOf subExpr) salt (depsOf subExpr) subs -- TODO salt right?
rRepeatN _ _ = error "bad argument to rRepeatN"
