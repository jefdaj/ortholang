module ShortCut.Core.Compile.Repeat where

{- They're named similarly, but repeat and replace mean different things. The
 - Replace module copies the script and replaces a variable in each version of
 - it without changing the salts, so many times no real work will need to be
 - redone to get the new answer. Repeat (this module) is used when you want to
 - redo the same work multiple times. It's implemented by duplicating a
 - specific expression, changing its salt in each version, and passing those to
 - replace_each. So far this is the only thing the salts are ever used for.
 -}

-- TODO which parts of this should go in Core/Repeat.hs?
-- TODO debug transformations too!

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Replace (rReplaceEach)

import Data.Maybe      (fromJust)
import Data.Scientific (Scientific(), toBoundedInteger)

-- import Debug.Trace

cutModule :: CutModule
cutModule = CutModule
  { mName = "Repeat"
  , mDesc = "Repeatdly re-calculate variables using different random salts"
  , mTypes = []
  , mFunctions = [repeatN]
  }

-----------------------------------------------------
-- repeat without permutation (to test robustness) --
-----------------------------------------------------

repeatN :: CutFunction
repeatN = CutFunction
  { fName      = "repeat"
  , fFixity    = Prefix
  , fTypeCheck = tRepeatN
  , fTypeDesc  = "repeat : <outputvar> <inputvar> num -> <output>.list"
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
  rReplaceEach s (CutFun t salt deps name [resExpr, subVar, subList])
  where
    subExpr = fromJust $ lookup v scr
    nReps   = extractNum scr repsExpr
    subs    = take nReps $ zipWith setSalt [0..] (repeat subExpr) -- TODO is always starting from 0 right?
    -- subs    = zipWith setSalt (unfoldReplaceID salt nReps) (repeat subExpr)
    -- subs'   = trace ("rRepeatN salts: " ++ show (map saltOf subs)) subs
    subList = CutList (typeOf subExpr) salt (depsOf subExpr) subs -- TODO salt right?
rRepeatN _ _ = fail "bad argument to rRepeatN"
