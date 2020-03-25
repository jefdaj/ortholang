module OrthoLang.Modules.Repeat where

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
import OrthoLang.Core.Types
import OrthoLang.Core.Util (justOrDie)
import OrthoLang.Modules.Replace (rReplaceEach)

import Data.Scientific (Scientific(), toBoundedInteger)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "Repeat"
  , mDesc = "Repeatdly re-calculate variables using different random salts"
  , mTypes = []
  , mFunctions = [repeatN]
  }

-----------------------------------------------------
-- repeat without permutation (to test robustness) --
-----------------------------------------------------

repeatN :: OrthoLangFunction
repeatN = OrthoLangFunction
  { fNames     = ["repeat"]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = tRepeatN
  , fTypeDesc  = "repeat : <outputvar> <inputvar> num -> <output>.list"
  , fNewRules = Nothing, fOldRules = rRepeatN
  }

-- takes a result type, a starting type, and an int,
-- and returns a list of the result var type. start type can be whatever
-- TODO does num here refer to actual num, or is it shadowing it?
tRepeatN :: [OrthoLangType] -> Either String OrthoLangType 
tRepeatN [rType, _, n] | n == num = Right $ ListOf rType
tRepeatN _ = Left "invalid args to repeatN"

readSciInt :: String -> Int
readSciInt s = case toBoundedInteger (read s :: Scientific) of
  Nothing -> error $ "Not possible to repeat something " ++ s ++ " times."
  Just n  -> n

-- TODO is the bug here? might need to convert string -> sci -> int
extractNum :: OrthoLangScript -> OrthoLangExpr -> Int
extractNum _   (OrthoLangLit x _ n) | x == num = readSciInt n
extractNum scr (OrthoLangRef _ _ _ v) = extractNum scr $ justOrDie "extractNum failed!" $ lookup v scr
extractNum _ _ = error "bad argument to extractNum"

-- takes a result expression to re-evaluate, a variable to repeat and start from,
-- and a number of reps. returns a list of the result var re-evaluated that many times
-- can be read as "evaluate resExpr starting from subVar, repsExpr times"
-- TODO error if subVar not in (depsOf resExpr)
-- TODO is this how the salts should work?
rRepeatN :: RulesFn
rRepeatN s@(scr, _, _, _) (OrthoLangFun t salt deps name [resExpr, subVar@(OrthoLangRef _ _ _ v), repsExpr]) =
  rReplaceEach s (OrthoLangFun t salt deps name [resExpr, subVar, subList])
  where
    subExpr = justOrDie "lookup of subExpr in rRepeatN failed!" $ lookup v scr
    nReps   = extractNum scr repsExpr
    subs    = take nReps $ zipWith setSalt [0..] (repeat subExpr) -- TODO is always starting from 0 right?
    -- subs    = zipWith setSalt (unfoldReplaceID salt nReps) (repeat subExpr)
    -- subs'   = trace ("rRepeatN salts: " ++ show (map saltOf subs)) subs
    subList = OrthoLangList (typeOf subExpr) salt (depsOf subExpr) subs -- TODO salt right?
rRepeatN _ _ = fail "bad argument to rRepeatN"
