module OrthoLang.Modules.Repeat where

{- They're named similarly, but repeat and replace mean different things. The
 - Replace module copies the script and replaces a variable in each version of
 - it without changing the seeds, so many times no real work will need to be
 - redone to get the new answer. Repeat (this module) is used when you want to
 - redo the same work multiple times. It's implemented by duplicating a
 - specific expression, changing its seed in each version, and passing those to
 - replace_each. So far this is the only thing the seeds are ever used for.
 -}

-- TODO which parts of this should go in Core/Repeat.hs?
-- TODO debug transformations too!

import OrthoLang.Types
import OrthoLang.Interpreter

import OrthoLang.Modules.Replace (rReplaceEach)
import Data.Scientific           (Scientific(), toBoundedInteger)

olModule :: Module
olModule = Module
  { mName = "Repeat"
  , mDesc = "Repeatdly re-calculate variables using different random seeds"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mFunctions = [repeatN]
  }

-----------------------------------------------------
-- repeat without permutation (to test robustness) --
-----------------------------------------------------

-- the N keeps it from clashing with the Haskell repeat fn
repeatN :: Function
repeatN = Function
  { fOpChar = Nothing, fName = "repeat"
  ,fTags = []
  , fInputs = [ AnyType "the output type"
              , AnyType "the input type"
              , Exactly num
              ]
  , fOutput =  ListSigs (AnyType "the output type")
  , fNewRules = NewNotImplemented
  , fOldRules = rRepeatN
  }

-- takes a result type, a starting type, and an int,
-- and returns a list of the result var type. start type can be whatever
-- (Some ot "any type", num) (ListOf (Some ot "any type"))
-- shown as "t num -> t.list, where t is any type"
-- tRepeatN :: [Type] -> Either String Type 
-- tRepeatN [rType, _, n] | n == num = Right $ ListOf rType
-- tRepeatN _ = Left "invalid args to repeatN"

readSciInt :: String -> Int
readSciInt s = case toBoundedInteger (read s :: Scientific) of
  Nothing -> error $ "Not possible to repeat something " ++ s ++ " times."
  Just n  -> n

-- TODO move to Types?
extractNum :: Script -> Expr -> Int
extractNum _   (Lit x n) | x == num = readSciInt n
extractNum scr (Ref _ _ _ v) = extractNum scr $ justOrDie "extractNum failed!" $ lookupVar v (sAssigns scr)
extractNum _ _ = error "bad argument to extractNum"

-- OK, so this is expanding subList to a list of load_faa ... calls and passing those to rReplaceEach

-- takes a result expression to re-evaluate, a variable to repeat and start from,
-- and a number of reps. returns a list of the result var re-evaluated that many times
-- can be read as "evaluate resExpr starting from subVar, repsExpr times"
-- TODO error if subVar not in (depsOf resExpr)
-- TODO is this how the seeds should work?
rRepeatN :: RulesFn
rRepeatN       scr (Fun t mSeed deps name [resExpr, subVar@(Ref _ _ _ v), repsExpr]) =
  rReplaceEach scr (Fun t mSeed deps name [resExpr, subVar              , subList ])
  where
    subExpr = justOrDie "lookup of subExpr in rRepeatN failed!" $ lookupVar v (sAssigns scr)
    nReps   = extractNum scr repsExpr
    subs    = take nReps $ zipWith setSeed [0..] (repeat subExpr) -- TODO is always starting from 0 right?
    -- subs    = zipWith setSeed (unfoldRepID seed nReps) (repeat subExpr)
    -- subs'   = trace ("rRepeatN seeds: " ++ show (map seedOf subs)) subs
    subList = Lst (typeOf subExpr) (depsOf subExpr) subs -- TODO seed right?
rRepeatN _ _ = fail "bad argument to rRepeatN"
