module Detourrr.Core.Compile.Compose
  ( compose1
  )
  where

{- Attempt to compose Detourrr functions. Work in progress, but promising!
 - TODO Can this be an applicative?
 -}

import Detourrr.Core.Types
-- import Detourrr.Core.Compile.Basic
-- import Debug.Trace

--------------
-- compose1 --
--------------

-- `compose fn1 fn2` is kind of lie `fn2 . fn1` in Haskell
-- TODO can it figure out the types automatically from fn1 and fn2?

compose1 :: String      -- overall function name
         -> String      -- overall type description for :type command
         -> DtrFunction -- first function (takes inputs, returns intermediate)
         -> DtrType     -- intermediate type to be passed from fn1 to fn2
         -> DtrFunction -- second function (takes intermediate, returns output)
         -> DtrFunction -- overall fn (runs fn1, then fn2 on its output)
compose1 name desc fn1 type1 fn2 = DtrFunction
  { fName      = name
  , fTypeCheck = tCompose1 fn1 type1 fn2
  , fRules     = rCompose1 fn1 type1 fn2
  , fDesc = Nothing, fTypeDesc  = desc
  , fFixity    = Prefix
  }

tCompose1 :: DtrFunction -> DtrType -> DtrFunction -> TypeChecker
tCompose1 fn1 expected fn2 types = case fTypeCheck fn1 types of
  (Left  errMsg) -> Left errMsg
  (Right actual) -> if actual == expected
                      then fTypeCheck fn2 [expected]
                      else Left $ "error: composed fn " ++ fName fn1
                             ++ " produces a " ++ extOf actual
                             ++ ", not " ++ extOf expected

rCompose1 :: DtrFunction -> DtrType -> DtrFunction -> RulesFn
rCompose1 fn1 rtn1 fn2 st (DtrFun rtn2 salt deps _ args) = (fRules fn2) st expr2
  where
    expr1'  = DtrFun rtn1 salt deps (fName fn1) args
    expr1'' = DtrRules $ CompiledExpr expr1' $ (fRules fn1) st expr1'
    expr2   = DtrFun rtn2 salt deps (fName fn2) [expr1'']
rCompose1 _ _ _ _ _ = error "bad argument to rCompose1"
