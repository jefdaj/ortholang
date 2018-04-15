module ShortCut.Core.Compile.Compose
  ( compose1
  )
  where

{- Attempt to compose ShortCut functions. Work in progress, but promising!
 - TODO Can this be an applicative?
 -}

import ShortCut.Core.Types
-- import ShortCut.Core.Compile.Basic
-- import Debug.Trace

--------------
-- compose1 --
--------------

-- `compose fn1 fn2` is kind of lie `fn2 . fn1` in Haskell
-- TODO can it figure out the types automatically from fn1 and fn2?

-- TODO this type1 thing seems like a bad idea
compose1 :: CutFunction -> CutFunction
         -> String -> CutType -> String
         -> CutFunction
compose1 fn1 fn2 name type1 typeDesc = CutFunction
  { fName      = name
  , fTypeCheck = tCompose1 type1 fn1 fn2
  , fTypeDesc  = typeDesc
  , fFixity    = Infix
  , fRules     = rCompose1 fn1 fn2 type1
  }

tCompose1 :: CutType -> CutFunction -> CutFunction -> TypeChecker
tCompose1 expected fn1 fn2 types = case fTypeCheck fn1 types of
  (Left  errMsg) -> Left errMsg
  (Right actual) -> if actual == expected
                      then fTypeCheck fn2 [expected]
                      else Left $ "error: composed fn " ++ fName fn1
                             ++ " produces a " ++ extOf actual
                             ++ ", not " ++ extOf expected

rCompose1 :: CutFunction -> CutFunction -> CutType -> RulesFn
rCompose1 fn1 fn2 rtn1 st (CutFun rtn2 salt deps _ args) = (fRules fn2) st expr2
  where
    expr1'  = CutFun rtn1 salt deps (fName fn1) args
    expr1'' = CutRules $ CompiledExpr expr1' $ (fRules fn1) st expr1'
    expr2   = CutFun rtn2 salt deps (fName fn2) [expr1'']
rCompose1 _ _ _ _ _ = error "bad argument to rCompose1"
