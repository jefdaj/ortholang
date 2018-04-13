module ShortCut.Core.Compile.Compose
  ( compose1
  )
  where

{- Attempt to compose ShortCut functions. Work in progress, but promising!
 - TODO Can this be an applicative?
 -}

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic
import Debug.Trace

--------------
-- compose1 --
--------------

-- `compose fn1 fn2` is kind of lie `fn2 . fn1` in Haskell
-- TODO can it figure out the types automatically from fn1 and fn2?

compose1 :: CutFunction -> CutFunction
         -> String -> CutType -> TypeChecker -> String
         -> CutFunction
compose1 fn1 fn2 name type1 typeChecker typeDesc = CutFunction
  { fName      = name
  , fTypeCheck = typeChecker
  , fTypeDesc  = typeDesc
  , fFixity    = Infix
  , fRules     = rCompose1 fn1 fn2 type1
  }

rCompose1 :: CutFunction -> CutFunction -> CutType -> RulesFn
rCompose1 fn1 fn2 type1 st (CutFun rtn salt deps name args) = (fRules fn2) st expr2
  where
    expr1'  = CutFun type1 salt deps (fName fn1) args
    expr1'' = CutRules $ CompiledExpr expr1' $ (fRules fn1) st expr1'
    expr2   = CutFun rtn salt deps name [expr1'']
rCompose1 _ _ _ _ _ = error "bad argument to rCompose1"
