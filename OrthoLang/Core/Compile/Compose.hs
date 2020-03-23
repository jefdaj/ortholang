module OrthoLang.Core.Compile.Compose
  ( compose1
  )
  where

{- Attempt to compose OrthoLang functions. Work in progress, but promising!
 - TODO Can this be an applicative?
 -}

import OrthoLang.Core.Types
import OrthoLang.Core.Paths (exprPath, fromOrthoLangPath)
-- import OrthoLang.Core.Compile.Basic

--------------
-- compose1 --
--------------

-- `compose fn1 fn2` is kind of like `fn2 . fn1` in Haskell
-- TODO can it figure out the types automatically from fn1 and fn2?

compose1 :: String      -- overall function name
         -> String      -- overall type description for :type command
         -> OrthoLangFunction -- first function (takes inputs, returns intermediate)
         -> OrthoLangType     -- intermediate type to be passed from fn1 to fn2
         -> OrthoLangFunction -- second function (takes intermediate, returns output)
         -> OrthoLangFunction -- overall fn (runs fn1, then fn2 on its output)
compose1 name desc fn1 type1 fn2 = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = tCompose1 fn1 type1 fn2
  , fNewRules = Nothing, fOldRules = rCompose1 fn1 type1 fn2
  , fTypeDesc  = desc
  , fFixity    = Prefix, fTags = []
  }

tCompose1 :: OrthoLangFunction -> OrthoLangType -> OrthoLangFunction -> TypeChecker
tCompose1 fn1 expected fn2 types = case fTypeCheck fn1 types of
  (Left  errMsg) -> Left errMsg
  (Right actual) -> if actual == expected
                      then fTypeCheck fn2 [expected]
                      else Left $ "error: composed fn " ++ head (fNames fn1)
                             ++ " produces a " ++ extOf actual
                             ++ ", not " ++ extOf expected

rCompose1 :: OrthoLangFunction -> OrthoLangType -> OrthoLangFunction -> RulesFn
rCompose1 fn1 rtn1 fn2 st@(_, cfg, _, _) (OrthoLangFun rtn2 salt deps _ args) = (fOldRules fn2) st expr2
  where
    expr1'  = OrthoLangFun rtn1 salt deps (head $ fNames fn1) args
    path1'  = ExprPath $ fromOrthoLangPath cfg $ exprPath st expr1'
    expr1'' = OrthoLangRules $ CompiledExpr rtn1 path1' $ (fOldRules fn1) st expr1'
    expr2   = OrthoLangFun rtn2 salt deps (head $ fNames fn2) [expr1'']
rCompose1 _ _ _ _ _ = fail "bad argument to rCompose1"
