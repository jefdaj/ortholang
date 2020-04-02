{-# LANGUAGE ScopedTypeVariables #-}

{-|
Attempt to compose OrthoLang functions. Work in progress, but promising!

TODO Can this be an applicative?
-}

module OrthoLang.Core.Compile.Compose
  ( compose1
  )
  where

import OrthoLang.Core.Types
import OrthoLang.Core.Paths (exprPath, fromPath)
import Data.Maybe (fromJust)
import Development.Shake -- (getShakeExtraRules)
-- import OrthoLang.Core.Compile.Basic

--------------
-- compose1 --
--------------

-- `compose fn1 fn2` is kind of like `fn2 . fn1` in Haskell
-- TODO can it figure out the types automatically from fn1 and fn2?

compose1 :: String      -- overall function name
         -> String      -- overall type description for :type command
         -> Function -- first function (takes inputs, returns intermediate)
         -> Type     -- intermediate type to be passed from fn1 to fn2
         -> Function -- second function (takes intermediate, returns output)
         -> Function -- overall fn (runs fn1, then fn2 on its output)
compose1 name desc fn1 type1 fn2 = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = tCompose1 fn1 type1 fn2
  , fNewRules = NewNotImplemented, fOldRules = rCompose1 fn1 type1 fn2
  , fTypeDesc  = desc
  ,fTags = []
  }

tCompose1 :: Function -> Type -> Function -> TypeChecker
tCompose1 fn1 expected fn2 types = case fTypeCheck fn1 types of
  (Left  errMsg) -> Left errMsg
  (Right actual) -> if actual == expected
                      then fTypeCheck fn2 [expected]
                      else Left $ "error: composed fn " ++ fName fn1
                             ++ " produces a " ++ extOf actual
                             ++ ", not " ++ extOf expected

rCompose1 :: Function -> Type -> Function -> RulesFn
rCompose1 fn1 rtn1 fn2 scr (Fun rtn2 salt deps _ args) = do
  (cfg  :: Config    ) <- fmap fromJust getShakeExtraRules
  (dRef :: DigestsRef) <- fmap fromJust getShakeExtraRules
  let expr1'  = Fun rtn1 salt deps (fName fn1) args
      path1'  = ExprPath $ fromPath cfg $ exprPath cfg dRef scr expr1'
      expr1'' = Com $ CompiledExpr rtn1 path1' $ (fOldRules fn1) scr expr1'
      expr2   = Fun rtn2 salt deps (fName fn2) [expr1'']
  (fOldRules fn2) scr expr2
rCompose1 _ _ _ _ _ = fail "bad argument to rCompose1"
