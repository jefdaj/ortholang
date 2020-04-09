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
import OrthoLang.Core.Parse.Expr (typecheckFn) -- TODO move somewhere else
import OrthoLang.Core.Compile.NewRules
import OrthoLang.Core.Compile.Basic
import Data.Maybe (fromJust)
import Development.Shake -- (getShakeExtraRules)
import Control.Monad (when)
-- import OrthoLang.Core.Compile.Basic

--------------
-- compose1 --
--------------

-- `compose fn1 fn2` is kind of like `fn2 . fn1` in Haskell
-- TODO can it figure out the types automatically from fn1 and fn2?

compose1
  :: String   -- ^ overall function name
  -> Function -- ^ first function (takes inputs, returns intermediate)
  -> Function -- ^ second function (takes intermediate, returns output)
  -> Function -- ^ overall fn (runs fn1, then fn2 on its output)
compose1 name fn1 fn2 = newMacro name (fInputs fn1) (fOutput fn2) $ mCompose1 name fn1 fn2
  -- { fOpChar = Nothing, fName = name
  -- , fTypeCheck = tCompose1 fn1 type1 fn2
  -- , fTypeDesc  = desc
  -- , fOldRules = rCompose1 fn1 fn2
  -- , fNewRules = rCompose1 name fn1 fn2
  -- , fInputs = fInputs fn1
  -- , fOutput = fOutput fn2
  -- , fTags = []
  -- }

-- tCompose1 :: Function -> Type -> Function -> TypeChecker
-- tCompose1 fn1 expected fn2 types = case fTypeCheck fn1 types of
--   (Left  errMsg) -> Left errMsg
--   (Right actual) -> if actual == expected
--                       then fTypeCheck fn2 [expected]
--                       else Left $ "error: composed fn " ++ fName fn1
--                              ++ " produces a " ++ tExtOf actual
--                              ++ ", not " ++ tExtOf expected

checkCompose :: TypeSig -> [TypeSig] -> Bool
checkCompose (Some g1 _) [(Some g2 _)] = g1 == g2
checkCompose oSig1 [inSig2] = oSig1 == inSig2 -- TODO is actual equality OK here?
checkCompose _ _ = False

-- rCompose1 :: Function -> Function -> RulesFn
-- rCompose1 fn1 fn2 scr (Fun rtn2 salt deps _ args) = do
--   when (not $ checkCompose fn1 fn2) $
--     error "core.compile.compose.checkCompose" $ "bad argument: " ++ show inSigs2
--   cfg  <- fmap fromJust getShakeExtraRules
--   dRef <- fmap fromJust getShakeExtraRules
--   let expr1'  = Fun (fOutput fn1) salt deps (fName fn1) args
--       path1'  = ExprPath $ fromPath cfg $ exprPath cfg dRef scr expr1'
--       expr1'' = Com $ CompiledExpr (fOutput fn1) path1' $ (fOldRules fn1) scr expr1'
--       expr2   = Fun rtn2 salt deps (fName fn2) [expr1'']
--   (fOldRules fn2) scr expr2
-- rCompose1 _ _ _ _ = fail "bad argument to rCompose1"
  -- Fun Type Salt [Var] String [Expr]

err = error "core.compile.compose.mCompose1"

mCompose1 :: String -> Function -> Function -> MacroExpansion
mCompose1 n f1 f2 scr (Fun r2 salt deps n2 exprs)
  | n /= n2 = err $ "fn name mismatch: " ++ n ++ " /= " ++ n2
  | otherwise = case typecheckFn (fName f1) (fOutput f1) (fInputs f1) (map typeOf exprs) of
                  Left e -> err $ "fn type error: " ++ e
                  Right r1 -> if r1 /= r2
                                then err $ "fn type mismatch: " ++ show r1 ++ " /= " ++ show r2
                                else Fun r2 salt deps n [Fun r1 salt deps (fName f1) exprs]
mCompose1 _ _ _ _ e = error "core.compile.compose.mCompose1" $ "bad argument: " ++ show e

-- typecheckFn :: String -> TypeSig -> [TypeSig] -> [Type] -> Either String Type
-- typecheckFn name outSig inSigs inTypes =

-- newMacro :: String -> [TypeSig] -> TypeSig -> MacroExpansion -> Function
-- newMacro name iSigs oSig mFn = Function
--   { fOpChar    = Nothing
--   , fName      = name
--   -- , fTypeDesc  = mkTypeDesc name dTypes oType
--   -- , fTypeCheck = tFn
--   , fInputs    = iSigs
--   , fOutput    = oSig
--   , fTags      = []
--   , fOldRules  = undefined
--   , fNewRules  = NewMacro mFn
--   }
