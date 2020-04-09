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

-- | `compose fn1 fn2` is kind of like `fn2 . fn1` in Haskell
-- TODO can it figure out the types automatically from fn1 and fn2?
compose1
  :: String   -- ^ overall function name
  -> Function -- ^ first function (takes inputs, returns 1 intermediate file)
  -> Function -- ^ second function (takes intermediate file, produces output)
  -> Function -- ^ overall fn (runs fn1, then fn2 on its output)
compose1 name fn1 fn2 = newMacro name (fInputs fn1) (fOutput fn2) $ mCompose1 name fn1 fn2

-- TODO strictly evaluate functions when loading, so these all get revealed!
err :: a
err = error "core.compile.compose.mCompose1"

{-|
When this function recieves the initial expression it should be named like f2,
but have the proper input 'Expr's for f1. We fix that by creating an f1 'Expr'
around them and inserting it as the only input to f2. The 'Salt' and 'Var's
depended on should be correct for both already.

TODO try to factor some of this boilerplate out into rMacro
-}
mCompose1 :: String -> Function -> Function -> MacroExpansion
mCompose1 n f1 f2 _ (Fun r2 salt deps n2 exprs)
  | n /= n2 = err $ "function names don't match: '" ++ n ++ "' /= '" ++ n2 ++ "'"
  | otherwise = case typecheckFn (fName f1) (fOutput f1) (fInputs f2) (map typeOf exprs) of
    Left e -> err $ "typecheckFn error: '" ++ e ++ "'"
    Right r1 -> if r1 /= r2
      then err $ "return types don't match: " ++ show r1 ++ " /= " ++ show r2
      else let e1 = Fun r1 salt deps (fName f1) exprs
           in Fun r2 salt deps n [e1]
mCompose1 _ _ _ _ e = error "core.compile.compose.mCompose1" $ "bad argument: " ++ show e
