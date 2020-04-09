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
import Control.DeepSeq

-- | `compose fn1 fn2` is kind of like `fn2 . fn1` in Haskell
-- TODO can it figure out the types automatically from fn1 and fn2?
compose1
  :: String   -- ^ overall function name
  -> Function -- ^ first function (takes inputs, returns 1 intermediate file)
  -> Function -- ^ second function (takes intermediate file, produces output)
  -> Function -- ^ overall fn (runs fn1, then fn2 on its output)
compose1 name fn1 fn2 = newMacro name (fInputs fn1) (fOutput fn2) macro'
  where
    macro = mCompose1 fn1 fn2
    macro' = macro `deepseq` macro -- force composition errors immediately

err :: a
err = error "core.compile.compose.mCompose1"

{-|
When this function recieves the initial expression it should be named like f2,
but have the proper input 'Expr's for f1. We fix that by creating an f1 'Expr'
around them and inserting it as the only input to f2. The 'Salt' and 'Var's
depended on should be correct for both already. This checks that the first
`Function`'s return type matches the second `Function`'s only input, but
otherwise assumes the 'Expr' is set up properly. For example, it doesn't check
the names.

TODO try to factor some of this boilerplate out and make a generic mCompose
-}
mCompose1 :: Function -> Function -> MacroExpansion
mCompose1 f1 f2 _ (Fun r2 salt deps n2 es)

  -- first check that f2 expects one input
  | length (fInputs f2) /= 1 =
    let n = show (length (fInputs f2))
    in err $ fName f2 ++ "has " ++ n ++ " inputs, so you can't use compose1"
  | otherwise =

    -- then check that it matches the output of f1, so they could be composed
    case typecheckFn (fName f1) (fOutput f1) (fInputs f2) (map typeOf es) of
      Left e -> err $ "typecheckFn error: '" ++ e ++ "'"

      -- finally, check that the composed fn has the expected output type
      Right r1 -> if r1 /= r2
        then err $ "return types don't match: " ++ show r1 ++ " /= " ++ show r2

        -- if types look OK, make the composed function
        else let e1 = Fun r1 salt deps (fName f1) es
             in Fun r2 salt deps n2 [e1]

-- oh, and the very first check is that mCompose1 was given a Fun
mCompose1 _ _ _ e = err $ "bad argument: " ++ show e
