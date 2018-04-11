module ShortCut.Core.Compile.Map

  -- map an action over a list of inputs
  ( map1of1
  , map1of2, map2of2
  , map1of3, map2of3, map3of3

  -- concat a list of expressions
  , concatExprs

  )
  where

{- Haskell functions for making mapped versions of ShortCut functions.
 - Some are applied to Actions and some to Rules, but either way the result is
 - a higher-order RulesFn for use in the fRules field of a CutFunction.
 -}

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Paths

-----------------------------------------
-- map an action over a list of inputs --
-----------------------------------------

{- These take explicit path arguments rather than a [CutPath] in order to be
 - better-typed, now and in the future if I get around to making the whole AST
 - a GADT again.
 -}

type Action1 = CutConfig -> Locks -> CutPath -> Action ()
type Action2 = CutConfig -> Locks -> CutPath -> CutPath -> Action ()
type Action3 = CutConfig -> Locks -> CutPath -> CutPath -> CutPath -> Action ()

map1of1 :: Action1 -> Action1
map1of1 = undefined

map1of2 :: Action2 -> Action2
map1of2 = undefined

map2of2 :: Action2 -> Action2
map2of2 = undefined

map1of3 :: Action3 -> Action3
map1of3 = undefined

map2of3 :: Action3 -> Action3
map2of3 = undefined

map3of3 :: Action3 -> Action3
map3of3 = undefined

----------------------------------
-- concat a list of expressions --
----------------------------------

-- This goes well with any of the above map functions for writing a "concatMap"

concatExprs :: CutExpr -> CutExpr
concatExprs lst = case typeOf lst of
  (ListOf _) -> undefined
  x -> error $ "bad argument to concatExprs. type was " ++ show x
