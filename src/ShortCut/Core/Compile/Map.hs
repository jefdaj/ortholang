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
import ShortCut.Core.Util (digest)
import ShortCut.Core.Actions (readStrings, writeStrings)
import Control.Monad (forM_)
import System.FilePath ((</>))

-----------------------------------------
-- map an action over a list of inputs --
-----------------------------------------

{- These take explicit path arguments rather than a [CutPath] in order to be
 - better-typed, now and in the future if I get around to making the whole AST
 - a GADT again.
 -}

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

-- TODO shit, need to come up with the outPaths separately because act3 returns ()?
map3of3 :: CutType -> CutType -> Action3 -> Action3
map3of3 inType outType act3 cfg locks out a1 a2 a3 = do
  inPaths <- readStrings inType cfg locks $ fromCutPath cfg a3
  let tmpDir   = cfgTmpDir cfg </> "cache" </> "map" -- TODO figure this out better
      outPaths = (flip map) inPaths $ \i -> tmpDir </> digest [out, toCutPath cfg i]
      ioPairs  = zip inPaths outPaths
  forM_ ioPairs $ \(i,o) -> act3 cfg locks (toCutPath cfg o) a1 a2 (toCutPath cfg i)
  writeStrings outType cfg locks (fromCutPath cfg out) outPaths

----------------------------------
-- concat a list of expressions --
----------------------------------

-- This goes well with any of the above map functions for writing a "concatMap"

concatExprs :: CutExpr -> CutExpr
concatExprs lst = case typeOf lst of
  (ListOf _) -> undefined
  x -> error $ "bad argument to concatExprs. type was " ++ show x
