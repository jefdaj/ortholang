module ShortCut.Core.Compile.Map

  -- map an action over a list of inputs
  ( map1of1
  , map1of2, map2of2
  , map1of3, map2of3, map3of3

  -- concat a list of expressions
  , concatExprs

  -- base functions (move somewhere else)
  , rFun3
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
import ShortCut.Core.Actions (readStrings, writeStrings, debugL)
import Control.Monad (forM_)
import System.FilePath ((</>), (<.>))
import ShortCut.Core.Compile.Basic (rExpr, debugRules)

-----------------------------------------
-- map an action over a list of inputs --
-----------------------------------------

{- These take explicit path arguments rather than a [CutPath] in order to be
 - better-typed, now and in the future if I get around to making the whole AST
 - a GADT again.
 -}

map1of1 :: CutType -> CutType -> Action1 -> Action1
map1of1 = undefined

map1of2 :: CutType -> CutType -> Action2 -> Action2
map1of2 = undefined

map2of2 :: CutType -> CutType -> Action2 -> Action2
map2of2 = undefined

map1of3 :: CutType -> CutType -> Action3 -> Action3
map1of3 = undefined

-- TODO does this need to be more elaborate?
mapCache :: CutConfig -> FilePath
mapCache cfg = cfgTmpDir cfg </> "cache" </> "map"

-- TODO seems like we could DRY out everything except have the mapped act3 function
--      inside each one take args in a different order? probably easiest to use the last 
--      like map2of3 would put its 2nd arg last:
--        map2of3 ... a1 a2 a3 = mapBase a1 a3 a2 act3'
--          where
--            act3' a3 a1 a2 = act3 a1 a2 a3
--      or as a one-liner:
--        map2of3 act3 a1 a2 a3 = mapBase a2 a1 a3 $ \a3' a1' a2' -> act3 a1' a2' a3'

-- map2of3 :: CutType -> CutType -> Action3 -> Action3
-- map2of3 inType outType act3 cfg locks out a1 a2 a3 = do
--   inPaths <- readStrings inType cfg locks $ fromCutPath cfg a2
--   let tmpDir   = mapCache cfg
--       outPaths = (flip map) inPaths $ \i ->
--                    tmpDir </> digest [out, toCutPath cfg i] <.> extOf outType
--       ioPairs  = zip inPaths outPaths
--   forM_ ioPairs $ \(i,o) -> act3 cfg locks (toCutPath cfg o) a1 (toCutPath cfg i) a3
--   writeStrings outType cfg locks (fromCutPath cfg out) outPaths

map2of3 :: CutType -> CutType -> Action3 -> Action3
map2of3    inType outType act3  cfg locks out a1 a2 a3 =
  map3Base inType outType act3' cfg locks out a1 a3 a2 -- move arg2 to the end
  where
    act3' o x z y = act3 o x y z -- and adjust the action fn to put it back

map3of3 :: CutType -> CutType -> Action3 -> Action3
map3of3 = map3Base -- because it's already the 3rd

map3Base :: CutType -> CutType -> Action3 -> Action3
map3Base inType outType act3 cfg locks out a1 a2 a3 = do
  -- debugL cfg $ "map3of3 arg paths: " ++ show [a1, a2, a3]
  inPaths <- readStrings inType cfg locks $ fromCutPath cfg a3
  -- debugL cfg $ "map3of3 inPaths read from a3: " ++ show inPaths
  let tmpDir   = cfgTmpDir cfg </> "cache" </> "map" -- TODO figure this out better
      outPaths = (flip map) inPaths $ \i -> tmpDir </> digest [out, toCutPath cfg i] <.> extOf outType
      ioPairs  = zip inPaths outPaths
  -- debugL cfg $ "map3of3 outPaths: " ++ show outPaths
  -- debugL cfg $ "map3of3 out: " ++ show out
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

------------------------------------------
-- base functions (move somewhere else) --
------------------------------------------

-- Compile a CutFunction with 3 arguments
-- TODO is it really this simple? if so, replace everything with these! rFun1, rFun2...
-- TODO include the fn name when debugging
rFun3 :: Action3 -> RulesFn
rFun3 act3 st@(_, cfg, ref) expr@(CutFun _ _ _ _ [a1, a2, a3]) = do
  (ExprPath arg1') <- rExpr st a1
  (ExprPath arg2') <- rExpr st a2
  (ExprPath arg3') <- rExpr st a3
  let arg1   = toCutPath cfg arg1'
      arg2   = toCutPath cfg arg2'
      arg3   = toCutPath cfg arg3'
      oPath  = exprPath st expr
      oPath' = debugRules cfg "rFun3" expr $ fromCutPath cfg oPath
  oPath' %> \_ -> act3 cfg ref oPath arg1 arg2 arg3
  return $ ExprPath oPath'
rFun3 _ _ e = error $ "bad argument to rFun3: " ++ show e
