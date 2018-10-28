module ShortCut.Core.Compile.Map2

-- TODO should output files in an identifyable folder by final fn name,
--      and link intermediate outputs from their single locations

  -- map an action over a list of inputs
  ( map1of1
  , map1of2, map2of2
  , map1of3, map2of3, map3of3

  -- concat a list of expressions
  -- , concatExprs

  -- base functions (move somewhere else)
  , rFun1, rFun3

  -- misc
  , singleton
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
import Control.Monad (forM, forM_)
import ShortCut.Core.Actions (readStrings, writeStrings, debugL)
import System.FilePath ((</>), (<.>))
import ShortCut.Core.Compile.Basic (rExpr, debugRules)

-----------------------------------------
-- map an action over a list of inputs --
-----------------------------------------

{- These take explicit path arguments rather than a [CutPath] in order to be
 - better-typed, now and in the future when the whole AST will be a GADT.
 -}

-- TODO is forP OK here since there aren't any shared input files to conflict on locking?
--      might have to sort afterward, or is order automatically preserved?
-- TODO make sure hashes match the single versions or there will be trouble?
map1of1 :: CutType -> CutType -> Action1 -> Action1
map1of1 inType outType act1 cfg locks out a1 = do
  inPaths <- readStrings inType cfg locks $ fromCutPath cfg a1
  debugL cfg $ "map1of1 a1: " ++ show a1
  debugL cfg $ "map1of1 inPaths: " ++ show inPaths
  let tmpDir = mapCache cfg
  debugL cfg $ "map1of1 tmpDir: " ++ show tmpDir
  outPaths <- forM inPaths $ \i -> do
    let o = tmpDir </> digest [out, toCutPath cfg i] <.> extOf outType
    debugL cfg $ "map1of1 o: " ++ show o
    act1 cfg locks (toCutPath cfg o) (toCutPath cfg i)
    return o
  debugL cfg $ "map1of1 outPaths: " ++ show outPaths
  debugL cfg $ "map1of1 out: " ++ show out
  writeStrings outType cfg locks (fromCutPath cfg out) outPaths

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

map2of3 :: CutType -> CutType -> Action3 -> Action3
map2of3 inType outType act3 cfg locks out a1 a2 a3 = do
  inPaths <- readStrings inType cfg locks $ fromCutPath cfg a2
  let tmpDir   = mapCache cfg
      outPaths = (flip map) inPaths $ \i ->
                   tmpDir </> digest [out, toCutPath cfg i] <.> extOf outType
      ioPairs  = zip inPaths outPaths
  -- TODO can this be done with forP in parallel? have to only do one overall read lock on input
  -- might need to pass a list of already-locked files to skip locking inside?
  forM_ ioPairs $ \(i,o) -> act3 cfg locks (toCutPath cfg o) a1 (toCutPath cfg i) a3
  writeStrings outType cfg locks (fromCutPath cfg out) outPaths

-- TODO fix this
-- map2of3 :: CutType -> CutType -> Action3 -> Action3
-- map2of3    inType outType act3  cfg locks out a1 a2 a3 =
--   map3Base inType outType act3' cfg locks out a1 a3 a2 -- move arg2 to the end
--   where
--     act3' o x z y = act3 o x y z -- and adjust the action fn to put it back

map3of3 :: CutType -> CutType -> Action3 -> Action3
map3of3 = map3Base -- because it's already the 3rd

map3Base :: CutType -> CutType -> Action3 -> Action3
map3Base inType outType act3 cfg locks out a1 a2 a3 = do
  -- debugL cfg $ "map3Base arg paths: " ++ show [a1, a2, a3]

  -- this way breaks psiblast_db_each
  inPaths <- readStrings inType cfg locks $ fromCutPath cfg a3

  -- but this way breaks something too, right?
  -- a3path  <- readPath cfg locks $ fromCutPath cfg a3
  -- debugL cfg $ "map3Base a3path: " ++ show a3path
  -- inPaths <- readStrings inType cfg locks $ fromCutPath cfg a3path

  debugL cfg $ "map3Base inPaths read from a3: " ++ show inPaths
  let tmpDir   = cfgTmpDir cfg </> "cache" </> "map" -- TODO figure this out better
      outPaths = (flip map) inPaths $ \i -> tmpDir </> digest [out, toCutPath cfg i] <.> extOf outType
      ioPairs  = zip inPaths outPaths
  -- debugL cfg $ "map3Base outPaths: " ++ show outPaths
  -- debugL cfg $ "map3Base out: " ++ show out
  -- forM_ ioPairs $ \(i,o) -> act3 cfg locks (toCutPath cfg o) a1 a2 (toCutPath cfg i)
  -- TODO can this be done with forP in parallel? have to only do one overall read lock on input
  forM_ ioPairs $ \(i,o) -> do
    debugL cfg $ "map3Base input and output: " ++ show i ++ ", " ++ show o
    act3 cfg locks (toCutPath cfg o) a1 a2 (toCutPath cfg i)
  writeStrings outType cfg locks (fromCutPath cfg out) outPaths

-- TODO match the single outpaths with exprPathExplicit! otherwise loooots of duplication
-- map3Base :: CutType -> CutType -> Action3 -> Action3
-- map3Base inType outType act3 cfg locks out a1 a2 a3 = do
--   debugL cfg $ "map3Base arg paths: " ++ show [a1, a2, a3]
-- 
--   -- TODO is this right? read a3 to get one path, then that path to get the list?
--   a3path  <- readPath cfg locks $ fromCutPath cfg a3
--   inPaths <- readStrings inType cfg locks $ fromCutPath cfg a3path
--   debugL cfg $ "map3Base inPaths read from list: " ++ show inPaths
-- 
--   let tmpDir   = cfgTmpDir cfg </> "cache" </> "map" -- TODO figure this out better
--       outPaths = (flip map) inPaths $ \i -> tmpDir </> digest [out, toCutPath cfg i] <.> extOf outType
--       ioPairs  = zip inPaths outPaths
--   debugL cfg $ "map3Base outPaths: " ++ show outPaths
--   debugL cfg $ "map3Base out: " ++ show out
--   forM_ ioPairs $ \(i,o) -> do
--     debugL cfg $ "map3Base input and output: " ++ show i ++ ", " ++ show o
--     act3 cfg locks (toCutPath cfg o) a1 a2 (toCutPath cfg i)
--   writeStrings outType cfg locks (fromCutPath cfg out) outPaths


----------------------------------
-- concat a list of expressions --
----------------------------------

-- This goes well with any of the above map functions for writing a "concatMap"

-- concatExprs :: CutExpr -> CutExpr
-- concatExprs lst = case typeOf lst of
--   (ListOf _) -> undefined
--   x -> error $ "bad argument to concatExprs. type was " ++ show x

------------------------------------------
-- base functions (move somewhere else) --
------------------------------------------

-- Compile a CutFunction with 3 arguments
-- TODO is it really this simple? if so, replace everything with these! rFun1, rFun2...
-- TODO include the fn name when debugging
rFun1 :: Action1 -> RulesFn
rFun1 act1 st@(_, cfg, ref) expr@(CutFun _ _ _ _ [a1]) = do
  (ExprPath arg1') <- rExpr st a1
  let arg1   = toCutPath cfg arg1'
      oPath  = exprPath st expr
      oPath' = debugRules cfg "rFun1" expr $ fromCutPath cfg oPath
  oPath' %> \_ -> do
    debugL cfg $ "rFun1 arg1: "  ++ show arg1
    debugL cfg $ "rFun1 oPath: " ++ show oPath
    act1 cfg ref oPath arg1
  return $ ExprPath oPath'
rFun1 _ _ e = error $ "bad argument to rFun1: " ++ show e

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
  oPath' %> \_ -> do
    debugL cfg $ "rFun3 arg1: "  ++ show arg1
    debugL cfg $ "rFun3 arg2: "  ++ show arg2
    debugL cfg $ "rFun3 arg3: "  ++ show arg3
    debugL cfg $ "rFun3 oPath: " ++ show oPath
    act3 cfg ref oPath arg1 arg2 arg3
  return $ ExprPath oPath'
rFun3 _ _ e = error $ "bad argument to rFun3: " ++ show e

----------------
-- singletons --
----------------

singleton :: CutExpr -> CutExpr
singleton e = CutList (typeOf e) (saltOf e) (depsOf e) [e]
