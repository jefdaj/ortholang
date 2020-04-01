{-|
Haskell functions for making mapped versions of OrthoLang functions.
Some are applied to Actions and some to Rules, but either way the result is
a higher-order RulesFn for use in the fOldRules field of a Function.
-}

module OrthoLang.Core.Compile.Map2

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

import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Paths
import OrthoLang.Util (digest)
import Control.Monad (forM, forM_)
import OrthoLang.Core.Actions (readStrings, writeStrings, debugA)
import System.FilePath ((</>))
import OrthoLang.Core.Compile.Basic (rExpr, debugRules)

debugA' :: String -> String -> Action ()
debugA' name msg = debugA ("ortholang.core.compile.map2." ++ name) msg

-----------------------------------------
-- map an action over a list of inputs --
-----------------------------------------

{- These take explicit path arguments rather than a [Path] in order to be
 - better-typed, now and in the future when the whole AST will be a GADT.
 -}

-- TODO is forP OK here since there aren't any shared input files to conflict on locking?
--      might have to sort afterward, or is order automatically preserved?
-- TODO make sure hashes match the single versions or there will be trouble?
map1of1 :: Type -> Type -> Action1 -> Action1
map1of1 inType outType act1 cfg locks ids out a1 = do
  inPaths <- readStrings inType cfg locks $ fromPath cfg a1
  debugFn $ "a1: " ++ show a1
  debugFn $ "inPaths: " ++ show inPaths
  let tmpDir = mapCache cfg
  debugFn $ "tmpDir: " ++ show tmpDir
  outPaths <- forM inPaths $ \i -> do
    let o = tmpDir </> digest [out, toPath cfg i] </> "result" -- <.> extOf outType
    debugFn $ "o: " ++ show o
    act1 cfg locks ids (toPath cfg o) (toPath cfg i)
    return o
  debugFn $ "map1of1 outPaths: " ++ show outPaths
  debugFn $ "map1of1 out: " ++ show out
  writeStrings outType cfg locks (fromPath cfg out) outPaths
  where
    debugFn = debugA' "map1of1"

map1of2 :: Type -> Type -> Action2 -> Action2
map1of2 = undefined

map2of2 :: Type -> Type -> Action2 -> Action2
map2of2 = undefined

map1of3 :: Type -> Type -> Action3 -> Action3
map1of3 = undefined

-- TODO does this need to be more elaborate?
mapCache :: Config -> FilePath
mapCache cfg = cfgTmpDir cfg </> "cache" </> "map"

-- TODO seems like we could DRY out everything except have the mapped act3 function
--      inside each one take args in a different order? probably easiest to use the last 
--      like map2of3 would put its 2nd arg last:
--        map2of3 ... a1 a2 a3 = mapBase a1 a3 a2 act3'
--          where
--            act3' a3 a1 a2 = act3 a1 a2 a3
--      or as a one-liner:
--        map2of3 act3 a1 a2 a3 = mapBase a2 a1 a3 $ \a3' a1' a2' -> act3 a1' a2' a3'

map2of3 :: Type -> Type -> Action3 -> Action3
map2of3 inType outType act3 cfg locks ids out a1 a2 a3 = do
  inPaths <- readStrings inType cfg locks $ fromPath cfg a2
  let tmpDir   = mapCache cfg
      outPaths = (flip map) inPaths $ \i ->
                   tmpDir </> digest [out, toPath cfg i] </> "result" -- <.> extOf outType
      ioPairs  = zip inPaths outPaths
  -- TODO can this be done with forP in parallel? have to only do one overall read lock on input
  -- might need to pass a list of already-locked files to skip locking inside?
  forM_ ioPairs $ \(i,o) -> act3 cfg locks ids (toPath cfg o) a1 (toPath cfg i) a3
  writeStrings outType cfg locks (fromPath cfg out) outPaths

-- TODO fix this
-- map2of3 :: Type -> Type -> Action3 -> Action3
-- map2of3    inType outType act3  cfg locks out a1 a2 a3 =
--   map3Base inType outType act3' cfg locks out a1 a3 a2 -- move arg2 to the end
--   where
--     act3' o x z y = act3 o x y z -- and adjust the action fn to put it back

map3of3 :: Type -> Type -> Action3 -> Action3
map3of3 = map3Base -- because it's already the 3rd

map3Base :: Type -> Type -> Action3 -> Action3
map3Base inType outType act3 cfg locks ids out a1 a2 a3 = do
  -- debugA $ "map3Base arg paths: " ++ show [a1, a2, a3]

  -- this way breaks psiblast_db_each
  inPaths <- readStrings inType cfg locks $ fromPath cfg a3

  -- but this way breaks something too, right?
  -- a3path  <- readPath locks $ fromPath cfg a3
  -- debugA $ "map3Base a3path: " ++ show a3path
  -- inPaths <- readStrings inType cfg locks $ fromPath cfg a3path

  debugFn $ "map3Base inPaths read from a3: " ++ show inPaths
  let tmpDir   = cfgTmpDir cfg </> "cache" </> "map" -- TODO figure this out better
      outPaths = (flip map) inPaths $ \i -> tmpDir </> digest [out, toPath cfg i] </> "result" -- <.> extOf outType
      ioPairs  = zip inPaths outPaths
  -- debugFn $ "map3Base outPaths: " ++ show outPaths
  -- debugFn $ "map3Base out: " ++ show out
  -- forM_ ioPairs $ \(i,o) -> act3 cfg locks (toPath cfg o) a1 a2 (toPath cfg i)
  -- TODO can this be done with forP in parallel? have to only do one overall read lock on input
  forM_ ioPairs $ \(i,o) -> do
    debugFn $ "map3Base input and output: " ++ show i ++ ", " ++ show o
    act3 cfg locks ids (toPath cfg o) a1 a2 (toPath cfg i)
  writeStrings outType cfg locks (fromPath cfg out) outPaths
  where
    debugFn = debugA' "map3Base"

-- TODO match the single outpaths with unsafeExprPathExplicit! otherwise loooots of duplication
-- map3Base :: Type -> Type -> Action3 -> Action3
-- map3Base inType outType act3 cfg locks out a1 a2 a3 = do
--   debugA $ "map3Base arg paths: " ++ show [a1, a2, a3]
-- 
--   -- TODO is this right? read a3 to get one path, then that path to get the list?
--   a3path  <- readPath locks $ fromPath cfg a3
--   inPaths <- readStrings inType cfg locks $ fromPath cfg a3path
--   debugA $ "map3Base inPaths read from list: " ++ show inPaths
-- 
--   let tmpDir   = cfgTmpDir cfg </> "cache" </> "map" -- TODO figure this out better
--       outPaths = (flip map) inPaths $ \i -> tmpDir </> digest [out, toPath cfg i] <.> extOf outType
--       ioPairs  = zip inPaths outPaths
--   debugA $ "map3Base outPaths: " ++ show outPaths
--   debugA $ "map3Base out: " ++ show out
--   forM_ ioPairs $ \(i,o) -> do
--     debugA $ "map3Base input and output: " ++ show i ++ ", " ++ show o
--     act3 cfg locks (toPath cfg o) a1 a2 (toPath cfg i)
--   writeStrings outType cfg locks (fromPath cfg out) outPaths


----------------------------------
-- concat a list of expressions --
----------------------------------

-- This goes well with any of the above map functions for writing a "concatMap"

-- concatExprs :: Expr -> Expr
-- concatExprs lst = case typeOf lst of
--   (ListOf _) -> undefined
--   x -> error $ "bad argument to concatExprs. type was " ++ show x

------------------------------------------
-- base functions (move somewhere else) --
------------------------------------------

-- Compile a Function with 3 arguments
-- TODO is it really this simple? if so, replace everything with these! rFun1, rFun2...
-- TODO include the fn name when debugging
rFun1 :: Action1 -> RulesFn
rFun1 act1 st@(scr, cfg, ref, ids, dRef) expr@(Fun _ _ _ _ [a1]) = do
  (ExprPath arg1') <- rExpr st a1
  let arg1   = toPath cfg arg1'
      oPath  = exprPath cfg dRef scr expr
      oPath' = debugRules cfg "rFun1" expr $ fromPath cfg oPath
  oPath' %> \_ -> do
    debugFn $ "rFun1 arg1: "  ++ show arg1
    debugFn $ "rFun1 oPath: " ++ show oPath
    act1 cfg ref ids oPath arg1
  return $ ExprPath oPath'
  where
    debugFn = debugA' "rFun1"
rFun1 _ _ e = error $ "bad argument to rFun1: " ++ show e

-- Compile a Function with 3 arguments
-- TODO is it really this simple? if so, replace everything with these! rFun1, rFun2...
-- TODO include the fn name when debugging
rFun3 :: Action3 -> RulesFn
rFun3 act3 st@(scr, cfg, ref, ids, dRef) expr@(Fun _ _ _ _ [a1, a2, a3]) = do
  (ExprPath arg1') <- rExpr st a1
  (ExprPath arg2') <- rExpr st a2
  (ExprPath arg3') <- rExpr st a3
  let arg1   = toPath cfg arg1'
      arg2   = toPath cfg arg2'
      arg3   = toPath cfg arg3'
      oPath  = exprPath cfg dRef scr expr
      oPath' = debugRules cfg "rFun3" expr $ fromPath cfg oPath
  oPath' %> \_ -> do
    debugFn $ "rFun3 arg1: "  ++ show arg1
    debugFn $ "rFun3 arg2: "  ++ show arg2
    debugFn $ "rFun3 arg3: "  ++ show arg3
    debugFn $ "rFun3 oPath: " ++ show oPath
    act3 cfg ref ids oPath arg1 arg2 arg3
  return $ ExprPath oPath'
  where
    debugFn = debugA' "rFun3"
rFun3 _ _ e = error $ "bad argument to rFun3: " ++ show e

----------------
-- singletons --
----------------

singleton :: Expr -> Expr
singleton e = Lst (typeOf e) (saltOf e) (depsOf e) [e]
