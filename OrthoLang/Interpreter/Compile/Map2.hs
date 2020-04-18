{-|
Haskell functions for making mapped versions of OrthoLang functions.
Some are applied to Actions and some to Rules, but either way the result is
a higher-order RulesFn for use in the fOldRules field of a Function.

This is in the process of being cleaned up and removed in favor of NewRules.
-}

module OrthoLang.Interpreter.Compile.Map2
  ( map3of3
  , rFun3
  )
  where

import Development.Shake
import OrthoLang.Interpreter.Types
import OrthoLang.Interpreter.Paths
import OrthoLang.Util (digest)
import Control.Monad (forM_)
import OrthoLang.Interpreter.Actions (readStrings, writeStrings, debugA)
import System.FilePath ((</>))
import OrthoLang.Interpreter.Compile.Basic (rExpr, debugRules)
import Data.Maybe (fromJust)

debugA' :: String -> String -> Action ()
debugA' name msg = debugA ("ortholang.core.compile.map2." ++ name) msg

{- These take explicit path arguments rather than a [Path] in order to be
 - better-typed, now and in the future when the whole AST will be a GADT.
 -}

map3of3 :: Type -> Type -> Action3 -> Action3
map3of3 inType outType act3 out a1 a2 a3 = do
  -- debugA $ "map3Base arg paths: " ++ show [a1, a2, a3]
  -- this way breaks psiblast_db_each
  cfg <- fmap fromJust getShakeExtra
  let loc = "core.compile.map2.map3of3"
  inPaths <- readStrings loc inType $ fromPath loc cfg a3
  -- but this way breaks something too, right?
  -- a3path  <- readPath locks $ fromPath loc cfg a3
  -- debugA $ "map3Base a3path: " ++ show a3path
  -- inPaths <- readStrings inType cfg locks $ fromPath loc cfg a3path
  debugFn $ "map3Base inPaths read from a3: " ++ show inPaths
  let tmpDir   = cfgTmpDir cfg </> "cache" </> "map" -- TODO figure this out better
      outPaths = (flip map) inPaths $ \i -> tmpDir </> digest [out, toPath loc cfg i] </> "result"
                 -- <.> tExtOf outType
      ioPairs  = zip inPaths outPaths
  forM_ ioPairs $ \(i,o) -> do
    debugFn $ "map3Base input and output: " ++ show i ++ ", " ++ show o
    act3 (toPath loc cfg o) a1 a2 (toPath loc cfg i)
  writeStrings loc outType (fromPath loc cfg out) outPaths
  where
    debugFn = debugA' "map3Base"

-- Compile a Function with 3 arguments
rFun3 :: Action3 -> RulesFn
rFun3 act3 scr expr@(Fun _ _ _ _ [a1, a2, a3]) = do
  (ExprPath arg1') <- rExpr scr a1
  (ExprPath arg2') <- rExpr scr a2
  (ExprPath arg3') <- rExpr scr a3
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "core.compile.map2.rFun3"
      arg1   = toPath loc cfg arg1'
      arg2   = toPath loc cfg arg2'
      arg3   = toPath loc cfg arg3'
      oPath  = exprPath cfg dRef scr expr
      oPath' = debugRules "rFun3" expr $ fromPath loc cfg oPath
  oPath' %> \_ -> do
    debugFn $ "rFun3 arg1: "  ++ show arg1
    debugFn $ "rFun3 arg2: "  ++ show arg2
    debugFn $ "rFun3 arg3: "  ++ show arg3
    debugFn $ "rFun3 oPath: " ++ show oPath
    act3 oPath arg1 arg2 arg3
  return $ ExprPath oPath'
  where
    debugFn = debugA' "rFun3"
rFun3 _ _ e = error $ "bad argument to rFun3: " ++ show e
