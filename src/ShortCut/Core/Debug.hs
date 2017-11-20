module ShortCut.Core.Debug
  ( debug
  , debugShow
  , debugParser
  , debugRules
  , debugAction
  , debugHash
  , debugPath
  )
  where

-- import ShortCut.Core.Actions (wrappedCmd)
import ShortCut.Core.Pretty ()
import ShortCut.Core.Types
-- TODO no! import cycle... import ShortCut.Core.Paths (CutPath, fromCutPath)
import Text.PrettyPrint.HughesPJClass

import Debug.Trace       (trace, traceShow)

-- TODO add tags/description for filtering the output? (plus docopt to read them)
-- TODO rename to Shake.hs or something if it gathers more than debugging? combine with Eval.hs?

---------------------------------
-- basic wrappers around trace --
---------------------------------

debug :: CutConfig -> String -> a -> a
debug cfg msg rtn = if cfgDebug cfg then trace msg rtn else rtn

-- TODO stop exporting this in favor of the ones below?
debugShow :: Show a => CutConfig -> a -> b -> b
debugShow cfg shw rtn = if cfgDebug cfg then traceShow shw rtn else rtn

--------------------------------
-- debuggers for core modules --
--------------------------------

-- TODO are these all kind of the same fn? "debugExpr" or something

debugParser :: Pretty a => CutConfig -> String -> a -> a
debugParser cfg name res = debug cfg msg res
  where
    ren = render $ pPrint res
    msg = name ++ " parsed '" ++ ren ++ "'"

debugHash :: CutConfig -> String -> CutExpr -> String -> String
debugHash cfg name expr hash = debug cfg msg hash
  where
    ren = render $ pPrint expr
    msg = name ++ " hashed '" ++ ren ++ "' to " ++ hash

debugPath :: Show a => CutConfig -> String -> CutExpr -> a -> a
debugPath cfg name expr path = debug cfg msg path
  where
    ren = render $ pPrint expr
    msg = name ++ " for '" ++ ren ++ "' is " ++ show path -- TODO include types?

-- TODO restrict to CutExpr?
-- TODO put in rExpr to catch everything at once? but misses which fn was called
debugRules :: (Pretty a, Show b) => CutConfig -> String -> a -> b -> b
debugRules cfg name input output = debug cfg msg output
  where
    ren = render $ pPrint input
    msg = name ++ " compiled '" ++ ren ++ "' to " ++ show output

-- TODO put outPath last, here and in actual action fns
debugAction :: Show a => CutConfig -> String -> a -> [String] -> a
debugAction cfg name outPath args = debug cfg msg outPath
  where
    msg = name ++ " creating " ++ show outPath ++ " from " ++ show args

