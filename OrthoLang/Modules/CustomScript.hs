{- This module lets you "cheat" the type system by calling your own script and
 - specifying the return type as a string. Why would you want that, when the
 - whole idea of OrthoLang is that it makes sure scripts fit together smoothly?
 - I can think of two situations in which it might be helpful:
 -
 - 1. You need to extend OrthoLang, but can't figure out Haskell or don't want
 -    to bother writing a module.
 -
 - 2. You are developing a OrthoLang module, but want to make sure the "useful"
 -    parts work before fiddling with the wrapper code. (This is me sometimes)
 -
 - More generally, I think it's important never to lock people into using a
 - specific set of abstractions. Especially in science! Odds are your research
 - includes complicated issues I can't forsee, and you might need an "escape
 - hatch" to get around my clever rules.
 -
 - Anyway, this provides one function called "cheat". It takes two strings: the
 - script itself (relative path, absolute path, or basename of something on
 - your PATH) and the name of the OrthoLang type to assign to the result.
 -
 - You can use a nonexistent OrthoLang type here if you want. The only problem
 - is it won't be usable by any other (non-cheat) functions afterward.
 -
 - After those two strings, any number of other arguments are allowed. Their
 - filenames will be passed to the script along with the standard return path
 - and temporary directory.
 -}

-- TODO write examples
-- TODO write cheatTypeCheck
-- TODO write rCustomScript
-- TODO come up with more neutral word than cheat?
-- TODO have a help menu that lists current types? not until after meeting!

module OrthoLang.Modules.CustomScript where

import Development.Shake
-- import OrthoLang.Types (typeError)
import OrthoLang.Types

import Development.Shake
import OrthoLang.Types
import OrthoLang.Script (rDepsOf)
import OrthoLang.Interpreter.Paths (prefixOf)
import OrthoLang.Interpreter
import OrthoLang.Util (digest, justOrDie)
import OrthoLang.Debug (error, trace)
import Prelude hiding (error)
import Data.GraphViz
import Data.Graph.Inductive hiding (nodes, edges)
import qualified Data.Graph.Inductive.Graph as G
import Data.GraphViz.Attributes.Complete
import Data.Maybe (fromJust)
import System.Directory (renameFile)
import System.FilePath (combine)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Data.List (sort, nub, isSuffixOf)
import OrthoLang.Modules.Plots (png)
import OrthoLang.Modules.Load (mkLoad)


olModule :: Module
olModule = Module
  { mName = "CustomScript"
  , mDesc = "Run your own script and OrthoLang will assume the types are valid"
  , mTypes = [scr]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [loadScript, runScriptRaw, runScript]
  }

scr :: Type
scr = Type
  { tExt  = "scr"
  , tDesc = "custom scripts"
  , tShow = defaultShow -- TODO what if it's binary? maybe use file command to show?
  }

loadScript :: Function
loadScript = mkLoad False "load_script" (Exactly scr)

--------------------
-- run_script_raw --
--------------------

runScriptRaw :: Function
runScriptRaw = hidden $ newFnA2
  "run_script_raw"
  (Exactly scr, ListSigs (Exactly Untyped))
  (Exactly Untyped)
  aRunScriptRaw
  [Hidden]

-- aPlotDot :: NewAction1
-- aPlotDot (ExprPath out) inDot = do
--   let loc = "ortholang.modules.flowchart.aPlotDot"
--   txt <- readLit loc inDot
--   cfg <- fmap fromJust $ getShakeExtra
--   let g = read txt :: DotGraph Node
--       out' = toPath loc cfg out
--   withBinHash out out' $ \tmpPath -> do
--     let tmpPath' = fromPath loc cfg tmpPath
--     renderPng tmpPath' g

aRunScriptRaw :: NewAction2
aRunScriptRaw out inScr inList = do
  -- let loc = "modules.customscript.aRunScriptRaw"
  aNewRulesS1 inScr id out inList

  -- cfg <- fmap fromJust getShakeExtra
      -- tmp  = fromPath loc cfg $ cacheDir cfg "run_script" -- TODO bin cache? use script name? hash?
      -- ids  = tmp </> digest loc (toPath loc cfg inList) <.> "txt"
      -- ids' = toPath loc cfg ids
  -- TODO these should be the seqid_... ids themselves, not unhashed?
  -- unhashIDsFile (toPath loc cfg inList) ids -- TODO implement as a macro?
  -- TODO with bin hash, since we don't know what the user will return?

----------------
-- run_script --
----------------

runScript :: Function
runScript = newExprExpansion
  "run_script"
  [Exactly str, ListSigs (Exactly Untyped)]
  (Exactly Untyped)
  mRunScript
  [ReadsFile]

mRunScript :: ExprExpansion
mRunScript _ _ (Fun r _ ds n [sStr, iList]) = Fun r Nothing ds "run_script_raw" [s, iList]
  where
    s = Fun scr Nothing ds "load_script" [sStr]
mRunScript _ _ e = error "modules.customscript.mRunScript" $ "bad argument: " ++ show e
