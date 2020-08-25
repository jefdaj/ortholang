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


olModule :: Module
olModule = Module
  { mName = "CustomScript"
  , mDesc = "Run your own script and OrthoLang will assume the types are valid"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mFunctions = [customScript]
  }

-- customScript :: Function
-- customScript = Function
--   { fOpChar = Nothing, fName = "custom_script"
--   -- , fTypeCheck = cheatTypeCheck
--   -- , fTypeDesc  = "cheat : ??? (implement this)"
--   , fInputs = [Exactly str, ListSigs (Exactly Untyped)]
--   , fOutput = Exactly Untyped
--   , fTags = []
--   , fNewRules = NewNotImplemented
--   , fOldRules = rCustomScript
--   }

-- TODO detect return type based on string contents,
--      and make a new temporary type if the given one doesn't exist
-- TODO guess that requires either compile-time or runtime list of types?
--      do you keep a global runtime list of them, or have separate "cheat" types?
-- cheatTypeCheck :: [Type] -> Either String Type
-- cheatTypeCheck (script : rtype : _)
--   | script == str && rtype == str = findOrMake $ tExt rtype
--   where
--     findOrMake _ = undefined
-- cheatTypeCheck _ = Left $ "error! the first two arguments to cheat should \
--                           \be strings specifying the script path and return type"

-- rCustomScript :: RulesFn
-- rCustomScript = undefined

-- first attempt, based on FlowChart.hs

-- | Hidden function for rendering the raw Haskell Graphviz data structure passed as a string
customScript :: Function
customScript = newFnA2
  "custom_script"
  (Exactly str, ListSigs (Exactly Untyped))
  (Exactly Untyped)
  aCustomScript
  []

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

aCustomScript :: NewAction2
aCustomScript out inScript inList = do
  -- cfg <- fmap fromJust getShakeExtra
  -- let loc = "modules.customscript.aCustomScript"
      -- tmp  = fromPath loc cfg $ cacheDir cfg "custom_script" -- TODO bin cache? use script name? hash?
      -- ids  = tmp </> digest loc (toPath loc cfg inList) <.> "txt"
      -- ids' = toPath loc cfg ids
  -- TODO these should be the seqid_... ids themselves, not unhashed?
  -- unhashIDsFile (toPath loc cfg inList) ids -- TODO implement as a macro?
  aNewRulesS1 inScript id out inList -- TODO is it an S1 at this point? might need custom code
