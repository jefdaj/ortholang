{-# LANGUAGE ScopedTypeVariables #-}

{-|
The simplest way I can think to implement mapping is at the Action level, using
something similar to the macros above. The idea is that Shake can figure out
how to produce the mapped function elements, as long as we provide a list of
the paths we need.
-}

module OrthoLang.Interpreter.Compile.NewMap
  (

  -- * Interface
    newMap1of1
  , newMap1of2
  , newMap2of2
  , newMap1of3
  , newMap2of3
  , newMap3of3

  -- * Implementation
  , newMap1

  )
  where

import Prelude hiding (readList)
import Development.Shake
import OrthoLang.Util (digest)
import OrthoLang.Interpreter.Actions (need', readList, writePaths)
import OrthoLang.Types
import OrthoLang.Interpreter.Paths (exprPathExplicit, fromPath)
import OrthoLang.Interpreter.Compile.NewRules
import Data.Maybe (fromJust)


---------------
-- interface --
---------------

newMap1of1 :: String -> NewAction1 -> NewAction1
newMap1of1 prefix act o@(ExprPath o') l1' = newMap1 prefix act o l1'

newMap1of2 :: String -> NewAction2 -> NewAction2
newMap1of2 prefix act (ExprPath o) l1 p2 = undefined

newMap2of2 :: String -> NewAction2 -> NewAction2
newMap2of2 prefix act (ExprPath o) p1 l2 = undefined

newMap1of3 :: String -> NewAction3 -> NewAction3
newMap1of3 prefix act (ExprPath o) l1 p2 p3 = undefined

newMap2of3 :: String -> NewAction3 -> NewAction3
newMap2of3 prefix act (ExprPath o) p1 l2 p3 = undefined

newMap3of3 :: String -> NewAction3 -> NewAction3
newMap3of3 prefix act (ExprPath o) p1 p2 l3 = undefined


--------------------
-- implementation --
--------------------

-- this is everything we need to make a standard expr path:
--
-- exprPathExplicit :: Config -> String -> Maybe Seed -> [String] -> Path
-- exprPathExplicit cfg prefix mSeed hashes = toPath loc cfg path
--
-- could that be constructed based on nothing but the final outpath and a prefix?

-- we start with the single-list-only case, and will generalize if it works
newMap1 :: String -> NewAction1 -> NewAction1
newMap1 prefix actFn (ExprPath outPath) arg1Path = do
  let loc = "ortholang.interpreter.compile.newmap.newMap1"
  elems <- readList loc arg1Path -- TODO get the type and do readStrings instead?
  cfg <- fmap fromJust getShakeExtra
  let mSeed  = Nothing -- TODO how to get the seed??
  let mkPath p = exprPathExplicit cfg prefix mSeed [digest p]
  let outPaths = map mkPath elems -- TODO will fail on lits? paths?
      outPaths' = map (fromPath loc cfg) outPaths
  need' loc outPaths' -- TODO should this be needed later only? decides evaluation order
  writePaths loc outPath outPaths -- TODO will fail on lits?

-- calcTemplate :: ExprPath -> Int -> Action ExprPathTemplate
-- calcTemplate path index = do
  -- undefined
