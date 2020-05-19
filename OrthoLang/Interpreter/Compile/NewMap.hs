{-# LANGUAGE ScopedTypeVariables #-}

{-|
This is meant to replace the Map and Map2 modules in a way that allows mapping
over function-generated lists.

So, what would the ideal interface look like? It should probably operate at the
Action level for now rather than Rules. Seems much easier to grok. Sooo...

And how could it be implemented?

0. save the function expression, or a list of paths or something

1. need + read the input list that we're mapping over

2. substitute each path into the original expression/path and need that

3. gather the results into a final list

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
  , newMapRules

  )
  where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter.Compile.NewRules


---------------
-- interface --
---------------

-- type NewAction1 = ExprPath -> FilePath                         -> Action ()
-- type NewAction2 = ExprPath -> FilePath -> FilePath             -> Action ()
-- type NewAction3 = ExprPath -> FilePath -> FilePath -> FilePath -> Action ()

type NewMap1 = ExprPath -> FilePath                         -> NewAction1
type NewMap2 = ExprPath -> FilePath -> FilePath             -> NewAction2
type NewMap3 = ExprPath -> FilePath -> FilePath -> FilePath -> NewAction3

newMap1of1 :: NewMap1
newMap1of1 (ExprPath o) l1 = undefined

newMap1of2 :: NewMap2
newMap1of2 (ExprPath o) l1 p2 = undefined

newMap2of2 :: NewMap2
newMap2of2 (ExprPath o) p1 l2 = undefined

newMap1of3 :: NewMap3
newMap1of3 (ExprPath o) l1 p2 p3 = undefined

newMap2of3 :: NewMap3
newMap2of3 (ExprPath o) p1 l2 p3 = undefined

newMap3of3 :: NewMap3
newMap3of3 (ExprPath o) p1 p2 l3 = undefined


--------------------
-- implementation --
--------------------

-- data CompiledExpr = CompiledExpr Type ExprPath (Rules ExprPath)

{-|
String that will be readable into a Fun Expr once <COMPILED_EXPR_PATH_HERE> is
replaced with a suitable path. The idea is we can save it to a file, then read
back that file and fill in the template to create each of the mapped Exprs.
-}
newtype NewMapFunTemplate = NewMapFunTemplate String
  deriving (Read, Show, Eq, Ord)

-- | Create a NewMapFunTemplate by getting the exprPath of the Expr, then
--   replacing one of the arg hashes. Note that the Expr is expected to have an
--   invalid arg type, so it is not possible to compile except with the newmap
--   machinery.
newMapFunTemplate :: Int -> Expr -> NewMapFunTemplate
newMapFunTemplate i e@(Fun r ms ds n es) | length es > i = undefined
newMapFunTemplate i e = error loc $ "bad arg: " ++ show e
  where
    loc = "ortholang.interpreter.compile.newmap.newMapFunTemplate"

newMapRules :: Rules ()
newMapRules = return ()
