{-# LANGUAGE ScopedTypeVariables #-}

{-|
This is meant to replace the Map and Map2 modules in a way that allows mapping
over function-generated lists.

So, what would the ideal interface look like? It should probably operate at the
Action level for now rather than Rules. Seems much easier to grok. Problem is,
extractExprs seems to require being known in at Rules-time!

And how could it be implemented?

0. save the function expression, or a list of paths or something

1. need + read the input list that we're mapping over

2. substitute each path into the original expression/path and need that

3. gather the results into a final list

Maybe thinking in terms of the file paths will help...

Input list (the one mapped over) can be whatever it naturally is already, like:
exprs/load_faa_each/<hash>/<rep>/result

And that is the one whose digest should be in the MapHere, right?

Final output list should go in the result of whatever function it was applied to:
exprs/blastn_each/<hash>/<hash>/<hash>/<rep>/result etc

Ooooh possibly brilliant idea: instead of this whole "replace in the expr" dance,
can you just have it be called NeedExpr, and read what to need at Action time?
Have a Need "<digest here>" constructor and rExpr just returns its path.
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
  , NewMapTemplate
  , newMapTemplate
  , newMapRules

  )
  where

import Development.Shake
import OrthoLang.Util (digest)
import OrthoLang.Types
import OrthoLang.Interpreter.Paths (exprPath)
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

-- data MappedExpr = MappedExpr Type ExprPath (Rules ExprPath)

{-|
String that will be readable into a Fun Expr once  is
replaced with a suitable path. The idea is we can save it to a file, then read
back that file and fill in the template to create each of the mapped Exprs.

TODO does it always need to be a Fun?
-}
newtype NewMapTemplate = NewMapTemplate String
  deriving (Read, Show, Eq, Ord)

{-
Create a NewMapTemplate by getting the exprPath of the Expr, then replacing
one of the arg hashes. Note that the Expr is expected to have an invalid arg
type, so it is not possible to compile except with the newmap machinery.

The inserted MapHere will have a MapID derived from the Expr it replaces. That
eliminates confusion about which Exprs to replace with which elements later.

The thing to be mapped over should always be a list, so we set the map
placeholder type to the type of each list element.

TODO write another version of exprPath without the requirement for dRef?
-}
newMapTemplate :: Config -> DigestsRef -> Script -> Int -> Expr -> NewMapTemplate
newMapTemplate _ _ _ i _ | i < 0 = error "ortholang.interpreter.compile.newmap.newMapTemplate"
                                          $ "bad arg: " ++ show i

newMapTemplate cfg dRef scr i (Fun r ms ds n es) | length es > i = NewMapTemplate $ show fn'
  where
    exprToReplace = es !! i
    (ListOf eType) = typeOf exprToReplace
    mapid = MapID $ digest $ exprPath cfg dRef scr exprToReplace
    placeholder = Map eType (seedOf exprToReplace)
                            (depsOf exprToReplace)
                            (MapHere mapid)
    es' = replace es (i, placeholder)
    fn' = Fun r ms ds n es'

newMapTemplate _ _ _ _ e = error "ortholang.interpreter.compile.newmap.newMapTemplate"
                                  $ "bad arg: " ++ show e

-- replace the Nth element in a list
replace :: (Num a, Ord a) => [b] -> (a,b) -> [b]
replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
  if n < 0
    then (x:xs)
    else x: replace xs (n-1,a)

{-|
The "new map" machinery works like so:

1. Save a  "template" file, which holds a shown Expr with a MapHere inside
2. Save an "elements" file, which is a list of paths that should be substituted in
3. Read those two to create a list of new Exprs
4. Compile the new Exprs normally (via rExpr)

TODO can the elements file be removed? Should be able to tell by the fn call path

TODO wait why was this necessary at all? can you just extractExprs by needing it first?
-}
newMapRules :: Rules ()
newMapRules = do
  return ()
