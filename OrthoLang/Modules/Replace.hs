{-|
This is some pretty awkward code, but I also think it's one of the coolest
features of OrthoLang! It conveniently answers the common question "What would
happen to my results if I changed variable X?" You can already edit and re-run
any code of course, but it tends to be something people don't take good notes
on. Having it built in to the language both documents and parallelizes
everything. It also makes it easy to plot the results so you don't have to
write your own R/python script for that (until you want publication-quality
figures).

It's implemented by:

1. copying the whole script once per variable substitution

2. "mangling" the var names in the copies so their variables don't conflict with existing ones

3. gathering a list of the final results into one final variable in the main namespace

The mangling thing is ugly, but relatively simple to implement and works well.
It results in a tree of "rep" symlinks like this:

reps
   ├── 312356c109
   │   ├── dep.num -> ../../exprs/multiply/653e971b5e_cd4761ce86_0.num
   │   ├── ind.num -> ../../exprs/num/89fb8b4aed_0.num
   │   └── result -> ../../reps/312356c109/dep.num
   ├── 4792af49ff
   │   ├── dep.num -> ../../exprs/multiply/653e971b5e_698967e992_0.num
   │   └── result -> ../../reps/4792af49ff/dep.num
   ├── 4ab901304c
   │   ├── dep.num -> ../../exprs/multiply/653e971b5e_538f7a4f17_0.num
   │   ├── ind.num -> ../../exprs/num/a629005e3c_0.num
   │   └── result -> ../../reps/4ab901304c/dep.num
   ├── 6eaa4d790a
   │   ├── dep.num -> ../../exprs/multiply/653e971b5e_e05aae5b00_0.num
   │   ├── ind.num -> ../../exprs/num/eaad7f2e40_0.num
   │   └── result -> ../../reps/6eaa4d790a/dep.num
   └── e8c54e2b5c
       ├── dep.num -> ../../exprs/multiply/653e971b5e_adf56c2208_0.num
       ├── ind.num -> ../../exprs/num/ea11b49459_0.num
       └── result -> ../../reps/e8c54e2b5c/dep.num

TODO is an initial var to start from really needed, or can that be the whole script?
-}

module OrthoLang.Modules.Replace where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Script (extractExprs, setRepIDs, calcRepID)
import OrthoLang.Interpreter
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "Replace"
  , mDesc = "Replace variables in the script to see how the results change"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mRules = []
  , mFunctions =
      [ replace
      , replaceEach
      ]
  }

-------------
-- replace --
-------------

-- TODO this is a good one to use when designing the fn help text
replace :: Function
replace = Function
  { fOpChar = Nothing, fName = "replace"
  , fTags = []
  , fInputs =
      [ AnyType "the return type"
      , AnyType "the type of the var to replace"
      , AnyType "the type of the var to replace"
      ]
  , fOutput = AnyType "the return type"
  , fOldRules = rReplace
  , fNewRules = NewNotImplemented
  }

-- (Some ot "the return type",
--  Some ot "the type of the var to replace",
--  Some ot "the type of the var to replace") (Some ot "the return type")
-- shown as "t1 t2 t2 -> t1, where t1 is the return type, t2 is the type of the var to replace"
-- TODO make a generic typegroup that matches anything, and gets represented as A, B, ...
-- tReplace :: [Type] -> Either String Type
-- tReplace (res:sub:sub':[]) | sub == sub' = Right res
-- tReplace _ = Left "invalid args to replace" -- TODO better errors here

-- TODO write this is a way that will make sense to other people
-- dReplace :: String
-- dReplace = "replace : <outputvar> <vartoreplace> <exprtoreplacewith> -> <newoutput>"

-- | Guards `rReplace'` against invalid arguments
rReplace :: RulesFn
rReplace st (Fun _ _ _ _ [resExpr, Ref _ _ _ subVar, subExpr]) = rReplace' st resExpr subVar subExpr
rReplace _ e = fail $ "bad argument to rReplace: " ++ show e

{-|
This does the actual replace operation. It takes an expression to edit, the
variable to replace, and an expression to put in its place. While it does that
it also filters to remove any irrelevant variables and sets a unique 'RepID' on
everything to keep the new copies of variables from conflicting with the
existing ones. The copies go in a separate "reps" folder named by the ID.

TODO any reason not to merge it into rReplace above?
-}
rReplace' :: Script
          -> Expr -- ^ the result expression for a single replacement, which *doesn't* contain all the info
          -> Var  -- ^ we also need the variable to be replaced (TODO could it be an expr?)
          -> Expr -- ^ and an expression to replace it with (which could be a ref to another variable)
          -> Rules ExprPath
rReplace' scr resExpr subVar@(Var _ _) subExpr = do
  let res   = Assign resultVar resExpr
      rRef  = Ref (typeOf resExpr) (seedOf resExpr) (depsOf resExpr) resultVar -- TODO is resultVar a dep too?
      sub   = Assign subVar subExpr
      deps  = filter (\a -> elem (aVar a) $ depsOf resExpr ++ depsOf subExpr) (sAssigns scr)
      newID = calcRepID scr resExpr subVar subExpr
      scr'  = setRepIDs newID $ Script
                { sAssigns = [sub] ++ deps ++ [res] -- TODO put sub deps before it?
                , sResult = Just rRef
                }
  (ResPath resPath) <- compileScript scr' -- TODO aha! this needs to adjust for RepIDs
  return (ExprPath resPath)

{- Helper function to write the final list when the results are literals
 - TODO factor out, and maybe unify with rListLits
 - TODO subPaths is only one path? if so, rename it
 - TODO what are the subpaths even used for?
 -}
aReplaceEachLits :: Type -> Path -> Path -> [Path] -> Action ()
aReplaceEachLits _ outPath subPaths resPaths = do
  cfg <- fmap fromJust getShakeExtra
  let outPath'  = fromPath loc cfg outPath
      subPaths' = fromPath loc cfg subPaths
      resPaths' = map (fromPath loc cfg) resPaths
      loc = "modules.summarize.aReplaceEachLits"
      out = traceA loc outPath' (outPath':subPaths':resPaths')
  lits <- mapM (readLit loc) resPaths'
  let lits' = map stripWhiteSpace lits
  writeLits loc out lits'

{- Helper function to write the final list when the results are links to files
 - TODO factor out, and maybe unify with rListLinks
 -}
aReplaceEachLinks :: Path -> Path -> [Path] -> Action ()
aReplaceEachLinks outPath subPaths resPaths = do
  cfg <- fmap fromJust getShakeExtra
  let outPath'  = fromPath loc cfg outPath
      subPaths' = fromPath loc cfg subPaths
      resPaths' = map (fromPath loc cfg) resPaths
      loc = "modules.summarize.aReplaceEachLinks"
      out = traceA loc outPath' (outPath':subPaths':resPaths')
  need' loc (subPaths':resPaths') -- TODO is needing subPaths required?

  -- this fixes load:faa.ol but breaks repeat:load.ol and repeat:replace_each_recursive.ol
  -- wait no, it doesn't fix that either lol
  -- mapM_ readPaths resPaths'

  writePaths loc out resPaths

------------------
-- replace_each --
------------------

replaceEach :: Function
replaceEach = Function
  { fOpChar = Nothing, fName = "replace_each"
  , fTags = []
  -- , fTypeCheck = tReplaceEach
  -- , fTypeDesc  = dReplaceEach
  , fInputs =
      [ AnyType "the return type"
      , AnyType "the type of the var to replace"
      , ListSigs (AnyType "the type of the var to replace")
      ]
  , fOutput = ListSigs (AnyType "the return type")
  , fNewRules = NewNotImplemented
  , fOldRules = rReplaceEach
  }

-- (Some ot "the return type",
--  Some ot "the type of the var to replace",
--  (ListOf (Some ot "the type of the var to replace")) (ListOf (Some ot "the return type"))
-- shown as "t1 t2 t2.list -> t1.list, where t1 is the return type, t2 is the type of the var to replace"
-- tReplaceEach :: [Type] -> Either String Type
-- tReplaceEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
-- tReplaceEach _ = Left "invalid args to replace_each" -- TODO better errors here

-- dReplaceEach :: String
-- dReplaceEach = "replace_each : <outputvar> <inputvar> <inputvars> -> <output>.list"

{- This takes a list expression that includes the variable to replace and a
 - list of things to replace it with. It calls rReplace' to do each replacement
 - operation, then gathers the results in a list.
 -
 - It has one major flaw that I'm trying to fix now: it assumes that the list
 - of expressions to substitute in are known at compile-time. That makes it
 - impossible to repeat based on the results of a function call, limiting it to
 - just lists you explicitly write in the ortholang code.
 -
 - The initial rewrite plan was:
 - 1) separate out aReplaceEach for clarity
 - 2) the main goal here is to replace extractExprs with an equivalent that evaluates the list and returns its paths
 -    that needs to run in Action so it can use IO
 -    and because of that so does the rReplace' call? is that even possible?
 - 3) then given paths we can build MappedExprs using the known types
 -
 - But then I remembered Core, which might be able to help:
 - rMap :: Int -> ([Path] -> Action ()) -> RulesFn
 - type Action1  = Path -> Path -> Action ()
 - you give it a single function and an index for the argument to map over, and it does everything
 - but it only works on Action functions, so it will take some adapting to use here
 - it does seem promising though: write replace and get a proper replace_each almost for free?
 -
 - So the new plan is relatively simple: implement replace first, and then try replace_each again.
 - I'm not that sure the rMap thing will work because it deals with Actions though.
 -}
rReplaceEach :: Script
             -> Expr -- the final result expression, which contains all the info we need
             -> Rules ExprPath
rReplaceEach scr expr@(Fun _ _ _ _ [resExpr, Ref _ _ _ subVar, subList]) = do
  subPaths <- rExpr scr subList

  -- TODO how to replace this with the NewMap machinery?
  -- It needs to handle fn calls + regular lists
  -- This does seem amenable to being phrased as "mapping over a list whose elements aren't known yet" right?
  -- Could you just do that and put the results in resPaths?
  let subExprs = extractExprs scr subList
  resPaths <- mapM (rReplace' scr resExpr subVar) subExprs

  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "modules.replace.rReplaceEach"
      subPaths' = (\(ExprPath p) -> toPath loc cfg p) subPaths
      resPaths' = map (\(ExprPath p) -> toPath loc cfg p) resPaths
      outPath   = exprPath cfg dRef scr expr
      outPath'  = debugRules loc expr $ fromPath loc cfg outPath
  outPath' %> \_ ->
    let actFn = if typeOf expr `elem` [ListOf str, ListOf num]
                  then aReplaceEachLits (typeOf expr)
                  else aReplaceEachLinks
    in actFn outPath subPaths' resPaths'
  return (ExprPath outPath')
rReplaceEach _ expr = fail $ "bad argument to rReplaceEach: " ++ show expr
