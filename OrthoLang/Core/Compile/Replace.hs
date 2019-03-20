module OrthoLang.Core.Compile.Replace where

{- This is some pretty awkward code, but I also think it's one of the coolest
 - features of OrthoLang! It conveniently answers the common question "What
 - would happen to my results if I changed variable X?" You can already edit
 - and re-run any code of course, but it tends to be something people don't
 - take good notes on. Having it built in to the language both documents and
 - parallelizes everything. It also makes it easy to plot the results so you
 - don't have to write your own R/python script for that (until you want
 - publication-quality figures).
 -
 - It's implemented by:
 - 1) copying the whole script once per variable substitution
 - 2) "mangling" the file names in the copies so their variables don't conflict with existing ones
 - 3) gathering a list of the final results into one final variable in the main namespace
 -
 - The mangling thing is ugly, but relatively simple to implement and works well.
 - It results in a tree of "rep" symlinks like this:
 -
 - reps
 -    ├── 312356c109
 -    │   ├── dep.num -> ../../exprs/multiply/653e971b5e_cd4761ce86_0.num
 -    │   ├── ind.num -> ../../exprs/num/89fb8b4aed_0.num
 -    │   └── result -> ../../reps/312356c109/dep.num
 -    ├── 4792af49ff
 -    │   ├── dep.num -> ../../exprs/multiply/653e971b5e_698967e992_0.num
 -    │   └── result -> ../../reps/4792af49ff/dep.num
 -    ├── 4ab901304c
 -    │   ├── dep.num -> ../../exprs/multiply/653e971b5e_538f7a4f17_0.num
 -    │   ├── ind.num -> ../../exprs/num/a629005e3c_0.num
 -    │   └── result -> ../../reps/4ab901304c/dep.num
 -    ├── 6eaa4d790a
 -    │   ├── dep.num -> ../../exprs/multiply/653e971b5e_e05aae5b00_0.num
 -    │   ├── ind.num -> ../../exprs/num/eaad7f2e40_0.num
 -    │   └── result -> ../../reps/6eaa4d790a/dep.num
 -    └── e8c54e2b5c
 -        ├── dep.num -> ../../exprs/multiply/653e971b5e_adf56c2208_0.num
 -        ├── ind.num -> ../../exprs/num/ea11b49459_0.num
 -        └── result -> ../../reps/e8c54e2b5c/dep.num
 -
 -TODO is an initial var to start from really needed, or can that be the whole script?
 -}

import Development.Shake
import OrthoLang.Core.Types

import OrthoLang.Core.Actions       (writeLits, writePaths, readLit, traceA)
import OrthoLang.Core.Paths         (exprPath, fromOrthoLangPath,
                                    OrthoLangPath, toOrthoLangPath, fromOrthoLangPath)
import OrthoLang.Core.Compile.Basic (rExpr, compileScript, debugRules)
import OrthoLang.Core.Util          (digest, stripWhiteSpace)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "Replace"
  , mDesc = "Replace variables in the script to see how the results change"
  , mTypes = []
  , mFunctions =
      [ replace
      , replaceEach
      , replaceEach2
      ]
  }

---------------------------
-- mangle variable names --
---------------------------

{- This does the filename mangling by setting a "replace ID" in each variable
 - in a script. If it's anything other than Nothing it gets used by
 - OrthoLang.Core.Paths.varPath to set the rep dir.
 -
 - TODO should it go in Types, or maybe Paths?
 -}

mangleExpr :: (OrthoLangVar -> OrthoLangVar) -> OrthoLangExpr -> OrthoLangExpr
mangleExpr _ e@(OrthoLangLit  _ _ _) = e
mangleExpr fn (OrthoLangRef  t n vs v      ) = OrthoLangRef  t n (map fn vs)   (fn v)
mangleExpr fn (OrthoLangBop  t n vs s e1 e2) = OrthoLangBop  t n (map fn vs) s (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (OrthoLangFun  t n vs s es   ) = OrthoLangFun  t n (map fn vs) s (map (mangleExpr fn) es)
mangleExpr fn (OrthoLangList t n vs   es   ) = OrthoLangList t n (map fn vs)   (map (mangleExpr fn) es)
mangleExpr _ (OrthoLangRules _) = error "implement this!"

mangleAssign :: (OrthoLangVar -> OrthoLangVar) -> OrthoLangAssign -> OrthoLangAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (OrthoLangVar -> OrthoLangVar) -> OrthoLangScript -> OrthoLangScript
mangleScript fn = map (mangleAssign fn)

setReplaceID :: ReplaceID -> OrthoLangVar -> OrthoLangVar
setReplaceID newID (OrthoLangVar _ name) = OrthoLangVar newID name

setReplaceIDs :: ReplaceID -> OrthoLangScript -> OrthoLangScript
setReplaceIDs newID = mangleScript (setReplaceID newID)

-------------
-- replace --
-------------

replace :: OrthoLangFunction
replace = OrthoLangFunction
  { fNames     = ["replace"]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = tReplace
  , fTypeDesc  = dReplace
  , fRules     = rReplace
  }

tReplace :: [OrthoLangType] -> Either String OrthoLangType
tReplace (res:sub:sub':[]) | sub == sub' = Right res
tReplace _ = Left "invalid args to replace" -- TODO better errors here

-- TODO write this is a way that will make sense to other people
dReplace :: String
dReplace = "replace : <outputvar> <vartoreplace> <exprtoreplacewith> -> <newoutput>"

rReplace :: OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rReplace st (OrthoLangFun _ _ _ _ (resExpr:(OrthoLangRef _ _ _ subVar):subExpr:[])) = rReplace' st resExpr subVar subExpr
rReplace _ e = fail $ "bad argument to rReplace: " ++ show e

{- This does the actual replace operation. It takes an expression to edit, the
 - variable to replace, and an expression to put in its place. While it does
 - that it also filters to remove any irrelevant variables and sets a unique
 - "replace ID" on everything to keep the new copies of variables from
 - conflicting with the existing ones. The copies go in a separate "reps"
 - folder named by the ID.
 -
 - TODO any reason not to merge it into rReplace above?
 -}
rReplace' :: OrthoLangState
          -> OrthoLangExpr -- the result expression for a single replacement, which *doesn't* contain all the info
          -> OrthoLangVar  -- we also need the variable to be replaced
          -> OrthoLangExpr -- and an expression to replace it with (which could be a ref to another variable)
          -> Rules ExprPath
rReplace' st@(script, cfg, ref, ids) resExpr subVar@(OrthoLangVar _ _) subExpr = do
  let res   = (OrthoLangVar (ReplaceID Nothing) "result", resExpr)
      sub   = (subVar, subExpr)
      deps  = filter (\(v,_) -> (elem v $ depsOf resExpr ++ depsOf subExpr)) script
      newID = calcReplaceID st resExpr subVar subExpr
      scr'  = (setReplaceIDs newID ([sub] ++ deps ++ [res]))
  (ResPath resPath) <- compileScript (scr', cfg, ref, ids) newID -- TODO remove the ID here, or is it useful?
  return (ExprPath resPath)

{- This decides the "replace ID" in rReplace' above. It's important because the
 - hash needs to be unique whenever we would want to return different results,
 - but the same between things that we actually want deduplicated. So far we
 - err on the side of uniqueness.
 -
 - TODO think carefully about whether all of these need to be in here
 -}
calcReplaceID :: OrthoLangState -> OrthoLangExpr -> OrthoLangVar -> OrthoLangExpr -> ReplaceID
calcReplaceID (scr, _, _, _) resExpr subVar subExpr = ReplaceID $ Just $ digest
  [ show scr
  , show resExpr
  , show subVar
  , show subExpr
  ]

{- Helper function to write the final list when the results are literals
 - TODO factor out, and maybe unify with rListLits
 - TODO subPaths is only one path? if so, rename it
 -}
aReplaceEachLits :: OrthoLangType -> OrthoLangConfig -> Locks
                -> OrthoLangPath -> OrthoLangPath -> [OrthoLangPath] -> Action ()
aReplaceEachLits _ cfg ref outPath subPaths resPaths = do
  lits <- mapM (readLit cfg ref) resPaths'
  let lits' = map stripWhiteSpace lits
  writeLits cfg ref out lits'
  where
    outPath'  = fromOrthoLangPath cfg outPath
    subPaths' = fromOrthoLangPath cfg subPaths
    resPaths' = map (fromOrthoLangPath cfg) resPaths
    out = traceA "aReplaceEachLits" outPath' (outPath':subPaths':resPaths')

{- Helper function to write the final list when the results are links to files
 - TODO factor out, and maybe unify with rListLinks
 -}
aReplaceEachLinks :: OrthoLangConfig -> Locks -> OrthoLangPath -> OrthoLangPath -> [OrthoLangPath] -> Action ()
aReplaceEachLinks cfg ref outPath subPaths resPaths = do
  need (subPaths':resPaths') -- TODO is needing subPaths required?
  writePaths cfg ref out resPaths
  where
    outPath'  = fromOrthoLangPath cfg outPath
    subPaths' = fromOrthoLangPath cfg subPaths
    resPaths' = map (fromOrthoLangPath cfg) resPaths
    out = traceA "aReplaceEachLinks" outPath' (outPath':subPaths':resPaths')

------------------
-- replace_each --
------------------

replaceEach :: OrthoLangFunction
replaceEach = OrthoLangFunction
  { fNames     = ["replace_each"]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = tReplaceEach
  , fTypeDesc  = dReplaceEach
  , fRules     = rReplaceEach
  }

tReplaceEach :: [OrthoLangType] -> Either String OrthoLangType
tReplaceEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
tReplaceEach _ = Left "invalid args to replace_each" -- TODO better errors here

dReplaceEach :: String
dReplaceEach = "replace_each : <outputvar> <inputvar> <inputvars> -> <output>.list"

{- This takes a list expression that includes the variable to replace and a
 - list of things to replace it with. It calls rReplace' to do each replacement
 - operation, then gathers the results in a list.
 -
 - It has one major flaw that I'm trying to fix now: it assumes that the list
 - of expressions to substitute in are known at compile-time. That makes it - impossible to repeat based on the results of a function call, limiting it to
 - just lists you explicitly write in the cut code.
 -
 - The initial rewrite plan was:
 - 1) separate out aReplaceEach for clarity
 - 2) the main goal here is to replace extractExprs with an equivalent that evaluates the list and returns its paths
 -    that needs to run in Action so it can use IO
 -    and because of that so does the rReplace' call? is that even possible?
 - 3) then given paths we can build CompiledExprs using the known types
 -
 - But then I remembered Core.Compile.Map, which might be able to help:
 - rMap :: Int -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ()) -> RulesFn
 - type Action1  = OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> OrthoLangPath -> Action ()
 - you give it a single function and an index for the argument to map over, and it does everything
 - but it only works on Action functions, so it will take some adapting to use here
 - it does seem promising though: write replace and get a proper replace_each almost for free?
 -
 - So the new plan is relatively simple: implement replace first, and then try replace_each again.
 - I'm not that sure the rMap thing will work because it deals with Actions though.
 -}
rReplaceEach :: OrthoLangState
             -> OrthoLangExpr -- the final result expression, which contains all the info we need
             -> Rules ExprPath
rReplaceEach s@(scr, cfg, ref, _) expr@(OrthoLangFun _ _ _ _ (resExpr:(OrthoLangRef _ _ _ subVar):subList:[])) = do
  subPaths <- rExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (rReplace' s resExpr subVar) subExprs
  let subPaths' = (\(ExprPath p) -> toOrthoLangPath cfg p) subPaths
      resPaths' = map (\(ExprPath p) -> toOrthoLangPath cfg p) resPaths
      outPath   = exprPath s expr
      outPath'  = debugRules cfg "rReplaceEach" expr $ fromOrthoLangPath cfg outPath
  outPath' %> \_ ->
    let actFn = if typeOf expr `elem` [ListOf str, ListOf num]
                  then aReplaceEachLits (typeOf expr)
                  else aReplaceEachLinks
    in actFn cfg ref outPath subPaths' resPaths'
  return (ExprPath outPath')
rReplaceEach _ expr = fail $ "bad argument to rReplaceEach: " ++ show expr

--------------------------
-- replace_each rewrite --
--------------------------

{- Unlike the first version, this one should work without being able to extractExprs from subList.
 - That is, subList shouldn't have to be known at rules-time but only when running the actions.
 - I plan to base the algorithm loosely on the Map module with its .args files, but only loosely.
 - Like that it should have two separate action blocks:
 -
 - (1) need read the list of replacement expressions, generate a replace input for each one,
 -     need the corresponding outputs, and return a final list
 - (2) given a single input (replacement), generate the output
 -
 - Part (2) will also need access to all the rest of the standard information,
 - but that can be encoded beforehand by making the pattern only match a specific hashed subfolder.
 -
 - TODO figure out the file naming scheme in more detail before/while implementing
 - TODO think about whether a hybrid approach might be easier, where you call rMap on an action at the end
 - TODO once this works, rewrite replace using it + singleton
 -}

replaceEach2 :: CutFunction
replaceEach2 = CutFunction
  { fName      = "replace_each2"
  , fFixity    = Prefix
  , fTypeCheck = tReplaceEach
  , fDesc      = Nothing
  , fTypeDesc  = dReplaceEach2
  , fRules     = rReplaceEach
  }

dReplaceEach2 :: String
dReplaceEach2 = "replace_each2 : <outputvar> <inputvar> <inputvars> -> <output>.list"
