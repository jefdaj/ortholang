module ShortCut.Core.Compile.Replace where

{- This is some pretty awkward code, but I also think it's one of the coolest
 - features of ShortCut! It conveniently answers the common question "What
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
import ShortCut.Core.Types

import ShortCut.Core.Actions       (writeLits, writePaths, readLit, debugA)
import ShortCut.Core.Paths         (exprPath, fromCutPath,
                                    CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Compile.Basic (rExpr, compileScript, debugRules)
import ShortCut.Core.Util          (digest, stripWhiteSpace)

cutModule :: CutModule
cutModule = CutModule
  { mName = "Replace"
  , mDesc = "Replace variables in the script to see how the results change"
  , mTypes = []
  , mFunctions =
      [ replace
      , replaceEach
      ]
  }

---------------------------
-- mangle variable names --
---------------------------

{- This does the filename mangling by setting a "replace ID" in each variable
 - in a script. If it's anything other than Nothing it gets used by
 - ShortCut.Core.Paths.varPath to set the rep dir.
 -
 - TODO should it go in Types, or maybe Paths?
 -}

mangleExpr :: (CutVar -> CutVar) -> CutExpr -> CutExpr
mangleExpr _ e@(CutLit  _ _ _) = e
mangleExpr fn (CutRef  t n vs v      ) = CutRef  t n (map fn vs)   (fn v)
mangleExpr fn (CutBop  t n vs s e1 e2) = CutBop  t n (map fn vs) s (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (CutFun  t n vs s es   ) = CutFun  t n (map fn vs) s (map (mangleExpr fn) es)
mangleExpr fn (CutList t n vs   es   ) = CutList t n (map fn vs)   (map (mangleExpr fn) es)
mangleExpr _ (CutRules _) = error "implement this!"

mangleAssign :: (CutVar -> CutVar) -> CutAssign -> CutAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (CutVar -> CutVar) -> CutScript -> CutScript
mangleScript fn = map (mangleAssign fn)

setReplaceID :: ReplaceID -> CutVar -> CutVar
setReplaceID newID (CutVar _ name) = CutVar newID name

setReplaceIDs :: ReplaceID -> CutScript -> CutScript
setReplaceIDs newID = mangleScript (setReplaceID newID)

-------------
-- replace --
-------------

replace :: CutFunction
replace = CutFunction
  { fName      = "replace"
  , fFixity    = Prefix
  , fTypeCheck = tReplace
  , fDesc      = Just "See what one (dependent) variable would be if you changed another (independent) variable."
  , fTypeDesc  = dReplace
  , fRules     = rReplace
  }

tReplace :: [CutType] -> Either String CutType
tReplace (res:sub:sub':[]) | sub == sub' = Right res
tReplace _ = Left "invalid args to replace" -- TODO better errors here

-- TODO write this is a way that will make sense to other people
dReplace :: String
dReplace = "replace : <outputvar> <vartoreplace> <exprtoreplacewith> -> <newoutput>"

rReplace :: CutState -> CutExpr -> Rules ExprPath
rReplace st (CutFun _ _ _ _ (resExpr:(CutRef _ _ _ subVar):subExpr:[])) = rReplace' st resExpr subVar subExpr
rReplace _ _ = error "bad argument to rReplace"

{- This does the actual replace operation. It takes an expression to edit, the
 - variable to replace, and an expression to put in its place. While it does
 - that it also filters to remove any irrelevant variables and sets a unique
 - "replace ID" on everything to keep the new copies of variables from
 - conflicting with the existing ones. The copies go in a separate "reps"
 - folder named by the ID.
 -
 - TODO any reason not to merge it into rReplace above?
 -}
rReplace' :: CutState
          -> CutExpr -- the result expression for a single replacement, which *doesn't* contain all the info
          -> CutVar  -- we also need the variable to be replaced
          -> CutExpr -- and an expression to replace it with (which could be a ref to another variable)
          -> Rules ExprPath
rReplace' st@(script, cfg, ref, ids) resExpr subVar@(CutVar _ _) subExpr = do
  let res   = (CutVar (ReplaceID Nothing) "result", resExpr)
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
calcReplaceID :: CutState -> CutExpr -> CutVar -> CutExpr -> ReplaceID
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
aReplaceEachLits :: CutType -> CutConfig -> Locks
                -> CutPath -> CutPath -> [CutPath] -> Action ()
aReplaceEachLits _ cfg ref outPath subPaths resPaths = do
  lits <- mapM (readLit cfg ref) resPaths'
  let lits' = map stripWhiteSpace lits
  writeLits cfg ref out lits'
  where
    outPath'  = fromCutPath cfg outPath
    subPaths' = fromCutPath cfg subPaths
    resPaths' = map (fromCutPath cfg) resPaths
    out = debugA cfg "aReplaceEachLits" outPath' (outPath':subPaths':resPaths')

{- Helper function to write the final list when the results are links to files
 - TODO factor out, and maybe unify with rListLinks
 -}
aReplaceEachLinks :: CutConfig -> Locks -> CutPath -> CutPath -> [CutPath] -> Action ()
aReplaceEachLinks cfg ref outPath subPaths resPaths = do
  need (subPaths':resPaths') -- TODO is needing subPaths required?
  writePaths cfg ref out resPaths
  where
    outPath'  = fromCutPath cfg outPath
    subPaths' = fromCutPath cfg subPaths
    resPaths' = map (fromCutPath cfg) resPaths
    out = debugA cfg "aReplaceEachLinks" outPath' (outPath':subPaths':resPaths')

------------------
-- replace_each --
------------------

replaceEach :: CutFunction
replaceEach = CutFunction
  { fName      = "replace_each"
  , fFixity    = Prefix
  , fTypeCheck = tReplaceEach
  , fDesc      = Nothing
  , fTypeDesc  = dReplaceEach
  , fRules     = rReplaceEach
  }

tReplaceEach :: [CutType] -> Either String CutType
tReplaceEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
tReplaceEach _ = Left "invalid args to replace_each" -- TODO better errors here

dReplaceEach :: String
dReplaceEach = "replace_each : <outputvar> <inputvar> <inputvars> -> <output>.list"

{- This takes a list expression that includes the variable to replace and a
 - list of things to replace it with. It calls rReplace' to do each replacement
 - operation, then gathers the results in a list.
 -
 - It has one major flaw that I'm trying to fix now: it assumes that the list
 - of expressions to substitute in are known at compile-time. That makes it
 - impossible to repeat based on the results of a function call, limiting it to
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
 - rMap :: Int -> (CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()) -> RulesFn
 - type Action1  = CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> CutPath -> Action ()
 - you give it a single function and an index for the argument to map over, and it does everything
 - but it only works on Action functions, so it will take some adapting to use here
 - it does seem promising though: write replace and get a proper replace_each almost for free?
 -
 - So the new plan is relatively simple: implement replace first, and then try replace_each again.
 - I'm not that sure the rMap thing will work because it deals with Actions though.
 -}
rReplaceEach :: CutState
             -> CutExpr -- the final result expression, which contains all the info we need
             -> Rules ExprPath
rReplaceEach s@(scr, cfg, ref, _) expr@(CutFun _ _ _ _ (resExpr:(CutRef _ _ _ subVar):subList:[])) = do
  subPaths <- rExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (rReplace' s resExpr subVar) subExprs
  let subPaths' = (\(ExprPath p) -> toCutPath cfg p) subPaths
      resPaths' = map (\(ExprPath p) -> toCutPath cfg p) resPaths
      outPath   = exprPath s expr
      outPath'  = debugRules cfg "rReplaceEach" expr $ fromCutPath cfg outPath
  outPath' %> \_ ->
    let actFn = if typeOf expr `elem` [ListOf str, ListOf num]
                  then aReplaceEachLits (typeOf expr)
                  else aReplaceEachLinks
    in actFn cfg ref outPath subPaths' resPaths'
  return (ExprPath outPath')
rReplaceEach _ expr = error $ "bad argument to rReplaceEach: " ++ show expr
