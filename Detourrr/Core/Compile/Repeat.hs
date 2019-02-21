module Detourrr.Core.Compile.Repeat where

{- Why can't we use an obvious var mangling scheme again?
 -   vars/<varname>.<salt>.<ext>
 -   vars/<salt>/<varname>.<ext>
 -   vars/<varname>/<varname>.<salt>.<ext>
 - Maybe just have to delete all var links when starting?
 - Start by moving to prefix dirs, then rename to ints if possible
 -}

import Development.Shake
import Detourrr.Core.Types

import Detourrr.Core.Actions (writeLits, writePaths, readLit, debugA)
import Detourrr.Core.Paths  (exprPath, fromRrrPath,
                             RrrPath, toRrrPath, fromRrrPath)
import Detourrr.Core.Compile.Basic (rExpr, compileScript, debugRules)
-- import Detourrr.Core.Debug   (debugRules, debugA)
import Detourrr.Core.Util    (digest, stripWhiteSpace)

--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

-- TODO only mangle the specific vars we want changed!

mangleExpr :: (RrrVar -> RrrVar) -> RrrExpr -> RrrExpr
mangleExpr _ e@(RrrLit  _ _ _) = e
mangleExpr fn (RrrRef  t n vs v      ) = RrrRef  t n (map fn vs)   (fn v)
mangleExpr fn (RrrBop  t n vs s e1 e2) = RrrBop  t n (map fn vs) s (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (RrrFun  t n vs s es   ) = RrrFun  t n (map fn vs) s (map (mangleExpr fn) es)
mangleExpr fn (RrrList t n vs   es   ) = RrrList t n (map fn vs)   (map (mangleExpr fn) es)
mangleExpr _ (RrrRules _) = error "implement this!"

mangleAssign :: (RrrVar -> RrrVar) -> RrrAssign -> RrrAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (RrrVar -> RrrVar) -> RrrScript -> RrrScript
mangleScript fn = map (mangleAssign fn)

-- TODO pad with zeros?
-- Add a "dupN." prefix to each variable name in the path from independent
-- -> dependent variable, using a list of those varnames
addPrefix :: String -> (RrrVar -> RrrVar)
addPrefix p (RrrVar s) = RrrVar $ s ++ "." ++ p

-- TODO should be able to just apply this to a duplicate script section right?
addPrefixes :: String -> RrrScript -> RrrScript
addPrefixes p = mangleScript (addPrefix p)

----------------------------------
-- main repeat function for PRS --
----------------------------------

repeatEach :: RrrFunction
repeatEach = RrrFunction
  { fName      = "repeat_each"
  , fFixity    = Prefix
  , fTypeCheck = tRepeatEach
  , fDesc = Nothing, fTypeDesc  = dRepeatEach
  , fRules  = rRepeatEach
  }

tRepeatEach :: [RrrType] -> Either String RrrType
tRepeatEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
tRepeatEach _ = Left "invalid args to repeat_each" -- TODO better errors here

dRepeatEach :: String
dRepeatEach = "repeat_each : <outputvar> <inputvar> <inputvars> -> <output>.list"

-- TODO ideally, this shouldn't need any custom digesting? but whatever no
--      messing with it for now
-- TODO can this be parallelized better?
cRepeat :: RrrState -> RrrExpr -> RrrVar -> RrrExpr -> Rules ExprPath
cRepeat (script, cfg, ref, ids) resExpr subVar subExpr = do
  let res  = (RrrVar "result", resExpr)
      sub  = (subVar, subExpr)
      deps = filter (\(v,_) -> (elem v $ depsOf resExpr ++ depsOf subExpr)) script
      -- TODO this should be very different right?
      pre  = digest $ map show $ res:sub:deps
      scr' = (addPrefixes pre ([sub] ++ deps ++ [res]))
  (ResPath resPath) <- compileScript (scr', cfg, ref, ids) (Just pre)
  return (ExprPath resPath) -- TODO this is supposed to convert result -> expr right?

rRepeatEach :: RrrState -> RrrExpr -> Rules ExprPath
rRepeatEach s@(scr, cfg, ref, _) expr@(RrrFun _ _ _ _ (resExpr:(RrrRef _ _ _ subVar):subList:[])) = do
  subPaths <- rExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (cRepeat s resExpr subVar) subExprs
  let subPaths' = (\(ExprPath p) -> toRrrPath cfg p) subPaths
      resPaths' = map (\(ExprPath p) -> toRrrPath cfg p) resPaths
      outPath   = exprPath s expr
      outPath'  = debugRules cfg "rRepeatEach" expr $ fromRrrPath cfg outPath
  outPath' %> \_ ->
    let actFn = if typeOf expr `elem` [ListOf str, ListOf num]
                  then aRepeatEachLits (typeOf expr)
                  else aRepeatEachLinks
    in actFn cfg ref outPath subPaths' resPaths'
  return (ExprPath outPath')
rRepeatEach _ expr = error $ "bad argument to rRepeatEach: " ++ show expr

-- TODO factor out, and maybe unify with rListLits
-- TODO subPaths is only one path? if so, rename it
aRepeatEachLits :: RrrType -> RrrConfig -> Locks
                -> RrrPath -> RrrPath -> [RrrPath] -> Action ()
aRepeatEachLits _ cfg ref outPath subPaths resPaths = do
  lits <- mapM (readLit cfg ref) resPaths'
  let lits' = map stripWhiteSpace lits
  writeLits cfg ref out lits'
  where
    outPath'  = fromRrrPath cfg outPath
    subPaths' = fromRrrPath cfg subPaths
    resPaths' = map (fromRrrPath cfg) resPaths
    out = debugA cfg "aRepeatEachLits" outPath' (outPath':subPaths':resPaths')

-- TODO factor out, and maybe unify with rListLinks
aRepeatEachLinks :: RrrConfig -> Locks -> RrrPath -> RrrPath -> [RrrPath] -> Action ()
aRepeatEachLinks cfg ref outPath subPaths resPaths = do
  need (subPaths':resPaths') -- TODO is needing subPaths required?
  writePaths cfg ref out resPaths
  where
    outPath'  = fromRrrPath cfg outPath
    subPaths' = fromRrrPath cfg subPaths
    resPaths' = map (fromRrrPath cfg) resPaths
    out = debugA cfg "aRepeatEachLinks" outPath' (outPath':subPaths':resPaths')