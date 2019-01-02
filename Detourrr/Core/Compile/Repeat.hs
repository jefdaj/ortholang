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
import Detourrr.Core.Paths  (exprPath, fromDtrPath,
                             DtrPath, toDtrPath, fromDtrPath)
import Detourrr.Core.Compile.Basic (rExpr, compileScript, debugRules)
-- import Detourrr.Core.Debug   (debugRules, debugA)
import Detourrr.Core.Util    (digest, stripWhiteSpace)

--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

-- TODO only mangle the specific vars we want changed!

mangleExpr :: (DtrVar -> DtrVar) -> DtrExpr -> DtrExpr
mangleExpr _ e@(DtrLit  _ _ _) = e
mangleExpr fn (DtrRef  t n vs v      ) = DtrRef  t n (map fn vs)   (fn v)
mangleExpr fn (DtrBop  t n vs s e1 e2) = DtrBop  t n (map fn vs) s (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (DtrFun  t n vs s es   ) = DtrFun  t n (map fn vs) s (map (mangleExpr fn) es)
mangleExpr fn (DtrList t n vs   es   ) = DtrList t n (map fn vs)   (map (mangleExpr fn) es)
mangleExpr _ (DtrRules _) = error "implement this!"

mangleAssign :: (DtrVar -> DtrVar) -> DtrAssign -> DtrAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (DtrVar -> DtrVar) -> DtrScript -> DtrScript
mangleScript fn = map (mangleAssign fn)

-- TODO pad with zeros?
-- Add a "dupN." prefix to each variable name in the path from independent
-- -> dependent variable, using a list of those varnames
addPrefix :: String -> (DtrVar -> DtrVar)
addPrefix p (DtrVar s) = DtrVar $ s ++ "." ++ p

-- TODO should be able to just apply this to a duplicate script section right?
addPrefixes :: String -> DtrScript -> DtrScript
addPrefixes p = mangleScript (addPrefix p)

----------------------------------
-- main repeat function for PRS --
----------------------------------

repeatEach :: DtrFunction
repeatEach = DtrFunction
  { fName      = "repeat_each"
  , fFixity    = Prefix
  , fTypeCheck = tRepeatEach
  , fDesc = Nothing, fTypeDesc  = dRepeatEach
  , fRules  = rRepeatEach
  }

tRepeatEach :: [DtrType] -> Either String DtrType
tRepeatEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
tRepeatEach _ = Left "invalid args to repeat_each" -- TODO better errors here

dRepeatEach :: String
dRepeatEach = "repeat_each : <outputvar> <inputvar> <inputvars> -> <output>.list"

-- TODO ideally, this shouldn't need any custom digesting? but whatever no
--      messing with it for now
-- TODO can this be parallelized better?
cRepeat :: DtrState -> DtrExpr -> DtrVar -> DtrExpr -> Rules ExprPath
cRepeat (script, cfg, ref, ids) resExpr subVar subExpr = do
  let res  = (DtrVar "result", resExpr)
      sub  = (subVar, subExpr)
      deps = filter (\(v,_) -> (elem v $ depsOf resExpr ++ depsOf subExpr)) script
      -- TODO this should be very different right?
      pre  = digest $ map show $ res:sub:deps
      scr' = (addPrefixes pre ([sub] ++ deps ++ [res]))
  (ResPath resPath) <- compileScript (scr', cfg, ref, ids) (Just pre)
  return (ExprPath resPath) -- TODO this is supposed to convert result -> expr right?

rRepeatEach :: DtrState -> DtrExpr -> Rules ExprPath
rRepeatEach s@(scr, cfg, ref, _) expr@(DtrFun _ _ _ _ (resExpr:(DtrRef _ _ _ subVar):subList:[])) = do
  subPaths <- rExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (cRepeat s resExpr subVar) subExprs
  let subPaths' = (\(ExprPath p) -> toDtrPath cfg p) subPaths
      resPaths' = map (\(ExprPath p) -> toDtrPath cfg p) resPaths
      outPath   = exprPath s expr
      outPath'  = debugRules cfg "rRepeatEach" expr $ fromDtrPath cfg outPath
  outPath' %> \_ ->
    let actFn = if typeOf expr `elem` [ListOf str, ListOf num]
                  then aRepeatEachLits (typeOf expr)
                  else aRepeatEachLinks
    in actFn cfg ref outPath subPaths' resPaths'
  return (ExprPath outPath')
rRepeatEach _ expr = error $ "bad argument to rRepeatEach: " ++ show expr

-- TODO factor out, and maybe unify with rListLits
-- TODO subPaths is only one path? if so, rename it
aRepeatEachLits :: DtrType -> DtrConfig -> Locks
                -> DtrPath -> DtrPath -> [DtrPath] -> Action ()
aRepeatEachLits _ cfg ref outPath subPaths resPaths = do
  lits <- mapM (readLit cfg ref) resPaths'
  let lits' = map stripWhiteSpace lits
  writeLits cfg ref out lits'
  where
    outPath'  = fromDtrPath cfg outPath
    subPaths' = fromDtrPath cfg subPaths
    resPaths' = map (fromDtrPath cfg) resPaths
    out = debugA cfg "aRepeatEachLits" outPath' (outPath':subPaths':resPaths')

-- TODO factor out, and maybe unify with rListLinks
aRepeatEachLinks :: DtrConfig -> Locks -> DtrPath -> DtrPath -> [DtrPath] -> Action ()
aRepeatEachLinks cfg ref outPath subPaths resPaths = do
  need (subPaths':resPaths') -- TODO is needing subPaths required?
  writePaths cfg ref out resPaths
  where
    outPath'  = fromDtrPath cfg outPath
    subPaths' = fromDtrPath cfg subPaths
    resPaths' = map (fromDtrPath cfg) resPaths
    out = debugA cfg "aRepeatEachLinks" outPath' (outPath':subPaths':resPaths')
