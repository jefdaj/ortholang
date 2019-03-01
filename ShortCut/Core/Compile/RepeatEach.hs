module ShortCut.Core.Compile.RepeatEach where

{- Why can't we use an obvious var mangling scheme again?
 -   vars/<varname>.<seed>.<ext>
 -   vars/<seed>/<varname>.<ext>
 -   vars/<varname>/<varname>.<seed>.<ext>
 - Maybe just have to delete all var links when starting?
 - Start by moving to prefix dirs, then rename to ints if possible
 -}

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions (writeLits, writePaths, readLit, debugA)
import ShortCut.Core.Paths  (exprPath, fromCutPath,
                             CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Compile.Basic (rExpr, compileScript, debugRules)
-- import ShortCut.Core.Debug   (debugRules, debugA)
import ShortCut.Core.Util    (digest, stripWhiteSpace)
import ShortCut.Core.Random   (initialRandomSeed)
import System.Random (StdGen, next)

--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

-- TODO only mangle the specific vars we want changed!
-- TODO can all this silliness be avoided using random seeds?

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

-- TODO pad with zeros?
-- Add a "dupN." prefix to each variable name in the path from independent
-- -> dependent variable, using a list of those varnames
addPrefix :: String -> (CutVar -> CutVar)
addPrefix p (CutVar r s) = CutVar r $ s ++ "." ++ p
-- addPrefix seed (CutVar _ s) = CutVar (RandomSeed seed) s

-- TODO should be able to just apply this to a duplicate script section right?
addPrefixes :: String -> CutScript -> CutScript
addPrefixes p = mangleScript (addPrefix p)

----------------------------------
-- main repeat function for PRS --
----------------------------------

repeatEach :: CutFunction
repeatEach = CutFunction
  { fName      = "repeat_each"
  , fFixity    = Prefix
  , fTypeCheck = tRepeatEach
  , fDesc = Nothing, fTypeDesc  = dRepeatEach
  , fRules  = rRepeatEach
  }

tRepeatEach :: [CutType] -> Either String CutType
tRepeatEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
tRepeatEach _ = Left "invalid args to repeat_each" -- TODO better errors here

dRepeatEach :: String
dRepeatEach = "repeat_each : <outputvar> <inputvar> <inputvars> -> <output>.list"

-- TODO ideally, this shouldn't need any custom digesting? but whatever no
--      messing with it for now
-- TODO can this be parallelized better?
cRepeat :: CutState -> CutExpr -> CutVar -> CutExpr -> Rules ExprPath
cRepeat (script, cfg, ref, ids) resExpr subVar subExpr = do
  let res  = (CutVar initialRandomSeed "result", resExpr) -- TODO is this the right seed?
      sub  = (subVar, subExpr)
      deps = filter (\(v,_) -> (elem v $ depsOf resExpr ++ depsOf subExpr)) script
      -- TODO this should be very different right?
      -- pre  = digest $ map show $ res:sub:deps
      (RandomSeed s) = seedOf resExpr
      pre  = digest $ RandomSeed $ show $ fst $ next $ (read s :: StdGen)
      scr' = (addPrefixes pre ([sub] ++ deps ++ [res]))
  (ResPath resPath) <- compileScript (scr', cfg, ref, ids) (Just pre)
  return (ExprPath resPath) -- TODO this is supposed to convert result -> expr right?

rRepeatEach :: CutState -> CutExpr -> Rules ExprPath
rRepeatEach s@(scr, cfg, ref, _) expr@(CutFun _ _ _ _ (resExpr:(CutRef _ _ _ subVar):subList:[])) = do
  subPaths <- rExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (cRepeat s resExpr subVar) subExprs
  let subPaths' = (\(ExprPath p) -> toCutPath cfg p) subPaths
      resPaths' = map (\(ExprPath p) -> toCutPath cfg p) resPaths
      outPath   = exprPath s expr
      outPath'  = debugRules cfg "rRepeatEach" expr $ fromCutPath cfg outPath
  outPath' %> \_ ->
    let actFn = if typeOf expr `elem` [ListOf str, ListOf num]
                  then aRepeatEachLits (typeOf expr)
                  else aRepeatEachLinks
    in actFn cfg ref outPath subPaths' resPaths'
  return (ExprPath outPath')
rRepeatEach _ expr = error $ "bad argument to rRepeatEach: " ++ show expr

-- TODO factor out, and maybe unify with rListLits
-- TODO subPaths is only one path? if so, rename it
aRepeatEachLits :: CutType -> CutConfig -> Locks
                -> CutPath -> CutPath -> [CutPath] -> Action ()
aRepeatEachLits _ cfg ref outPath subPaths resPaths = do
  lits <- mapM (readLit cfg ref) resPaths'
  let lits' = map stripWhiteSpace lits
  writeLits cfg ref out lits'
  where
    outPath'  = fromCutPath cfg outPath
    subPaths' = fromCutPath cfg subPaths
    resPaths' = map (fromCutPath cfg) resPaths
    out = debugA cfg "aRepeatEachLits" outPath' (outPath':subPaths':resPaths')

-- TODO factor out, and maybe unify with rListLinks
aRepeatEachLinks :: CutConfig -> Locks -> CutPath -> CutPath -> [CutPath] -> Action ()
aRepeatEachLinks cfg ref outPath subPaths resPaths = do
  need (subPaths':resPaths') -- TODO is needing subPaths required?
  writePaths cfg ref out resPaths
  where
    outPath'  = fromCutPath cfg outPath
    subPaths' = fromCutPath cfg subPaths
    resPaths' = map (fromCutPath cfg) resPaths
    out = debugA cfg "aRepeatEachLinks" outPath' (outPath':subPaths':resPaths')
