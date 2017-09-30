module ShortCut.Core.Compile.Repeat where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Paths  (exprPath, fromCutPath, writeLits, readLit)
import ShortCut.Core.Compile.Basic (rExpr, compileScript)
import ShortCut.Core.Debug   (debugRules, debugAction)
import System.FilePath       (makeRelative)
import ShortCut.Core.Util    (digest, stripWhiteSpace)

--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

-- TODO only mangle the specific vars we want changed!

mangleExpr :: (CutVar -> CutVar) -> CutExpr -> CutExpr
mangleExpr _ e@(CutLit  _ _ _) = e
mangleExpr fn (CutRef  t n vs v      ) = CutRef  t n (map fn vs)   (fn v)
mangleExpr fn (CutBop  t n vs s e1 e2) = CutBop  t n (map fn vs) s (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (CutFun  t n vs s es   ) = CutFun  t n (map fn vs) s (map (mangleExpr fn) es)
mangleExpr fn (CutList t n vs   es   ) = CutList t n (map fn vs)   (map (mangleExpr fn) es)

mangleAssign :: (CutVar -> CutVar) -> CutAssign -> CutAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (CutVar -> CutVar) -> CutScript -> CutScript
mangleScript fn = map (mangleAssign fn)

-- TODO pad with zeros?
-- Add a "dupN." prefix to each variable name in the path from independent
-- -> dependent variable, using a list of those varnames
addPrefix :: String -> (CutVar -> CutVar)
addPrefix p (CutVar s) = CutVar $ s ++ "." ++ p

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
  , fRules  = rRepeatEach
  }

tRepeatEach :: [CutType] -> Either String CutType
tRepeatEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
tRepeatEach _ = Left "invalid args to repeat_each" -- TODO better errors here

-- TODO ideally, this shouldn't need any custom digesting? but whatever no
--      messing with it for now
-- TODO can this be parallelized better?
cRepeat :: CutState -> CutExpr -> CutVar -> CutExpr -> Rules ExprPath
cRepeat (script,cfg) resExpr subVar subExpr = do
  let res  = (CutVar "result", resExpr)
      sub  = (subVar, subExpr)
      deps = filter (\(v,_) -> (elem v $ depsOf resExpr)) script
      pre  = digest $ map show $ res:sub:deps
      scr' = (addPrefixes pre ([sub] ++ deps ++ [res]))
  (ResPath resPath) <- compileScript (scr',cfg) (Just pre)
  -- let res  = (ExprPath resPath) -- TODO this is supposed to convert result -> expr right?
  -- let resPath' = debugRules cfg "cRepeat" (resExpr, subVar, subExpr) resPath
  return (ExprPath resPath) -- TODO this is supposed to convert result -> expr right?

rRepeatEach :: CutState -> CutExpr -> Rules ExprPath
rRepeatEach s@(scr,cfg) expr@(CutFun _ _ _ _ (resExpr:(CutRef _ _ _ subVar):subList:[])) = do
  subPaths <- rExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (cRepeat s resExpr subVar) subExprs
  let (ExprPath subPaths') = subPaths
      resPaths'  = map (\(ExprPath p) -> p) resPaths
      outPath    = fromCutPath cfg $ exprPath s expr
      outPath'   = debugRules cfg "rRepeatEach" expr outPath
  outPath %> \_ ->
    let actFn = if typeOf expr `elem` [ListOf str, ListOf num]
                  then aRepeatEachLits (typeOf expr)
                  else aRepeatEachLinks
    in actFn cfg outPath subPaths' resPaths'
  return (ExprPath outPath')
rRepeatEach _ expr = error $ "bad argument to rRepeatEach: " ++ show expr

-- TODO factor out, and maybe unify with rListLits
aRepeatEachLits :: CutType -> CutConfig
                -> FilePath -> FilePath -> [FilePath] -> Action ()
aRepeatEachLits _ cfg outPath subPaths resPaths = do
  lits <- mapM (readLit cfg) resPaths
  let lits' = map stripWhiteSpace lits
      out = debugAction cfg "aRepeatEachLits" outPath (outPath:subPaths:resPaths)
  -- liftIO $ putStrLn $ "aRepeatEachLits lits': " ++ show lits'
  writeLits cfg out lits'

-- TODO factor out, and maybe unify with rListLinks
aRepeatEachLinks :: CutConfig -> FilePath -> FilePath -> [FilePath] -> Action ()
aRepeatEachLinks cfg outPath subPaths' resPaths' = do
  need (subPaths':resPaths') -- TODO is needing subPaths required?
  let outPaths' = map (makeRelative $ cfgTmpDir cfg) resPaths'
  let out = debugAction cfg "aRepeatEachLinks" outPath (outPath:subPaths':resPaths')
  writeLits cfg out outPaths'
