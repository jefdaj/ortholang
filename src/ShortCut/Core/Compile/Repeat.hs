module ShortCut.Core.Compile.Repeat where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile.Paths   (exprPath)
import ShortCut.Core.Compile.Rules (rExpr, addPrefixes, compileScript)
import ShortCut.Core.Debug   (debugRules, debugReadFile, debugWriteLines,
                              debugAction)
import System.FilePath       (makeRelative)
import ShortCut.Core.Util    (digest, stripWhiteSpace)

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
      resPaths'' = map (makeRelative $ cfgTmpDir cfg) resPaths'
      outPath'   = debugRules cfg "rRepeatEach" expr outPath
      (ExprPath outPath) = exprPath cfg True expr $ map ExprPath resPaths''
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
aRepeatEachLits rtn cfg outPath subPaths resPaths = do
  lits <- mapM (debugReadFile cfg) resPaths
  let lits' = map stripWhiteSpace lits
      out = debugAction cfg "aRepeatEachLits" outPath (outPath:subPaths:resPaths)
  -- liftIO $ putStrLn $ "aRepeatEachLits lits': " ++ show lits'
  debugWriteLines cfg out lits'

-- TODO factor out, and maybe unify with rListLinks
aRepeatEachLinks :: CutConfig -> FilePath -> FilePath -> [FilePath] -> Action ()
aRepeatEachLinks cfg outPath subPaths' resPaths' = do
  need (subPaths':resPaths') -- TODO is needing subPaths required?
  let outPaths' = map (makeRelative $ cfgTmpDir cfg) resPaths'
  let out = debugAction cfg "aRepeatEachLinks" outPath (outPath:subPaths':resPaths')
  debugWriteLines cfg out outPaths'
