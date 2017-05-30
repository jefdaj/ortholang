module ShortCut.Modules.Repeat where

import Development.Shake
import ShortCut.Core.Types

import Data.List             (nub)
import Data.List.Utils       (delFromAL)
import Data.Maybe            (fromJust)
import ShortCut.Core.Compile (cExpr, hashedTmp, addPrefixes, compileScript, digest)
import System.FilePath       (makeRelative)

cutModule :: CutModule
cutModule = CutModule
  { mName = "repeat"
  , mFunctions =
    [ repeatEach
    ]
  }

repeatEach :: CutFunction
repeatEach = CutFunction
  { fName      = "repeat_each"
  , fFixity    = Prefix
  , fTypeCheck = tRepeatEach
  , fCompiler  = cRepeatEach
  }

tRepeatEach :: [CutType] -> Either String CutType
tRepeatEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
tRepeatEach _ = Left "invalid args to repeat_each" -- TODO better errors here

extractExprs :: CutScript -> CutExpr -> [CutExpr]
extractExprs  _  (CutList _ _ es) = es
extractExprs scr (CutRef  _ _ v ) = extractExprs scr $ fromJust $ lookup v scr
extractExprs  _   e               = error $ "bad arg to extractExpr: " ++ show e

cRepeat :: CutState -> CutExpr -> CutVar -> CutExpr -> Rules FilePath
cRepeat (script,cfg) resExpr subVar subExpr = do
  let res  = (CutVar "result", resExpr)
      sub  = (subVar, subExpr)
      deps = filter (\(v,_) -> (elem v $ depsOf resExpr)) script
      pre  = digest $ map show $ res:sub:deps
      scr' = (addPrefixes pre ([sub] ++ deps ++ [res]))
  resPath <- compileScript (scr',cfg) (Just pre)
  return resPath

cRepeatEach :: CutState -> CutExpr -> Rules FilePath
cRepeatEach s@(scr,cfg) expr@(CutFun _ _ _ (resExpr:(CutRef _ _ subVar):subList:[])) = do
  subPaths <- cExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (cRepeat s resExpr subVar) subExprs
  let outPath = hashedTmp cfg expr resPaths
  outPath %> \out -> do
    need (subPaths:resPaths)
    let outPaths' = map (makeRelative $ cfgTmpDir cfg) resPaths
    writeFileLines out outPaths'
  return outPath
cRepeatEach _ expr = error $ "bad argument to cRepeatEach: " ++ show expr
