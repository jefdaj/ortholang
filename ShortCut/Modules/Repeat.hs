module ShortCut.Modules.Repeat where

import Development.Shake
import ShortCut.Core.Types

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

-- TODO will this get printed, or will there just be a parse error?
explainFnBug :: String
explainFnBug = "You've stumbled on an outstanding bug. Sorry about that! \
               \The problem is that when doing transformations involving lists \
               \like repeat or map, ShortCut can't \"see\" through future function calls; \
               \it can only manipulate lists whose elements are known *before* running the \
               \program. If you want Jeff to consider rewriting some things to fix that, \
               \drop him a line!"

-- TODO what if it's a function call? do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: CutScript -> CutExpr -> [CutExpr]
extractExprs  _  (CutList _ _ _ es) = es
extractExprs scr (CutRef  _ _ _ v ) = extractExprs scr $ fromJust $ lookup v scr
extractExprs _   (CutFun _ _ _ _ _) = error explainFnBug
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
cRepeatEach s@(scr,cfg) expr@(CutFun _ _ _ _ (resExpr:(CutRef _ _ _ subVar):subList:[])) = do
  subPaths <- cExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (cRepeat s resExpr subVar) subExprs
  let outPath = hashedTmp cfg expr resPaths
  outPath %> \out -> do
    need (subPaths:resPaths) -- TODO is needing subPaths required?
    let outPaths' = map (makeRelative $ cfgTmpDir cfg) resPaths
    writeFileLines out outPaths'
  return outPath
cRepeatEach _ expr = error $ "bad argument to cRepeatEach: " ++ show expr
