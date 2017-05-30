-- TODO rename to Repeat (the function too I guess)

module ShortCut.Modules.Substitute where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile (cExpr, hashedTmp, addPrefixes, compileScript)
import System.FilePath (makeRelative)
import Data.List.Utils (delFromAL)
import Data.Maybe (fromJust)
import Data.List (nub)

cutModule :: CutModule
cutModule = CutModule
  { mName = "substitute"
  , mFunctions =
    [ subEach
    ]
  }

subEach :: CutFunction
subEach = CutFunction
  { fName      = "substitute_each"
  , fFixity    = Prefix
  , fTypeCheck = sTypeCheck
  , fCompiler  = cSubs
  }

sTypeCheck :: [CutType] -> Either String CutType
sTypeCheck (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
sTypeCheck _ = Left "invalid args to substitute_each" -- TODO better errors here

extractExprs :: CutScript -> CutExpr -> [CutExpr]
extractExprs  _  (CutList _ _ es) = es
extractExprs scr (CutRef  _ _ v ) = extractExprs scr $ fromJust $ lookup v scr
extractExprs  _   e               = error $ "bad arg to extractExpr: " ++ show e

cSub :: CutState -> CutExpr -> CutVar -> Int -> CutExpr -> Rules FilePath
cSub (script,cfg) resExpr subVar n subExpr = do
  let res   = (CutVar "result", resExpr)
      sub   = (subVar, subExpr)
      deps  = filter (\(v,_) -> (elem v $ depsOf resExpr)
                            && elem v (rDepsOf script subVar)) script
      scr'' = addPrefixes n ([sub] ++ deps ++ [res])
  resPath <- compileScript (scr'',cfg) (Just n)
  return resPath

cSubs :: CutState -> CutExpr -> Rules FilePath
cSubs s@(scr,cfg) expr@(CutFun _ _ _ (resExpr:(CutRef _ _ subVar):subList:[])) = do
  subPaths <- cExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (\(n,e) -> cSub s resExpr subVar n e) (zip [1..] subExprs)
  let outPath  = hashedTmp cfg expr resPaths
  outPath %> \out -> do
    need (subPaths:resPaths)
    let outPaths' = map (makeRelative $ cfgTmpDir cfg) resPaths
    writeFileLines out outPaths'
  return outPath
cSubs _ expr = error $ "bad argument to cSubs: " ++ show expr
