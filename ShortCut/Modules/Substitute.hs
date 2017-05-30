-- TODO rename to Repeat (the function too I guess)

module ShortCut.Modules.Substitute where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile (cExpr, hashedTmp, hashedTmp', addPrefixes, compileScript)
import System.FilePath (makeRelative)
import Data.List.Utils (delFromAL)
import Data.Maybe (fromJust)

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
  , fTypeCheck = subEachTypeCheck
  , fCompiler  = cSubs
  }

subEachTypeCheck = undefined

-- TODO only the var needs to be handled differently from in other fns?
-- TODO is the result thing going to mess everything up?
cSub :: CutState -> CutExpr -> CutVar -> Int -> CutExpr -> Rules FilePath
cSub (script,cfg) resExpr subVar n subExpr = do
  let res    = (CutVar "result", resExpr)
      sub    = (subVar, subExpr)
      scr'   = delFromAL script subVar -- TODO need to remove result too?
      scr''  = res:sub:scr'
      scr''' = addPrefixes n scr''
  resPath <- compileScript (scr''',cfg) (Just n)
  return resPath

extractExprs :: CutScript -> CutExpr -> [CutExpr]
extractExprs  _  (CutList _ _ es) = es
extractExprs scr (CutRef  _ _ v ) = extractExprs scr $ fromJust $ lookup v scr
extractExprs  _   e               = error $ "bad arg to extractExpr: " ++ show e

-- TODO this has to work with *Refs* to the things too! (no assuming CutList)
--      does that mean it has to be written to a file?
--      ... not possible :( requires the recursive script it holds too
--      maybe it's time to give up and pass the whole state?
--      then this could be a regular function in a Substitute module
--      yeah, better go with that for now!
cSubs :: CutState -> CutExpr -> Rules FilePath
cSubs s@(scr,cfg) expr@(CutFun t _ _ (resExpr:(CutRef _ _ subVar):subList:[])) = do
  subPaths <- cExpr s subList

  -- TODO the main thing left is, how to compile the individual sub results?
  --      one idea: make a single wildcard pattern for it here and map over the names
  --      another: look up subVar in the script and pass *that* to cSub?
  let subExprs = extractExprs scr subList
  resPaths <- mapM (\(n,e) -> cSub s resExpr subVar n e) (zip [1..] subExprs)
  let outPath  = hashedTmp cfg expr resPaths
  -- resPaths <- mapM (\(n,e) -> cSub s resExpr subVar scr n e) (zip [1..] subList)
  -- let resPaths' = map (makeRelative $ cfgTmpDir cfg) resPaths
  --     outPath   = hashedTmp' cfg (ListOf $ typeOf resExpr) resExpr resPaths'
  -- outPath %> \out -> need resPaths >> writeFileLines out resPaths'
  outPath %> \out -> do
    outPaths <- readFileLines subPaths
    need outPaths
    let outPaths' = map (makeRelative $ cfgTmpDir cfg) outPaths
    writeFileLines out outPaths'
  return outPath
cSubs _ expr = error $ "bad argument to cSubs: " ++ show expr
