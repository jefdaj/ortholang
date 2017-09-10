module ShortCut.Modules.Repeat where

import Development.Shake
import ShortCut.Core.Types

import Data.Maybe            (fromJust)
import ShortCut.Core.Paths   (exprPath)
import ShortCut.Core.Compile (cExpr, addPrefixes, compileScript)
import ShortCut.Core.Debug   (debugCompiler, debugReadFile, debugWriteLines)
import System.FilePath       (makeRelative)
import ShortCut.Core.Util    (digest, stripWhiteSpace)
import Data.List             (sort)

cutModule :: CutModule
cutModule = CutModule
  { mName = "repeat"
  , mFunctions =
    [ repeatEach
    , repeatN
    ]
  }

----------------------------------
-- main repeat function for PRS --
----------------------------------

repeatEach :: CutFunction
repeatEach = CutFunction
  { fName      = "repeat_each"
  , fFixity    = Prefix
  , fTypeCheck = tRepeatEach
  , fCompiler  = cRepeatEach
  }

tRepeatEach :: [CutType] -> Either String CutType
tRepeatEach (res:sub:(SetOf sub'):[]) | sub == sub' = Right $ SetOf res
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
  -- let resPath' = debugCompiler cfg "cRepeat" (resExpr, subVar, subExpr) resPath
  return (ExprPath resPath) -- TODO this is supposed to convert result -> expr right?

-- sortNumLits :: [String] -> [String]
-- sortNumLits = sort -- TODO write this

cRepeatEach :: CutState -> CutExpr -> Rules ExprPath
cRepeatEach s@(scr,cfg) expr@(CutFun _ _ _ _ (resExpr:(CutRef _ _ _ subVar):subList:[])) = do
  subPaths <- cExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (cRepeat s resExpr subVar) subExprs
  let (ExprPath subPaths') = subPaths
      resPaths'  = map (\(ExprPath p) -> p) resPaths
      resPaths'' = map (makeRelative $ cfgTmpDir cfg) resPaths'
      outPath'   = debugCompiler cfg "cRepeatEach" expr outPath
      (ExprPath outPath) = exprPath cfg expr $ map ExprPath resPaths''
  outPath %> \_ ->
    let actFn = if typeOf expr `elem` [SetOf str, SetOf num]
                  then aRepeatEachLits (typeOf expr)
                  else aRepeatEachLinks
    in actFn cfg outPath subPaths' resPaths'
  return (ExprPath outPath')
cRepeatEach _ expr = error $ "bad argument to cRepeatEach: " ++ show expr

-- TODO factor out, and maybe unify with cSetLits
aRepeatEachLits :: CutType
                -> CutConfig -> FilePath -> FilePath -> [FilePath] -> Action ()
aRepeatEachLits rtn cfg outPath subPaths' resPaths' = do
  lits  <- mapM (debugReadFile cfg) (subPaths':resPaths')
  let sortFn = if rtn == (SetOf num) then sort else sort -- TODO write sortNumLits
      lits'  = sortFn $ map stripWhiteSpace lits
  debugWriteLines cfg outPath lits'

-- TODO factor out, and maybe unify with cSetLinks
aRepeatEachLinks :: CutConfig -> FilePath -> FilePath -> [FilePath] -> Action ()
aRepeatEachLinks cfg outPath subPaths' resPaths' = do
  need (subPaths':resPaths') -- TODO is needing subPaths required?
  let outPaths' = map (makeRelative $ cfgTmpDir cfg) resPaths'
  debugWriteLines cfg outPath outPaths'

-----------------------------------------------------
-- repeat without permutation (to test robustness) --
-----------------------------------------------------

repeatN :: CutFunction
repeatN = CutFunction
  { fName      = "repeat"
  , fFixity    = Prefix
  , fTypeCheck = tRepeatN
  , fCompiler  = cRepeatN
  }

-- takes a result type, a starting type, and an int,
-- and returns a list of the result var type. start type can be whatever
-- TODO does num here refer to actual num, or is it shadowing it?
tRepeatN :: [CutType] -> Either String CutType 
tRepeatN [rType, _, n] | n == num = Right $ SetOf rType
tRepeatN _ = Left "invalid args to repeatN"

extractNum :: CutScript -> CutExpr -> Int
extractNum _   (CutLit x _ n) | x == num = read n
extractNum scr (CutRef _ _ _ v) = extractNum scr $ fromJust $ lookup v scr
extractNum _ _ = error "bad argument to extractNum"

-- takes a result expression to re-evaluate, a variable to repeat and start from,
-- and a number of reps. returns a list of the result var re-evaluated that many times
-- can be read as "evaluate resExpr starting from subVar, repsExpr times"
-- TODO error if subVar not in (depsOf resExpr)
cRepeatN :: CutState -> CutExpr -> Rules ExprPath
cRepeatN s@(scr,_) (CutFun t salt deps name [resExpr, subVar@(CutRef _ _ _ v), repsExpr]) =
  cRepeatEach s (CutFun t salt deps name [resExpr, subVar, subList])
  where
    subExpr = fromJust $ lookup v scr
    nReps   = extractNum scr repsExpr
    subs    = zipWith setSalt [1..nReps] (repeat subExpr)
    subList = CutSet (typeOf subExpr) 0 (depsOf subExpr) subs
cRepeatN _ _ = error "bad argument to cRepeatN"
