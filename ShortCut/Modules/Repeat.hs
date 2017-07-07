module ShortCut.Modules.Repeat where

import Development.Shake
import ShortCut.Core.Types

import Data.Maybe            (fromJust)
import ShortCut.Core.Paths   (hashedTmp)
import ShortCut.Core.Compile (cExpr, addPrefixes, compileScript)
import System.FilePath       (makeRelative)
import ShortCut.Core.Util         (digest)

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
tRepeatEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
tRepeatEach _ = Left "invalid args to repeat_each" -- TODO better errors here

-- TODO will this get printed, or will there just be a parse error?
explainFnBug :: String
explainFnBug =
  "You've stumbled on an outstanding bug. Sorry about that! \
  \The problem is that when doing transformations involving lists \
  \like repeat or map, ShortCut can't \"see\" through future function calls; \
  \it can only manipulate lists whose elements are known *before* running the \
  \program. If you want Jeff to consider rewriting some things to fix that, \
  \drop him a line!"

-- TODO what if it's a function call?
-- do we have to make a rule that you can't use those?
-- (uuuugly! but not a show-stopper for now)
extractExprs :: CutScript -> CutExpr -> [CutExpr]
extractExprs  _  (CutList _ _ _ es) = es
extractExprs scr (CutRef  _ _ _ v ) = extractExprs scr $ fromJust $ lookup v scr
extractExprs _   (CutFun _ _ _ _ _) = error explainFnBug
extractExprs  _   e               = error $ "bad arg to extractExpr: " ++ show e

-- TODO ideally, this shouldn't need any custom digesting? but whatever no
--      messing with it for now
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
tRepeatN [rType, _, num] = Right $ ListOf rType
tRepeatN _ = Left "invalid args to repeatN"

extractNum :: CutScript -> CutExpr -> Int
extractNum _   (CutLit num _ n) = read n
extractNum scr (CutRef _ _ _ v) = extractNum scr $ fromJust $ lookup v scr
extractNum _ _ = error "bad argument to extractNum"

-- takes a result expression to re-evaluate, a variable to repeat and start from,
-- and a number of reps. returns a list of the result var re-evaluated that many times
-- can be read as "evaluate resExpr starting from subVar, repsExpr times"
-- TODO error if subVar not in (depsOf resExpr)
cRepeatN :: CutState -> CutExpr -> Rules FilePath
cRepeatN s@(scr,cfg) e@(CutFun t salt deps name
                               [resExpr, subVar@(CutRef _ _ _ v), repsExpr]) =
  cRepeatEach s (CutFun t salt deps name [resExpr, subVar, subList])
  where
    subExpr = fromJust $ lookup v scr
    nReps   = extractNum scr repsExpr
    subs    = zipWith setSalt [1..nReps] (repeat subExpr)
    subList = CutList (typeOf subExpr) 0 (depsOf subExpr) subs
cRepeatN _ _ = error "bad argument to cRepeatN"
