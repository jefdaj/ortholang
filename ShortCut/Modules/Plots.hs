module ShortCut.Modules.Plots where

{- Finally, ShortCut gets plotting!
 - I think this will go a long way toward convincing people it's useful.
 - The plot type is just an image for now.
 - TODO should "showing" a plot mean opening it in an image viewer? not yet
 - TODO how to name the axes?
 -}

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Actions (withBinHash)
import ShortCut.Core.Paths (exprPath, toCutPath, fromCutPath)
import ShortCut.Core.Compile.Basic (rExpr, rLit, defaultTypeCheck, aSimpleScript)
import ShortCut.Core.Parse.Basic (initialRandomSeed)
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath  ((</>), (<.>))

cutModule :: CutModule
cutModule = CutModule
  { mName = "Plots"
  , mDesc = "Generate half-decent plots"
  , mTypes = [plot]
  -- , mFunctions = [histogram, linegraph, bargraph, scatterplot, venndiagram]
  , mFunctions = [histogram, linegraph, scatterplot]
  }

plot :: CutType
plot = CutType
  { tExt  = "png"
  , tDesc = "png image of a plot"
  , tShow = \_ _ f -> return $ "plot image '" ++ f ++ "'"
  }

-------------------
-- get var names --
-------------------

{- If the user calls a plotting function with a named variable like
 - "num_genomes", this will write that name to a string for use in the plot.
 - Otherwise it will return an empty string, which the script should ignore.
 -}
varName :: CutState -> CutExpr -> Rules ExprPath
varName st expr = rLit st $ CutLit str initialRandomSeed $ case expr of
  (CutRef _ _ _ (CutVar name)) -> name
  _ -> ""

-- Like varName, but for a list of names
varNames :: CutState -> CutExpr -> Rules ExprPath
varNames _ expr = undefined lits -- TODO implement this
  where
    lits = CutLit str initialRandomSeed $ case expr of
             (CutRef _ _ _ (CutVar name)) -> name
             _ -> ""

---------------------
-- plot a num.list --
---------------------

histogram :: CutFunction
histogram = let name = "histogram" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str, ListOf num] plot
  , fDesc = Nothing, fTypeDesc  = name ++ " : str num.list -> plot"
  , fFixity    = Prefix
  , fRules     = rPlotNumList "histogram.R"
  }

-- for reference:
-- dedupByContent :: CutConfig -> Locks -> [FilePath] -> Action [FilePath]
-- dedupByContent cfg ref paths = do
--   -- TODO if the paths are already in the load cache, no need for content?
--   hashes <- mapM (hashContent cfg ref) $ map (toCutPath cfg) paths
--   let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
--   return paths'

rPlotNumList :: FilePath -> CutState -> CutExpr -> Rules ExprPath
rPlotNumList script st@(_, cfg, ref, ids) expr@(CutFun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr st title
  numsPath  <- rExpr st nums
  xlabPath  <- varName st nums
  let outPath   = exprPath st expr
      outPath'  = fromCutPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toCutPath cfg p) args
  outPath' %> \_ -> withBinHash cfg ref expr outPath $ \out ->
                      aSimpleScript script cfg ref ids (out:args')
  return outPath''
rPlotNumList _ _ _ = fail "bad argument to rPlotNumList"

---------------------
-- plot num.scores --
---------------------

tPlotScores :: TypeChecker
tPlotScores [s, ScoresOf n] | s == str && n == num = Right plot
tPlotScores _ = Left "expected a title and scores"

-- TODO line graph should label axis by input var name (always there!)
linegraph :: CutFunction
linegraph = let name = "linegraph" in CutFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fDesc = Nothing, fTypeDesc  = name ++ " : str num.scores -> plot"
  , fFixity    = Prefix
  , fRules     = rPlotRepeatScores "linegraph.R"
  }

-- TODO scatterplot should label axis by input var name (always there!)
scatterplot :: CutFunction
scatterplot = let name = "scatterplot" in CutFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fDesc = Nothing, fTypeDesc  = name ++ " : str num.scores -> plot"
  , fFixity    = Prefix
  , fRules     = rPlotRepeatScores "scatterplot.R"
  }

-- TODO take an argument for extracting the axis name
-- TODO also get y axis from dependent variable?
rPlotNumScores :: (CutState -> CutExpr -> Rules ExprPath)
               -> FilePath -> CutState -> CutExpr -> Rules ExprPath
rPlotNumScores xFn script st@(_, cfg, ref, ids) expr@(CutFun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr st title
  numsPath  <- rExpr st nums
  xlabPath  <- xFn   st nums
  -- ylabPath  <- yFn   st nums
  let outPath   = exprPath st expr
      outPath'  = fromCutPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toCutPath cfg p) args
  outPath' %> \_ -> withBinHash cfg ref expr outPath $ \out ->
                      aSimpleScript script cfg ref ids (out:args')
  return outPath''
rPlotNumScores _ _ _ _ = fail "bad argument to rPlotNumScores"

rPlotRepeatScores :: FilePath -> CutState -> CutExpr -> Rules ExprPath
rPlotRepeatScores = rPlotNumScores indRepeatVarName

indRepeatVarName :: CutState -> CutExpr -> Rules ExprPath
indRepeatVarName st expr = rLit st $ CutLit str initialRandomSeed $ case expr of
  (CutFun _ _ _ _ [_, (CutRef _ _ _ (CutVar v)), _]) -> v
  _ -> ""

depRepeatVarName :: CutState -> CutExpr -> Rules ExprPath
depRepeatVarName st expr = rLit st $ CutLit str initialRandomSeed $ case expr of
  (CutFun _ _ _ _ [_, (CutRef _ _ _ (CutVar v)), _]) -> v
  _ -> ""


-------------------
-- plot X.scores --
-------------------

bargraph :: CutFunction
bargraph = undefined

----------------------
-- plot X.list.list --
----------------------

venndiagram :: CutFunction
venndiagram = undefined
