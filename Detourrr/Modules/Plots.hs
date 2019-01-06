module Detourrr.Modules.Plots where

{- Finally, Detourrr gets plotting!
 - I think this will go a long way toward convincing people it's useful.
 - The plot type is just an image for now.
 - TODO should "showing" a plot mean opening it in an image viewer? not yet
 - TODO how to name the axes?
 -}

import Development.Shake
import Detourrr.Core.Types
import Detourrr.Core.Actions (withBinHash)
import Detourrr.Core.Paths (exprPath, toRrrPath, fromRrrPath)
import Detourrr.Core.Compile.Basic (rExpr, rLit, defaultTypeCheck, aSimpleScript)
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath  ((</>), (<.>))

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "Plots"
  , mDesc = "Generate half-decent plots"
  , mTypes = [plot]
  -- , mFunctions = [histogram, linegraph, bargraph, scatterplot, venndiagram]
  , mFunctions = [histogram, linegraph, scatterplot]
  }

plot :: RrrType
plot = RrrType
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
varName :: RrrState -> RrrExpr -> Rules ExprPath
varName st expr = rLit st $ RrrLit str 0 $ case expr of
  (RrrRef _ _ _ (RrrVar name)) -> name
  _ -> ""

-- Like varName, but for a list of names
varNames :: RrrState -> RrrExpr -> Rules ExprPath
varNames _ expr = undefined lits -- TODO implement this
  where
    lits = RrrLit str 0 $ case expr of
             (RrrRef _ _ _ (RrrVar name)) -> name
             _ -> ""

---------------------
-- plot a num.list --
---------------------

histogram :: RrrFunction
histogram = let name = "histogram" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str, ListOf num] plot
  , fDesc = Nothing, fTypeDesc  = name ++ " : str num.list -> plot"
  , fFixity    = Prefix
  , fRules     = rPlotNumList "histogram.R"
  }

-- for reference:
-- dedupByContent :: RrrConfig -> Locks -> [FilePath] -> Action [FilePath]
-- dedupByContent cfg ref paths = do
--   -- TODO if the paths are already in the load cache, no need for content?
--   hashes <- mapM (hashContent cfg ref) $ map (toRrrPath cfg) paths
--   let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
--   return paths'

rPlotNumList :: FilePath -> RrrState -> RrrExpr -> Rules ExprPath
rPlotNumList script st@(_, cfg, ref, ids) expr@(RrrFun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr st title
  numsPath  <- rExpr st nums
  xlabPath  <- varName st nums
  let outPath   = exprPath st expr
      outPath'  = fromRrrPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toRrrPath cfg p) args
  outPath' %> \_ -> withBinHash cfg ref expr outPath $ \out ->
                      aSimpleScript script cfg ref ids (out:args')
  return outPath''
rPlotNumList _ _ _ = error "bad argument to rPlotNumList"

---------------------
-- plot num.scores --
---------------------

tPlotScores :: TypeChecker
tPlotScores [s, ScoresOf n] | s == str && n == num = Right plot
tPlotScores _ = Left "expected a title and scores"

-- TODO line graph should label axis by input var name (always there!)
linegraph :: RrrFunction
linegraph = let name = "linegraph" in RrrFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fDesc = Nothing, fTypeDesc  = name ++ " : str num.scores -> plot"
  , fFixity    = Prefix
  , fRules     = rPlotRepeatScores "linegraph.R"
  }

-- TODO scatterplot should label axis by input var name (always there!)
scatterplot :: RrrFunction
scatterplot = let name = "scatterplot" in RrrFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fDesc = Nothing, fTypeDesc  = name ++ " : str num.scores -> plot"
  , fFixity    = Prefix
  , fRules     = rPlotRepeatScores "scatterplot.R"
  }

-- TODO take an argument for extracting the axis name
-- TODO also get y axis from dependent variable?
rPlotNumScores :: (RrrState -> RrrExpr -> Rules ExprPath)
               -> FilePath -> RrrState -> RrrExpr -> Rules ExprPath
rPlotNumScores xFn script st@(_, cfg, ref, ids) expr@(RrrFun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr st title
  numsPath  <- rExpr st nums
  xlabPath  <- xFn   st nums
  -- ylabPath  <- yFn   st nums
  let outPath   = exprPath st expr
      outPath'  = fromRrrPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toRrrPath cfg p) args
  outPath' %> \_ -> withBinHash cfg ref expr outPath $ \out ->
                      aSimpleScript script cfg ref ids (out:args')
  return outPath''
rPlotNumScores _ _ _ _ = error "bad argument to rPlotNumScores"

rPlotRepeatScores :: FilePath -> RrrState -> RrrExpr -> Rules ExprPath
rPlotRepeatScores = rPlotNumScores indRepeatVarName

indRepeatVarName :: RrrState -> RrrExpr -> Rules ExprPath
indRepeatVarName st expr = rLit st $ RrrLit str 0 $ case expr of
  (RrrFun _ _ _ _ [_, (RrrRef _ _ _ (RrrVar v)), _]) -> v
  _ -> ""

depRepeatVarName :: RrrState -> RrrExpr -> Rules ExprPath
depRepeatVarName st expr = rLit st $ RrrLit str 0 $ case expr of
  (RrrFun _ _ _ _ [_, (RrrRef _ _ _ (RrrVar v)), _]) -> v
  _ -> ""


-------------------
-- plot X.scores --
-------------------

bargraph :: RrrFunction
bargraph = undefined

----------------------
-- plot X.list.list --
----------------------

venndiagram :: RrrFunction
venndiagram = undefined
