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
import Detourrr.Core.Paths (exprPath, toDtrPath, fromDtrPath)
import Detourrr.Core.Compile.Basic (rExpr, rLit, defaultTypeCheck, aSimpleScript)
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath  ((</>), (<.>))

dtrModule :: DtrModule
dtrModule = DtrModule
  { mName = "Plots"
  , mDesc = "Generate half-decent plots"
  , mTypes = [plot]
  -- , mFunctions = [histogram, linegraph, bargraph, scatterplot, venndiagram]
  , mFunctions = [histogram, linegraph, scatterplot]
  }

plot :: DtrType
plot = DtrType
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
varName :: DtrState -> DtrExpr -> Rules ExprPath
varName st expr = rLit st $ DtrLit str 0 $ case expr of
  (DtrRef _ _ _ (DtrVar name)) -> name
  _ -> ""

-- Like varName, but for a list of names
varNames :: DtrState -> DtrExpr -> Rules ExprPath
varNames _ expr = undefined lits -- TODO implement this
  where
    lits = DtrLit str 0 $ case expr of
             (DtrRef _ _ _ (DtrVar name)) -> name
             _ -> ""

---------------------
-- plot a num.list --
---------------------

histogram :: DtrFunction
histogram = let name = "histogram" in DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str, ListOf num] plot
  , fDesc = Nothing, fTypeDesc  = name ++ " : str num.list -> plot"
  , fFixity    = Prefix
  , fRules     = rPlotNumList "histogram.R"
  }

-- for reference:
-- dedupByContent :: DtrConfig -> Locks -> [FilePath] -> Action [FilePath]
-- dedupByContent cfg ref paths = do
--   -- TODO if the paths are already in the load cache, no need for content?
--   hashes <- mapM (hashContent cfg ref) $ map (toDtrPath cfg) paths
--   let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
--   return paths'

rPlotNumList :: FilePath -> DtrState -> DtrExpr -> Rules ExprPath
rPlotNumList script st@(_, cfg, ref, ids) expr@(DtrFun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr st title
  numsPath  <- rExpr st nums
  xlabPath  <- varName st nums
  let outPath   = exprPath st expr
      outPath'  = fromDtrPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toDtrPath cfg p) args
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
linegraph :: DtrFunction
linegraph = let name = "linegraph" in DtrFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fDesc = Nothing, fTypeDesc  = name ++ " : str num.scores -> plot"
  , fFixity    = Prefix
  , fRules     = rPlotRepeatScores "linegraph.R"
  }

-- TODO scatterplot should label axis by input var name (always there!)
scatterplot :: DtrFunction
scatterplot = let name = "scatterplot" in DtrFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fDesc = Nothing, fTypeDesc  = name ++ " : str num.scores -> plot"
  , fFixity    = Prefix
  , fRules     = rPlotRepeatScores "scatterplot.R"
  }

-- TODO take an argument for extracting the axis name
-- TODO also get y axis from dependent variable?
rPlotNumScores :: (DtrState -> DtrExpr -> Rules ExprPath)
               -> FilePath -> DtrState -> DtrExpr -> Rules ExprPath
rPlotNumScores xFn script st@(_, cfg, ref, ids) expr@(DtrFun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr st title
  numsPath  <- rExpr st nums
  xlabPath  <- xFn   st nums
  -- ylabPath  <- yFn   st nums
  let outPath   = exprPath st expr
      outPath'  = fromDtrPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toDtrPath cfg p) args
  outPath' %> \_ -> withBinHash cfg ref expr outPath $ \out ->
                      aSimpleScript script cfg ref ids (out:args')
  return outPath''
rPlotNumScores _ _ _ _ = error "bad argument to rPlotNumScores"

rPlotRepeatScores :: FilePath -> DtrState -> DtrExpr -> Rules ExprPath
rPlotRepeatScores = rPlotNumScores indRepeatVarName

indRepeatVarName :: DtrState -> DtrExpr -> Rules ExprPath
indRepeatVarName st expr = rLit st $ DtrLit str 0 $ case expr of
  (DtrFun _ _ _ _ [_, (DtrRef _ _ _ (DtrVar v)), _]) -> v
  _ -> ""

depRepeatVarName :: DtrState -> DtrExpr -> Rules ExprPath
depRepeatVarName st expr = rLit st $ DtrLit str 0 $ case expr of
  (DtrFun _ _ _ _ [_, (DtrRef _ _ _ (DtrVar v)), _]) -> v
  _ -> ""


-------------------
-- plot X.scores --
-------------------

bargraph :: DtrFunction
bargraph = undefined

----------------------
-- plot X.list.list --
----------------------

venndiagram :: DtrFunction
venndiagram = undefined
