module ShortCut.Modules.Plots where

{- Finally, ShortCut gets plotting!
 - I think this will go a long way toward convincing people it's useful.
 - The plot type is just an image for now.
 - TODO should "showing" a plot mean opening it in an image viewer? not yet
 - TODO how to name the axes?
 -}

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Paths (exprPath, toCutPath, fromCutPath)
import ShortCut.Core.Compile.Basic (rExpr, rLit, defaultTypeCheck, aSimpleScript)

cutModule :: CutModule
cutModule = CutModule
  { mName = "plots"
  -- , mFunctions = [histogram, linegraph, bargraph, scatterplot, venndiagram]
  , mFunctions = [histogram, linegraph, scatterplot]
  }

plot :: CutType
plot = CutType
  { tExt  = "png"
  , tDesc = "png image of a plot"
  , tShow = \_ f -> return $ "png image '" ++ f ++ "'"
  }

---------------
-- utilities --
---------------

{- If the user calls a plotting function with a named variable like
 - "num_genomes", this will write that name to a string for use in the plot.
 - Otherwise it will return an empty string, which the script should ignore.
 -}
varName :: CutState -> CutExpr -> Rules ExprPath
varName st expr = rLit st $ CutLit str 0 $ case expr of
  (CutRef _ _ _ (CutVar name)) -> name
  _ -> ""

-- Like varName, but for a list of names
varNames :: CutState -> [CutExpr] -> Rules ExprPath
varNames = undefined

rPlot :: FilePath -> CutState -> CutExpr -> Rules ExprPath
rPlot script st@(_,cfg,ref) expr@(CutFun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr st title
  numsPath  <- rExpr st nums
  xlabPath  <- varName st nums
  let outPath   = exprPath st expr
      outPath'  = fromCutPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [outPath'', titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toCutPath cfg p) args
  outPath' %> \_ -> aSimpleScript script cfg ref args'
  return outPath''
rPlot _ _ _ = error "bad argument to rPlot"

---------------------
-- plot a num.list --
---------------------

histogram :: CutFunction
histogram = let name = "histogram" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str, ListOf num] plot
  , fTypeDesc  = name ++ " : str num.list -> plot"
  , fFixity    = Prefix
  , fRules     = rPlot "histogram.R"
  }

---------------------
-- plot num.scores --
---------------------

tPlotScores :: TypeChecker
tPlotScores [s, ScoresOf n] | s == str && n == num = Right plot
tPlotScores _ = Left "expected a title and scores"

-- TODO use varNames instead of varName
linegraph :: CutFunction
linegraph = let name = "linegraph" in CutFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fTypeDesc  = name ++ " : str num.scores -> plot"
  , fFixity    = Prefix
  , fRules     = rPlot "linegraph.R"
  }

-- TODO use varNames instead of varName
scatterplot :: CutFunction
scatterplot = let name = "scatterplot" in CutFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fTypeDesc  = name ++ " : str num.scores -> plot"
  , fFixity    = Prefix
  , fRules     = rPlot "scatterplot.R"
  }

----------------------------
-- plot <whatever>.scores --
----------------------------

bargraph = undefined

-------------------------------
-- plot <whatever>.list.list --
-------------------------------

venndiagram = undefined
