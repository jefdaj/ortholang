module ShortCut.Modules.Plots where

{- Finally, ShortCut gets plotting!
 - I think this will go a long way toward convincing people it's useful.
 - The plot type is just an image for now.
 - TODO should "showing" a plot mean opening it in an image viewer? not yet
 - TODO how to name the axes?
 -}

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Actions (withBinHash, writeCachedLines, writeLits, debugNeed)
import ShortCut.Core.Util (digest)
import ShortCut.Core.Paths (exprPath, toCutPath, fromCutPath, cacheDir)
import ShortCut.Core.Compile.Basic (rExpr, rLit, defaultTypeCheck, aSimpleScript,
                                    defaultTypeCheck)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeBaseName, (</>))

cutModule :: CutModule
cutModule = CutModule
  { mName = "Plots"
  , mDesc = "Generate half-decent plots"
  , mTypes = [png]
  , mFunctions = [histogram, linegraph, scatterplot, venndiagram] -- TODO bargraph
  }

png :: CutType
png = CutType
  { tExt  = "png"
  , tDesc = "plot image"
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
varName st expr = rLit st $ CutLit str (RepeatSalt 0) $ case expr of
  (CutRef _ _ _ (CutVar _ name)) -> name
  _ -> ""

-- Like varName, but for a list of names
varNames :: CutState -> CutExpr -> Rules ExprPath
varNames _ expr = undefined lits -- TODO implement this
  where
    lits = CutLit str (RepeatSalt 0) $ case expr of
             (CutRef _ _ _ (CutVar _ name)) -> name
             _ -> ""

---------------------
-- plot a num.list --
---------------------

histogram :: CutFunction
histogram = let name = "histogram" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str, ListOf num] png
  , fTypeDesc  = name ++ " : str num.list -> png"
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
tPlotScores [s, ScoresOf n] | s == str && n == num = Right png
tPlotScores _ = Left "expected a title and scores"

-- TODO line graph should label axis by input var name (always there!)
linegraph :: CutFunction
linegraph = let name = "linegraph" in CutFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fTypeDesc  = name ++ " : str num.scores -> png"
  , fFixity    = Prefix
  , fRules     = rPlotRepeatScores "linegraph.R"
  }

-- TODO scatterplot should label axis by input var name (always there!)
scatterplot :: CutFunction
scatterplot = let name = "scatterplot" in CutFunction
  { fName      = name
  , fTypeCheck = tPlotScores
  , fTypeDesc  = name ++ " : str num.scores -> png"
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
indRepeatVarName st expr = rLit st $ CutLit str (RepeatSalt 0) $ case expr of
  (CutFun _ _ _ _ [_, (CutRef _ _ _ (CutVar _ v)), _]) -> v
  _ -> ""

depRepeatVarName :: CutState -> CutExpr -> Rules ExprPath
depRepeatVarName st expr = rLit st $ CutLit str (RepeatSalt 0) $ case expr of
  (CutFun _ _ _ _ [_, (CutRef _ _ _ (CutVar _ v)), _]) -> v
  _ -> ""

----------------------
-- plot X.list.list --
----------------------

venndiagram :: CutFunction
venndiagram = let name = "venndiagram" in CutFunction
  { fName      = name
  , fTypeCheck = tPlotListOfLists
  , fTypeDesc  = name ++ " : X.list.list -> png"
  , fFixity    = Prefix
  , fRules     = rPlotListOfLists "venndiagram.R"
  }

tPlotListOfLists :: TypeChecker
tPlotListOfLists [(ListOf (ListOf _))] = Right png
tPlotListOfLists _ = Left "expected a list of lists"

-- TODO is this a reasonable way to do it for now?
plotLabel :: CutState -> CutExpr -> String
plotLabel _ (CutRef _ _ _ (CutVar _ v)) = v
plotLabel st expr = let (CutPath p) = exprPath st expr in takeBaseName p

plotCache :: CutConfig -> CutPath
plotCache cfg = cacheDir cfg "plots"

rPlotListOfLists :: FilePath -> CutState -> CutExpr -> Rules ExprPath
rPlotListOfLists script st@(scr, cfg, ref, ids) expr@(CutFun _ _ _ _ [lol]) = do
  (ExprPath lPath) <- rExpr st lol -- TODO is this needed?
  -- (lists, labels) <- fmap unzip $ mapM (\e -> (rExpr st e, return $ plotLabel st e)) (extractExprs scr lol)
  listsNLabels <- mapM (\e -> do
                          p <- rExpr st e
                          return (p, plotLabel st e)) (extractExprs scr lol)
  let (lists, labels) = unzip listsNLabels
  -- let (labels, lists) = unzip $ map (\e -> (plotLabel st e, exprPath st e)) (extractExprs scr lol)
  let lists'  = map (\(ExprPath p) -> p) lists
      -- ePaths' = map (\(ExprPath p) -> p) ePaths
      outPath   = exprPath st expr
      outPath'  = fromCutPath cfg outPath
      outPath'' = ExprPath outPath'
      cDir      = fromCutPath cfg $ plotCache cfg
      labPath  = cDir </> digest expr ++ "_names.txt"
      aLolPath = cDir </> digest expr ++ "_lists.txt"
  labPath  %> \_ -> do
    -- debugNeed cfg "rPlotListOfLists" (lPath:ePaths')
    liftIO $ createDirectoryIfMissing True cDir
    -- liftIO $ putStrLn $ "labels: " ++ show labels
    writeCachedLines cfg ref labPath labels
  aLolPath %> \_ -> do
    -- debugNeed cfg "rPlotListOfLists" (lPath:ePaths')
    liftIO $ createDirectoryIfMissing True cDir
    -- liftIO $ putStrLn $ "lists: " ++ show lists
    debugNeed cfg "rPlotListOfLists" lists'
    writeLits cfg ref aLolPath lists'
  outPath' %> \_ -> do
    debugNeed cfg "rPlotListOfLists" (lPath:labPath:aLolPath:[])
    -- debugNeed cfg "rPlotListOfLists" [labPath, aLolPath]
    withBinHash cfg ref expr outPath $ \out ->
      aSimpleScript script cfg ref ids (out:map (toCutPath cfg) [labPath, aLolPath])
  return outPath''
rPlotListOfLists _ _ _ = fail "bad argument to rPlotListOfLists"

-------------------
-- plot X.scores --
-------------------

bargraph :: CutFunction
bargraph = undefined
