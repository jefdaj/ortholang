module OrthoLang.Modules.Plots where

{- Finally, OrthoLang gets plotting!
 - I think this will go a long way toward convincing people it's useful.
 - The plot type is just an image for now.
 - TODO should "showing" a plot mean opening it in an image viewer? not yet
 - TODO how to name the axes?
 -}

import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Actions (withBinHash, writeCachedLines, writeLits, need')
import OrthoLang.Core.Util (digest)
import OrthoLang.Core.Paths (exprPath, toOrthoLangPath, fromOrthoLangPath, cacheDir)
import OrthoLang.Core.Compile.Basic (rExpr, rLit, defaultTypeCheck, aSimpleScript,
                                    defaultTypeCheck)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeBaseName, (</>))

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "Plots"
  , mDesc = "Generate half-decent plots"
  , mTypes = [png]
  , mFunctions = [histogram, linegraph, scatterplot, venndiagram] -- TODO bargraph
  }

png :: OrthoLangType
png = OrthoLangType
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
varName :: OrthoLangState -> OrthoLangExpr -> Rules ExprPath
varName st expr = rLit st $ OrthoLangLit str (RepeatSalt 0) $ case expr of
  (OrthoLangRef _ _ _ (OrthoLangVar _ name)) -> name
  _ -> ""

-- Like varName, but for a list of names
varNames :: OrthoLangState -> OrthoLangExpr -> Rules ExprPath
varNames _ expr = undefined lits -- TODO implement this
  where
    lits = OrthoLangLit str (RepeatSalt 0) $ case expr of
             (OrthoLangRef _ _ _ (OrthoLangVar _ name)) -> name
             _ -> ""

---------------------
-- plot a num.list --
---------------------

histogram :: OrthoLangFunction
histogram = let name = "histogram" in OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [str, ListOf num] png
  , fTypeDesc  = name ++ " : str num.list -> png"
  , fFixity    = Prefix
  , fRules     = rPlotNumList "histogram.R"
  }

-- for reference:
-- dedupByContent :: OrthoLangConfig -> Locks -> [FilePath] -> Action [FilePath]
-- dedupByContent cfg ref paths = do
--   -- TODO if the paths are already in the load cache, no need for content?
--   hashes <- mapM (hashContent cfg ref) $ map (toOrthoLangPath cfg) paths
--   let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
--   return paths'

rPlotNumList :: FilePath -> OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rPlotNumList script st@(_, cfg, ref, ids) expr@(OrthoLangFun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr st title
  numsPath  <- rExpr st nums
  xlabPath  <- varName st nums
  let outPath   = exprPath st expr
      outPath'  = fromOrthoLangPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toOrthoLangPath cfg p) args
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
linegraph :: OrthoLangFunction
linegraph = let name = "linegraph" in OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = tPlotScores
  , fTypeDesc  = name ++ " : str num.scores -> png"
  , fFixity    = Prefix
  , fRules     = rPlotRepeatScores "linegraph.R"
  }

-- TODO scatterplot should label axis by input var name (always there!)
scatterplot :: OrthoLangFunction
scatterplot = let name = "scatterplot" in OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = tPlotScores
  , fTypeDesc  = name ++ " : str num.scores -> png"
  , fFixity    = Prefix
  , fRules     = rPlotRepeatScores "scatterplot.R"
  }

-- TODO take an argument for extracting the axis name
-- TODO also get y axis from dependent variable?
rPlotNumScores :: (OrthoLangState -> OrthoLangExpr -> Rules ExprPath)
               -> FilePath -> OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rPlotNumScores xFn script st@(_, cfg, ref, ids) expr@(OrthoLangFun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr st title
  numsPath  <- rExpr st nums
  xlabPath  <- xFn   st nums
  -- ylabPath  <- yFn   st nums
  let outPath   = exprPath st expr
      outPath'  = fromOrthoLangPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toOrthoLangPath cfg p) args
  outPath' %> \_ -> withBinHash cfg ref expr outPath $ \out ->
                      aSimpleScript script cfg ref ids (out:args')
  return outPath''
rPlotNumScores _ _ _ _ = fail "bad argument to rPlotNumScores"

rPlotRepeatScores :: FilePath -> OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rPlotRepeatScores = rPlotNumScores indRepeatVarName

indRepeatVarName :: OrthoLangState -> OrthoLangExpr -> Rules ExprPath
indRepeatVarName st expr = rLit st $ OrthoLangLit str (RepeatSalt 0) $ case expr of
  (OrthoLangFun _ _ _ _ [_, (OrthoLangRef _ _ _ (OrthoLangVar _ v)), _]) -> v
  _ -> ""

depRepeatVarName :: OrthoLangState -> OrthoLangExpr -> Rules ExprPath
depRepeatVarName st expr = rLit st $ OrthoLangLit str (RepeatSalt 0) $ case expr of
  (OrthoLangFun _ _ _ _ [_, (OrthoLangRef _ _ _ (OrthoLangVar _ v)), _]) -> v
  _ -> ""

----------------------
-- plot X.list.list --
----------------------

venndiagram :: OrthoLangFunction
venndiagram = let name = "venndiagram" in OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = tPlotListOfLists
  , fTypeDesc  = name ++ " : X.list.list -> png"
  , fFixity    = Prefix
  , fRules     = rPlotListOfLists "venndiagram.R"
  }

tPlotListOfLists :: TypeChecker
tPlotListOfLists [(ListOf (ListOf _))] = Right png
tPlotListOfLists _ = Left "expected a list of lists"

-- TODO is this a reasonable way to do it for now?
plotLabel :: OrthoLangState -> OrthoLangExpr -> String
plotLabel _ (OrthoLangRef _ _ _ (OrthoLangVar _ v)) = v
plotLabel st expr = let (OrthoLangPath p) = exprPath st expr in takeBaseName p

plotCache :: OrthoLangConfig -> OrthoLangPath
plotCache cfg = cacheDir cfg "plots"

rPlotListOfLists :: FilePath -> OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rPlotListOfLists script st@(scr, cfg, ref, ids) expr@(OrthoLangFun _ _ _ _ [lol]) = do
  let labels = map (plotLabel st) (extractExprs scr lol)
      lists  = map (exprPath  st) (extractExprs scr lol)
      outPath   = exprPath st expr
      outPath'  = fromOrthoLangPath cfg outPath
      outPath'' = ExprPath outPath'
      cDir      = fromOrthoLangPath cfg $ plotCache cfg
  outPath' %> \_ -> do
    need' cfg ref "rPlotListOfLists" $ map (fromOrthoLangPath cfg) lists
    -- write labels + list paths to the cache dir
    let labPath  = cDir </> digest expr ++ "_names.txt"
        aLolPath = cDir </> digest expr ++ "_lists.txt"
    liftIO $ createDirectoryIfMissing True cDir
    writeCachedLines cfg ref labPath labels
    writeLits cfg ref aLolPath $ map (fromOrthoLangPath cfg) lists
    let args = [labPath, aLolPath]
    -- call the main script
    withBinHash cfg ref expr outPath $ \out ->
      aSimpleScript script cfg ref ids (out:map (toOrthoLangPath cfg) args)
  return outPath''
rPlotListOfLists _ _ _ = fail "bad argument to rPlotListOfLists"

-------------------
-- plot X.scores --
-------------------

bargraph :: OrthoLangFunction
bargraph = undefined
