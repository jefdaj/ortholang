module OrthoLang.Modules.Plots where

{- Finally, OrthoLang gets plotting!
 - I think this will go a long way toward convincing people it's useful.
 - The plot type is just an image for now.
 - TODO should "showing" a plot mean opening it in an image viewer? not yet
 - TODO how to name the axes?
 -}

import Development.Shake
import OrthoLang.Core

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeBaseName, (</>))
import Data.Maybe (fromJust)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "Plots"
  , mDesc = "Generate half-decent plots"
  , mTypes = [png]
  , mFunctions = [histogram, linegraph, scatterplot, venndiagram] -- TODO bargraph
  }

png :: Type
png = Type
  { tExt  = "png"
  , tDesc = "plot image"
  , tShow = \_ _ f -> return $ "plot image \"" ++ f ++ "\""
  }

-------------------
-- get var names --
-------------------

{- If the user calls a plotting function with a named variable like
 - "num_genomes", this will write that name to a string for use in the plot.
 - Otherwise it will return an empty string, which the script should ignore.
 -}
varName :: RulesFn
varName scr expr = rExpr scr $ Lit str (Salt 0) $ case expr of
  (Ref _ _ _ (Var _ name)) -> name
  _ -> ""

-- Like varName, but for a list of names
varNames :: RulesFn
varNames _ expr = undefined lits -- TODO implement this
  where
    lits = Lit str (Salt 0) $ case expr of
             (Ref _ _ _ (Var _ name)) -> name
             _ -> ""

---------------------
-- plot a num.list --
---------------------

histogram :: Function
histogram = let name = "histogram" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str, ListOf num] png
  , fTypeDesc  = name ++ " : str num.list -> png"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rPlotNumList "histogram.R"
  }

-- for reference:
-- dedupByContent :: Config -> LocksRef -> [FilePath] -> Action [FilePath]
-- dedupByContent cfg ref paths = do
--   -- TODO if the paths are already in the load cache, no need for content?
--   hashes <- mapM (hashContent cfg ref) $ map (toPath cfg) paths
--   let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
--   return paths'

rPlotNumList :: FilePath -> RulesFn
rPlotNumList script scr expr@(Fun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr scr title
  numsPath  <- rExpr scr nums
  xlabPath  <- varName scr nums
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let outPath   = exprPath cfg dRef scr expr
      outPath'  = fromPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toPath cfg p) args
  outPath' %> \_ -> withBinHash expr outPath $ \out ->
                      aSimpleScript script (out:args')
  return outPath''
rPlotNumList _ _ _ = fail "bad argument to rPlotNumList"

---------------------
-- plot num.scores --
---------------------

tPlotScores :: TypeChecker
tPlotScores [s, ScoresOf n] | s == str && n == num = Right png
tPlotScores _ = Left "expected a title and scores"

-- TODO line graph should label axis by input var name (always there!)
linegraph :: Function
linegraph = let name = "linegraph" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = tPlotScores
  , fTypeDesc  = name ++ " : str num.scores -> png"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rPlotRepeatScores "linegraph.R"
  }

-- TODO scatterplot should label axis by input var name (always there!)
scatterplot :: Function
scatterplot = let name = "scatterplot" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = tPlotScores
  , fTypeDesc  = name ++ " : str num.scores -> png"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rPlotRepeatScores "scatterplot.R"
  }

-- TODO take an argument for extracting the axis name
-- TODO also get y axis from dependent variable?
rPlotNumScores :: RulesFn -> FilePath -> RulesFn
rPlotNumScores xFn sPath scr expr@(Fun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr scr title
  numsPath  <- rExpr scr nums
  xlabPath  <- xFn   scr nums
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  -- ylabPath  <- yFn   st nums
  let outPath   = exprPath cfg dRef scr expr
      outPath'  = fromPath cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toPath cfg p) args
  outPath' %> \_ -> withBinHash expr outPath $ \out ->
                      aSimpleScript sPath (out:args')
  return outPath''
rPlotNumScores _ _ _ _ = fail "bad argument to rPlotNumScores"

rPlotRepeatScores :: FilePath -> RulesFn
rPlotRepeatScores = rPlotNumScores indRepeatVarName

indRepeatVarName :: RulesFn
indRepeatVarName scr expr = rExpr scr $ Lit str (Salt 0) $ case expr of
  (Fun _ _ _ _ [_, (Ref _ _ _ (Var _ v)), _]) -> v
  _ -> ""

depRepeatVarName :: RulesFn
depRepeatVarName scr expr = rExpr scr $ Lit str (Salt 0) $ case expr of
  (Fun _ _ _ _ [_, (Ref _ _ _ (Var _ v)), _]) -> v
  _ -> ""

----------------------
-- plot X.list.list --
----------------------

venndiagram :: Function
venndiagram = let name = "venndiagram" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = tPlotListOfLists
  , fTypeDesc  = name ++ " : X.list.list -> png"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rPlotListOfLists "venndiagram.R"
  }

tPlotListOfLists :: TypeChecker
tPlotListOfLists [(ListOf (ListOf _))] = Right png
tPlotListOfLists _ = Left "expected a list of lists"

-- TODO is this a reasonable way to do it for now?
plotLabel :: Config -> DigestsRef -> Script -> Expr -> String
plotLabel _ _ _ (Ref _ _ _ (Var _ v)) = v
plotLabel cfg dRef scr expr = let (Path p) = exprPath cfg dRef scr expr in takeBaseName p

plotCache :: Config -> Path
plotCache cfg = cacheDir cfg "plots"

rPlotListOfLists :: FilePath -> RulesFn
rPlotListOfLists sPath scr expr@(Fun _ _ _ _ [lol]) = do
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let labels = map (plotLabel cfg dRef scr) (extractExprs scr lol)
      lists  = map (exprPath  cfg dRef scr) (extractExprs scr lol)
      outPath   = exprPath cfg dRef scr expr
      outPath'  = fromPath cfg outPath
      outPath'' = ExprPath outPath'
      cDir      = fromPath cfg $ plotCache cfg
  outPath' %> \_ -> do
    need' "rPlotListOfLists" $ map (fromPath cfg) lists
    -- write labels + list paths to the cache dir
    let labPath  = cDir </> digest expr ++ "_names.txt"
        aLolPath = cDir </> digest expr ++ "_lists.txt"
    liftIO $ createDirectoryIfMissing True cDir
    writeCachedLines labPath labels
    writeLits aLolPath $ map (fromPath cfg) lists
    let args = [labPath, aLolPath]
    -- call the main script
    withBinHash expr outPath $ \out ->
      aSimpleScript sPath (out:map (toPath cfg) args)
  return outPath''
rPlotListOfLists _ _ _ = fail "bad argument to rPlotListOfLists"

-------------------
-- plot X.scores --
-------------------

bargraph :: Function
bargraph = undefined
