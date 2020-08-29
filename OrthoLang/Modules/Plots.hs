module OrthoLang.Modules.Plots where

{- Finally, OrthoLang gets plotting!
 - I think this will go a long way toward convincing people it's useful.
 - The plot type is just an image for now.
 - TODO should "showing" a plot mean opening it in an image viewer? not yet
 - TODO how to name the axes?
 -}

import Development.Shake
import OrthoLang.Types
import OrthoLang.Script (extractExprs)
import OrthoLang.Interpreter

import System.Directory (createDirectoryIfMissing)
import System.FilePath  (takeBaseName, (</>))
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "Plots"
  , mDesc = "Generate half-decent plots"
  , mTypes = [png]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [histogram, linegraph, scatterplot, venndiagram] -- TODO bargraph
  }

png :: Type
png = Type
  { tExt  = "png"
  , tDesc = "plot image"
  , tShow = \cfg _ path -> do
      -- resolving symlinks fixes a bug in the jupyter kernel when returning multiple plots
      -- (without it they'd all just be vars/result and erroneously return the last result)
      path' <- resolveSymlinks (Just [tmpdir cfg]) path
      return $ "plot image \"" ++ path' ++ "\""
  }

-------------------
-- get var names --
-------------------

{- If the user calls a plotting function with a named variable like
 - "num_genomes", this will write that name to a string for use in the plot.
 - Otherwise it will return an empty string, which the script should ignore.
 -}
varName :: String -> Expr -> Expr
varName def expr = Lit str $ case expr of
  (Ref r _ _ (Var _ name)) -> name ++ "." ++ ext r
  e -> def ++ "." ++ ext (typeOf e)

{- Like varName, but for a list of names. Cases it can handle:
 - 1. list contains a single list or ref to a list -> recurse
 - 2. list contains multiple elements -> get their names or default to 'unnamed1', 'unnamed2`, ...
 - 3. anything else -> default to one 'unnamed' element?
 -}
listVarNames :: Script -> [Expr] -> Expr
listVarNames scr es = case es of
  [Lst _ _ _ es'] -> listVarNames scr es'
  [Ref _ _ _ (Var _ name)] -> let e = fromJust $ lookupExpr name (sAssigns scr)
                              in listVarNames scr [e]
  es' -> let indexed = zip [(1 :: Int)..] es'
             varNameDef (i, e) = varName ("input" ++ show i) e
             mapped = map varNameDef indexed
         in Lst str Nothing [] mapped -- TODO should deps be empty, or contain the mapped vars?

---------------------
-- plot a num.list --
---------------------

histogram :: Function
histogram = let name = "histogram" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [str, ListOf num] png
  -- , fTypeDesc  = name ++ " : str num.list -> png"
  , fInputs = [Exactly str, Exactly (ListOf num)]
  , fOutput =  Exactly png
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rPlotNumList "histogram.R"
  }

-- for reference:
-- dedupByContent :: Config -> LocksRef -> [FilePath] -> Action [FilePath]
-- dedupByContent cfg ref paths = do
--   -- TODO if the paths are already in the load cache, no need for content?
--   hashes <- mapM (hashContent cfg ref) $ map (toPath loc cfg) paths
--   let paths' = map fst $ nubBy ((==) `on` snd) $ zip paths hashes
--   return paths'

rPlotNumList :: FilePath -> RulesFn
rPlotNumList binPath scr expr@(Fun _ _ _ _ [title, nums]) = do
  titlePath <- rExpr scr title
  numsPath  <- rExpr scr nums
  xlabPath  <- rExpr scr $ varName "nums" nums -- TODO better default name
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "modules.plots.rPlotNumList"
      outPath   = exprPath cfg dRef scr expr
      outPath'  = fromPath loc cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toPath loc cfg p) args
  outPath' %> \_ -> withBinHash expr outPath $ \out ->
                      aSimpleScript binPath (out:args')
  return outPath''
rPlotNumList _ _ _ = fail "bad argument to rPlotNumList"

---------------------
-- plot num.scores --
---------------------

-- (str, ScoresOf num) png
-- shown as "str num.scores -> png"
-- tPlotScores :: TypeChecker
-- tPlotScores [s, ScoresOf n] | s == str && n == num = Right png
-- tPlotScores _ = Left "expected a title and scores"

-- TODO line graph should label axis by input var name (always there!)
linegraph :: Function
linegraph = let name = "linegraph" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = tPlotScores
  -- , fTypeDesc  = name ++ " : str num.scores -> png"
  , fInputs = [Exactly str, Exactly (ScoresOf num)]
  , fOutput =  Exactly png
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rPlotRepeatScores "linegraph.R"
  }

-- TODO scatterplot should label axis by input var name (always there!)
scatterplot :: Function
scatterplot = let name = "scatterplot" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = tPlotScores
  -- , fTypeDesc  = name ++ " : str num.scores -> png"
  , fInputs = [Exactly str, Exactly (ScoresOf num)]
  , fOutput =  Exactly png
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rPlotRepeatScores "scatterplot.R"
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
  let loc = "modules.plots.rPlotNumScores"
      outPath   = exprPath cfg dRef scr expr
      outPath'  = fromPath loc cfg outPath
      outPath'' = ExprPath outPath'
      args      = [titlePath, numsPath, xlabPath]
      args'     = map (\(ExprPath p) -> toPath loc cfg p) args
  outPath' %> \_ -> withBinHash expr outPath $ \out ->
                      aSimpleScript sPath (out:args')
  return outPath''
rPlotNumScores _ _ _ _ = fail "bad argument to rPlotNumScores"

rPlotRepeatScores :: FilePath -> RulesFn
rPlotRepeatScores = rPlotNumScores indRepeatVarName

indRepeatVarName :: RulesFn
indRepeatVarName scr expr = rExpr scr $ Lit str $ case expr of
  (Fun _ _ _ _ [_, (Ref _ _ _ (Var _ v)), _]) -> v
  _ -> ""

depRepeatVarName :: RulesFn
depRepeatVarName scr expr = rExpr scr $ Lit str $ case expr of
  (Fun _ _ _ _ [_, (Ref _ _ _ (Var _ v)), _]) -> v
  _ -> ""

----------------------
-- plot X.list.list --
----------------------

venndiagram :: Function
venndiagram = let name = "venndiagram" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = tPlotListOfLists
  -- , fTypeDesc  = name ++ " : X.list.list -> png"
  , fInputs = [ListSigs (ListSigs (AnyType "the type to count unique values of"))]
  , fOutput = Exactly png
  , fTags = [] -- TODO add set names as an input str.list so they get included in hashes!
  , fNewRules = NewNotImplemented
  , fOldRules = rPlotListOfLists "venndiagram.R"
  }

-- (ListOf (ListOf (Some ot "any type"))) png
-- shown as "t.list.list -> png, where t is any type"
-- TODO does that actually make sense for specifying something you can plot?
--      yes but only for venn diagrams
-- tPlotListOfLists :: TypeChecker
-- tPlotListOfLists [(ListOf (ListOf _))] = Right png
-- tPlotListOfLists _ = Left "expected a list of lists"

-- TODO is this a reasonable way to do it for now?
plotLabel :: Config -> DigestsRef -> Script -> Expr -> String
plotLabel _ _ _ (Ref _ _ _ (Var _ v)) = v
plotLabel cfg dRef scr expr = let (Path p) = exprPath cfg dRef scr expr in takeBaseName p

extractVarNames :: Config -> DigestsRef -> Script -> Expr -> [String]
extractVarNames cfg dRef scr expr = map (plotLabel cfg dRef scr) (extractExprs scr expr)

plotCache :: Config -> Path
plotCache cfg = cacheDir cfg "plots"

rPlotListOfLists :: FilePath -> RulesFn
rPlotListOfLists sPath scr expr@(Fun _ _ _ _ [lol]) = do
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let labels = extractVarNames cfg dRef scr lol
      lists  = map (exprPath  cfg dRef scr) (extractExprs scr lol)
      outPath   = exprPath cfg dRef scr expr
      outPath'  = fromPath loc cfg outPath
      outPath'' = ExprPath outPath'
      cDir      = fromPath loc cfg $ plotCache cfg
      loc = "modules.plots.rPlotListOfLists"
  outPath' %> \_ -> do
    need' loc $ map (fromPath loc cfg) lists
    -- write labels + list paths to the cache dir
    let labPath  = cDir </> digest loc expr ++ "_names.txt"
        aLolPath = cDir </> digest loc expr ++ "_lists.txt"
    liftIO $ createDirectoryIfMissing True cDir
    writeCachedLines loc labPath labels
    writeLits loc aLolPath $ map (fromPath loc cfg) lists
    let args = [labPath, aLolPath]
    -- call the main script
    withBinHash expr outPath $ \out ->
      aSimpleScript sPath (out:map (toPath loc cfg) args)
  return outPath''
rPlotListOfLists _ _ _ = fail "bad argument to rPlotListOfLists"

-------------------
-- plot X.scores --
-------------------

bargraph :: Function
bargraph = undefined
