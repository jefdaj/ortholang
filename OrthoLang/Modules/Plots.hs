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
  , mFunctions =
      [ histogram, histogramExplicit
      , linegraph, linegraphExplicit
      -- , scatterplot
      , venndiagram
      ] -- TODO bargraph
  }

png :: Type
png = Type
  { tExt  = "png"
  , tDesc = "plot image"
  , tShow = \cfg _ path -> do
      -- resolving symlinks fixes a bug in the jupyter kernel when returning multiple plots
      -- (without it they'd all just be vars/result and erroneously return the last result)
      path' <- resolveSymlinks (Just [tmpdir cfg]) path
      return $ "plot image \"" ++ toGeneric cfg path' ++ "\""
  }

-------------------
-- get var names --
-------------------

{- If the user calls a plotting function with a named variable like
 - "num_genomes", this will write that name to a string for use in the plot.
 - Otherwise it will return an empty string, which the script should ignore.
 -}
varName' :: String -> Expr -> String
varName' _ (Ref r _ _ (Var _ name)) = name ++ "." ++ ext r
varName' def expr = def ++ "." ++ ext (typeOf expr)

varName d e = let r = varName' d e
                  m = "varName " ++ show d ++ " " ++ show e ++ " -> " ++ show r
              in trace "ortholang.modules.plots.varName" m r

{- Like varName, but for a list of names. Cases it can handle:
 - 1. list contains a single list or ref to a list -> recurse
 - 2. list contains multiple elements -> get their names or default to 'unnamed1', 'unnamed2`, ...
 - 3. anything else -> default to one 'unnamed' element?
 - TODO this is wrong when the only arg is a list, because it un-nests but the paths don't
 -      solve by not recursing at all? just make a dir + name its elements the defaults
 - TODO use the var name as default name for its list elements: hits1, hits2, ...
 - TODO remove the Lst wrapping part and do it separately?
 -}
listVarNames' :: String -> Script -> [Expr] -> Expr
listVarNames' def scr es =
  let indexed = zip [(1 :: Int)..] es
      varNameDef (i, e) = varName (def ++ show i) e -- TODO bug here?
      names = map varNameDef indexed
  in Lst str Nothing [] (map (Lit str) names) -- TODO should deps be empty, or contain the mapped vars?

listVarNames d s es = let r = listVarNames' d s es
                          m = "listVarNames " ++ show d ++ " " ++ show es ++ " -> " ++ show r
                      in trace "ortholang.modules.plots.listVarNames" m r

---------------------
-- plot a num.list --
---------------------

histogramExplicit :: Function
histogramExplicit = newFnS3
  "histogram_explicit"
  (Exactly str, Exactly str, ListSigs (Exactly num))
  (Exactly png)
  "histogram.R"
  [Hidden]
  id

-- | User-facing version that auto-loads the script and captures any varnames in the untyped list.
histogram :: Function
histogram = newExprExpansion
  "histogram"
  [Exactly str, ListSigs (Exactly num)]
  (Exactly png)
  mHistogram
  []

-- | Macro that adds the xlab str
mHistogram :: ExprExpansion
mHistogram _ scr (Fun r ms ds _ [title, ns]) =
  let xlab = Lit str $ varName "" ns
  in Fun r ms ds "histogram_explicit" [title, xlab, ns]
mHistogram _ _ e = error "modules.plots.mHistogram" $ "bad argument: " ++ show e

---------------------
-- plot num.scores --
---------------------

-- TODO any reason this wouldn't work with other things besides the .scores files?
linegraphExplicit :: Function
linegraphExplicit = newFnS3
  "linegraph_explicit"
  (Exactly str, Exactly str, ScoresSigs (Exactly num))
  (Exactly png)
  "linegraph.R"
  [Hidden]
  id

-- | User-facing version that auto-loads the script and captures any varnames in the untyped list.
linegraph :: Function
linegraph = newExprExpansion
  "linegraph"
  [Exactly str, ScoresSigs (Exactly num)]
  (Exactly png)
  mLinegraph
  []

-- | Macro that adds the xlab str
mLinegraph :: ExprExpansion
mLinegraph _ scr (Fun r ms ds _ [title, ns]) =
  let xlab = Lit str $ varName "" ns
  in Fun r ms ds "linegraph_explicit" [title, xlab, ns]
mLinegraph _ _ e = error "modules.plots.mLinegraph" $ "bad argument: " ++ show e

scatterplot = undefined

-- (str, ScoresOf num) png
-- shown as "str num.scores -> png"
-- tPlotScores :: TypeChecker
-- tPlotScores [s, ScoresOf n] | s == str && n == num = Right png
-- tPlotScores _ = Left "expected a title and scores"

-- TODO line graph should label axis by input var name (always there!)
-- linegraph :: Function
-- linegraph = let name = "linegraph" in Function
--   { fOpChar = Nothing, fName = name
--   -- , fTypeCheck = tPlotScores
--   -- , fTypeDesc  = name ++ " : str num.scores -> png"
--   , fInputs = [Exactly str, Exactly (ScoresOf num)]
--   , fOutput =  Exactly png
--   ,fTags = []
--   , fNewRules = NewNotImplemented, fOldRules = rPlotRepeatScores "linegraph.R"
--   }

-- TODO scatterplot should label axis by input var name (always there!)
-- scatterplot :: Function
-- scatterplot = let name = "scatterplot" in Function
--   { fOpChar = Nothing, fName = name
--   -- , fTypeCheck = tPlotScores
--   -- , fTypeDesc  = name ++ " : str num.scores -> png"
--   , fInputs = [Exactly str, Exactly (ScoresOf num)]
--   , fOutput =  Exactly png
--   ,fTags = []
--   , fNewRules = NewNotImplemented, fOldRules = rPlotRepeatScores "scatterplot.R"
--   }

-- TODO take an argument for extracting the axis name
-- TODO also get y axis from dependent variable?
-- rPlotNumScores :: RulesFn -> FilePath -> RulesFn
-- rPlotNumScores xFn sPath scr expr@(Fun _ _ _ _ [title, nums]) = do
--   titlePath <- rExpr scr title
--   numsPath  <- rExpr scr nums
--   xlabPath  <- xFn   scr nums
--   cfg  <- fmap fromJust getShakeExtraRules
--   dRef <- fmap fromJust getShakeExtraRules
--   -- ylabPath  <- yFn   st nums
--   let loc = "modules.plots.rPlotNumScores"
--       outPath   = exprPath cfg dRef scr expr
--       outPath'  = fromPath loc cfg outPath
--       outPath'' = ExprPath outPath'
--       args      = [titlePath, numsPath, xlabPath]
--       args'     = map (\(ExprPath p) -> toPath loc cfg p) args
--   outPath' %> \_ -> withBinHash expr outPath $ \out ->
--                       aSimpleScript sPath (out:args')
--   return outPath''
-- rPlotNumScores _ _ _ _ = fail "bad argument to rPlotNumScores"
-- 
-- rPlotRepeatScores :: FilePath -> RulesFn
-- rPlotRepeatScores = rPlotNumScores indRepeatVarName
-- 
-- indRepeatVarName :: RulesFn
-- indRepeatVarName scr expr = rExpr scr $ Lit str $ case expr of
--   (Fun _ _ _ _ [_, (Ref _ _ _ (Var _ v)), _]) -> v
--   _ -> ""

-- depRepeatVarName :: RulesFn
-- depRepeatVarName scr expr = rExpr scr $ Lit str $ case expr of
  -- (Fun _ _ _ _ [_, (Ref _ _ _ (Var _ v)), _]) -> v
  -- _ -> ""

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
