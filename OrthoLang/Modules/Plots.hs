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
      [ histogram  , histogramExplicit
      , linegraph  , linegraphExplicit
      , scatterplot, scatterplotExplicit
      , venndiagram, venndiagramExplicit
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

----------------------
-- plot a histogram --
----------------------

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

---------------
-- linegraph --
---------------

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

-----------------
-- scatterplot --
-----------------

-- TODO any reason this wouldn't work with other things besides the .scores files?
scatterplotExplicit :: Function
scatterplotExplicit = newFnS3
  "scatterplot_explicit"
  (Exactly str, Exactly str, ScoresSigs (Exactly num))
  (Exactly png)
  "scatterplot.R"
  [Hidden]
  id

-- | User-facing version that auto-loads the script and captures any varnames in the untyped list.
scatterplot :: Function
scatterplot = newExprExpansion
  "scatterplot"
  [Exactly str, ScoresSigs (Exactly num)]
  (Exactly png)
  mScatterplot
  []

-- | Macro that adds the xlab str
mScatterplot :: ExprExpansion
mScatterplot _ scr (Fun r ms ds _ [title, ns]) =
  let xlab = Lit str $ varName "" ns
  in Fun r ms ds "scatterplot_explicit" [title, xlab, ns]
mScatterplot _ _ e = error "modules.plots.mScatterplot" $ "bad argument: " ++ show e

-----------------
-- venndiagram --
-----------------

-- TODO any reason this wouldn't work with other things besides the .scores files?
venndiagramExplicit :: Function
venndiagramExplicit = newFnS3
  "venndiagram_explicit"
  (Exactly str, Exactly str, ListSigs (ListSigs (AnyType "the type to count unique values of")))
  (Exactly png)
  "venndiagram.R"
  [Hidden]
  id

-- | User-facing version that auto-loads the script and captures any varnames in the untyped list.
venndiagram :: Function
venndiagram = newExprExpansion
  "venndiagram"
  [Exactly str, ListSigs (ListSigs (AnyType "the type to count unique values of"))]
  (Exactly png)
  mVenndiagram
  []

-- | Macro that adds the xlab str
mVenndiagram :: ExprExpansion
mVenndiagram mods scr (Fun r ms ds n [title, (Ref _ _ _ (Var _ name))]) = case lookupExpr name (sAssigns scr) of
  Nothing -> error "modules.plots.mVenndiagram" $ "no such var: " ++ name
  Just e -> mVenndiagram mods scr (Fun r ms ds n [title, e]) -- TODO is this the right way to handle it?
mVenndiagram _ scr (Fun r ms ds _ [title, e@(Lst _ _ _ es)]) =
  let names = listVarNames "list" scr es
  in Fun r ms ds "venndiagram_explicit" [title, names, e]
mVenndiagram _ _ e = error "modules.plots.mVenndiagram" $ "bad argument: " ++ show e

-------------------
-- plot X.scores --
-------------------

bargraph :: Function
bargraph = undefined

---------------
-- old stuff --
---------------

-- TODO remove these after rewriting SetsTable:

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


