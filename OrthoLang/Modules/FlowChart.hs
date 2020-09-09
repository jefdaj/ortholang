{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Generates dependency graphs using Graphviz. They are useful for debugging the
interpreter, as well as for tutorials and documentation. This is a work in
progress, and may fail in interesting or surprising ways when given edge cases!
Notable repeat/replace and lists of results are not handled properly yet.
-}

-- Partially based on this tutorial:
-- http://haroldcarr.org/posts/2014-02-28-using-graphviz-via-haskell.html

-- TODO add a few tests
-- TODO add graph label attributes? https://stackoverflow.com/a/6452088/429898
-- TODO how to handle complicated lists like ortholog_in_max.ol uses?
--      oh, i get it: have to add more nodes + edges per Expr when they're nested, not just one per assign
--      and that's important for having the initial inputs show up too

module OrthoLang.Modules.FlowChart where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Script (rDepsOf)
import OrthoLang.Interpreter.Paths (prefixOf)
import OrthoLang.Interpreter
import OrthoLang.Util (digest, justOrDie)
import OrthoLang.Debug (error, trace)
import Prelude hiding (error)
import Data.GraphViz
import Data.Graph.Inductive hiding (nodes, edges)
import qualified Data.Graph.Inductive.Graph as G
import Data.GraphViz.Attributes.Complete
import Data.Maybe (fromJust)
import System.Directory (renameFile)
import System.FilePath (combine)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Data.List (sort, nub, isSuffixOf)
import OrthoLang.Modules.Plots (png)

olModule :: Module
olModule = Module
  { mName = "FlowChart"
  , mDesc = "Draw flowchart representing OrthoLang scripts"
  , mTypes = [png]
  , mGroups = []
  , mEncodings = []
  , mRules = return ()
  , mFunctions = [plotDot, plotVars, plotDepends, plotRDepends]
  }


--------------
-- plot_dot --
--------------

-- | Hidden function for rendering the raw Haskell Graphviz data structure passed as a string
plotDot :: Function
plotDot = hidden $ newFnA1
  "plot_dot"
  (Exactly str)
  (Exactly png) -- TODO svg?
  aPlotDot
  [Hidden]

aPlotDot :: NewAction1
aPlotDot (ExprPath out) inDot = do
  let loc = "ortholang.modules.flowchart.aPlotDot"
  txt <- readLit loc inDot
  cfg <- fmap fromJust $ getShakeExtra
  let g = read txt :: DotGraph Node
      out' = toPath loc cfg out
  withBinHash out out' $ \tmpPath -> do
    let tmpPath' = fromPath loc cfg tmpPath
    renderPng tmpPath' g

-- TODO how does the sizing work if we make it svg instead?
renderPng :: PrintDotRepr dg n => FilePath -> dg n -> Action ()
renderPng path g = do
  -- graphviz adds its own .png extension here, so we have to move the file afterward
  tmp <- liftIO $ Data.GraphViz.addExtension (runGraphvizCommand Dot g) Png path
  liftIO $ renameFile tmp path
  trackWrite' [path]


-----------------
-- plot_vars --
-----------------

-- TODO do use the result var if possible! just have a special constructor for it and render a different color
--      might need to use a special _inputs naming scheme. could be simple: result_inputs. lol
plotVars :: Function
plotVars = newExprExpansion
  "plot_vars"
  [Exactly str]
  (Exactly png)
  (mkFlowChartMacro selectAll)
  [ReadsScript]

selectAll :: [Module] -> Script -> [Expr] -> [Var]
selectAll _ scr _ = map aVar $ sAssigns scr

-- | This inserts a plot_dot call with the complete dot structure in its str input.
-- TODO implement the other two by applying a function to the script first?
mkFlowChartMacro :: ([Module] -> Script -> [Expr] -> [Var]) -> ExprExpansion
mkFlowChartMacro selectFn mods scr (Fun t ms vs n ((Lit str title):es)) = Fun t ms vs "plot_dot" [ds]
  where
    vs = selectFn mods scr es
    dg = dotGraph title $ filter (\a -> aVar a `elem` vs) $ sAssigns scr
    ds = Lit str (show dg)
mkFlowChartMacro _ _ _ e = error "ortholang.modules.flowchart.mkFlowChartMacro" $ "bad expr arg: " ++ show e


------------------
-- plot_depends --
------------------

plotDepends :: Function
plotDepends = newExprExpansion
  "plot_depends"
  [Exactly str, AnyType "type of the expr whose depends will be plotted"]
  (Exactly png)
  (mkFlowChartMacro selectDepends)
  [ReadsScript]

  -- Ref Type (Maybe Seed) [Var] Var -- do refs need a seed? yes! (i think?)
selectDepends :: [Module] -> Script -> [Expr] -> [Var]
selectDepends _ _ [expr] = depsOf expr
selectDepends _ _ es = error "ortholang.modules.flowchart.selectDepends" $ "bad exprs: " ++ show es


-------------------
-- plot_rdepends --
-------------------

plotRDepends :: Function
plotRDepends = newExprExpansion
  "plot_rdepends"
  [Exactly str, AnyType "type of the expr whose reverse depends will be plotted"]
  (Exactly png)
  (mkFlowChartMacro selectRDepends)
  [ReadsScript]

  -- Ref Type (Maybe Seed) [Var] Var -- do refs need a seed? yes! (i think?)
selectRDepends :: [Module] -> Script -> [Expr] -> [Var]
selectRDepends _ scr [Ref _ _ _ v] = v : rDepsOf scr v
selectRDepends _ _ es = error "ortholang.modules.flowchart.selectRDepends" $ "bad exprs: " ++ show es


---------------------------
-- create dot from graph --
---------------------------

-- TODO remove?
olColors :: [(String, ColorList)]
olColors =
 [ ("pink" , c 253 202 255)
 , ("blue" , c 197 255 255)
 , ("blue2", c 77 210 255)
 , ("green", c 217 255 173)
 , ("green2", c 102 255 51)
 ]
 where
   c r g b = toColorList [RGB r g b]

-- see here for how to use labels:
-- http://www.michaelburge.us/2017/09/01/how-to-use-graphviz-in-haskell.html

-- TODO remove strings here if they're actually passed somewhere else?
data NLabel
  = NLVar String        -- ^ string is the varname
  | NLTmp String String -- ^ strings are name, expr digest
  deriving (Read, Show, Eq, Ord)

data ELabel
  = ELTail -- ^ one of multiple arrows from input -> tmpnode
  | ELHead -- ^ arrow from tmpnode -> output
  | ELArrow String -- ^ the only function input; string is the fn name
  deriving (Read, Show, Eq, Ord)

params :: String -> GraphvizParams n NLabel ELabel () NLabel
params title = nonClusteredParams {globalAttributes = ga, fmtNode = fn, fmtEdge = fe}
  where
    fn (_,NLVar l  ) = [ textLabel $ T.pack l
                       , shape Ellipse
                       , style filled
                       , FillColor (fromJust $ Prelude.lookup "blue2" olColors)
                       ]

    fn (_,NLTmp l _) = [ textLabel $ T.pack l
                       , Shape PlainText
                       , FillColor (toColorList [RGB 255 255 255])
                       , Width  0.01
                       , Height 0.01
                       ]

    fe (_,_,ELArrow l) = [textLabel $ T.pack (" " ++ l ++ " ")]
    fe (_,_,ELTail   ) = [Dir NoDir]
    fe (_,_,ELHead   ) = []

    ga = [ GraphAttrs
             [ RankDir FromTop
             , BgColor [toWColor White]
             , Label (StrLabel (T.pack $ title ++ "\n\n"))
             , LabelLoc VTop
             , LabelDistance 5.0
             -- TODO scale it by the number of nodes?
             -- max size in pixels is (DPI * first number) x (DPI * second number):
             , DPI 300
             , Size (GSize 6.0 (Just 8.0) False)
             ]
         ]

{-|
Reads the script (only up to the point where the graph fn was called) and
generates a Haskell DotGraph data structure. It could be used directly with
renderPng, but instead it will be  and passed to the
graphing function via an OrthoLang string. Which is kind of roundabout but
seems to work.
-}
dotGraph :: String -> [Assign] -> DotGraph Node
dotGraph title assigns = graphToDot (params title) (gr :: Gr NLabel ELabel)
  where
    assigns' = filter keepAssign assigns
    gr = mkGraph nodes edges
    (nodes, nodemap) = mkNodes' assigns'
    edges = mkEdges' nodemap assigns'


------------------------------
-- create graph from script --
------------------------------

{-|
Like depsOf, but customized for pretty graph output. Differences:

* does not include indirect dependencies
-}
inputNodes :: String -> Expr -> [String]
inputNodes tmp e = inputs ++ if length inputs < 2 then [] else [tmp]
  where
    inputs = map (\(Var _ n) -> n) $ inputVars e -- TODO have to handle repeats here?

inputVars :: Expr -> [Var]
inputVars (Lit _ _           ) = []
inputVars (Ref _ _ vs v      ) = [v]
inputVars (Bop _ _ vs _ e1 e2) = nub $ concatMap inputVars [e1, e2] ++ concatMap varOf [e1, e2]
inputVars (Fun _ _ vs _ es   ) = nub $ concatMap inputVars es ++ concatMap varOf es
inputVars (Lst _ _ vs   es   ) = nub $ concatMap inputVars es ++ concatMap varOf es

-- merges input edges, as explained here:
-- https://mike42.me/blog/2015-02-how-to-merge-edges-in-graphviz

-- TODO this needs to add tmpNodes, but ideally not extra ones. how to tell?
mkNodes' :: [Assign] -> ([LNode NLabel], NodeMap NLabel)
mkNodes' assigns = mkNodes new nodes'
  where
    loc = "ortholang.modules.flowchart.mkNodes'"
    selected = map aVar assigns
    varNodes = concatMap (\(Assign (Var _ v) _) -> [NLVar v]) assigns
    tmpNodes = concatMap (\(Assign (Var _ v) e) ->
                            let inputs = filter (`elem` selected) $ inputVars e
                            in if length inputs < 2
                              then []
                              else [NLTmp (prefixOf e) (digest loc e)])
                         assigns
    nodes = varNodes ++ tmpNodes
    nodes' = trace "ortholang.modules.flowchart.mkNodes'" ("nodes: " ++ show nodes) nodes

varNamesToIgnore :: [String]
varNamesToIgnore = ["result"]

-- specifically, edge labels
-- TODO only remove the current plot fn while leaving any others?
--      don't bother just for this, but subgraphs might be required for repeat + replace too
fnNamesToIgnore :: [String]
fnNamesToIgnore = ["plot_vars", "plot_dot", "plot_depends", "plot_rdepends"]

-- TODO how would you go about also adding indirect inputs here? probably need a fn that works on only exprs
mkInputEdges :: [String] -> Assign -> [(NLabel, NLabel, ELabel)]
mkInputEdges selected (Assign (Var _ v) e) = directInputs
  where
    directInputs = if length inputs < 2 then edgesLabeled else edgesMerged
    loc = "ortholang.modules.flowchart.mkInputEdges"
    tmpNode      = NLTmp (prefixOf e) (digest loc e)
    inputs       = filter (\i -> i `elem` selected) $ filter (/= (digest loc e)) $ inputNodes (digest loc e) e
    edgesLabeled = map (\i -> (NLVar i, NLVar v, ELArrow (prefixOf e))) inputs
    edgesMerged  = map (\i -> (NLVar i, tmpNode, ELTail)) inputs ++ [(tmpNode, NLVar v, ELHead)]

keepAssign :: Assign -> Bool
keepAssign (Assign (Var _ v) e) = not (v `elem` varNamesToIgnore)
                               && not (prefixOf e `elem` fnNamesToIgnore)

mkEdges' :: NodeMap NLabel -> [Assign] -> [LEdge ELabel]
mkEdges' nodemap assigns = justOrDie "mkEdges'" $ mkEdges nodemap edges'
  where
    loc = "ortholang.modules.flowchart.mkEdges'"
    as' = trace loc ("assigns: " ++ show assigns) assigns
    selectedNames = map (\(Assign (Var _ n) _) -> n) assigns
    edges = concatMap (mkInputEdges selectedNames) as'
    edges' = trace loc ("edges: " ++ show edges) edges
