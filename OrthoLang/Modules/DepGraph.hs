{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Generates dependency graphs using Graphviz. They are useful for debugging the
interpreter, as well as for tutorials and documentation.

Partially based on this tutorial:

<http://haroldcarr.org/posts/2014-02-28-using-graphviz-via-haskell.html>
-}

-- TODO add graph label attributes: https://stackoverflow.com/a/6452088/429898

module OrthoLang.Modules.DepGraph where

import OrthoLang.Types
import OrthoLang.Interpreter.Paths (prefixOf)
import OrthoLang.Interpreter
import OrthoLang.Util (digest, justOrDie)
import OrthoLang.Debug (error, trace)
import Prelude hiding (error)
import Data.GraphViz
import Data.Graph.Inductive hiding (nodes, edges)
import qualified Data.Graph.Inductive.Graph as G
import Data.GraphViz.Attributes.Complete -- (ColorList, Color(..), toColorList)
import Data.Maybe (fromJust)
import System.FilePath (combine)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Data.List (sort, nub)

import OrthoLang.Modules.Plots (png) -- TODO rename?

olModule :: Module
olModule = Module
  { mName = "DepGraph"
  , mDesc = "Graphs dependencies between OrthoLang variables"
  , mTypes = [png]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [plotDot, plotScript] -- TODO plot_depends, plot_rdepends (with common parts factored out)
  }


--------------
-- plot_dot --
--------------

-- TODO also take a title
plotDot :: Function
plotDot = hidden $ newFnA1
  "plot_dot"
  (Exactly str) -- ^ shown dotgraph
  (Exactly png) -- ^ graph
  aPlotDot
  [Hidden]

-- TODO withBinHash, right?
aPlotDot :: NewAction1
aPlotDot (ExprPath out) inDot = do
  let loc = "ortholang.modules.depgraph.aPlotDot"
  txt <- readLit loc inDot
  let g = read txt :: DotGraph Node
  liftIO $ renderDotGraph out g
  trackWrite' [out]
  return ()

-- TODO why return the filepath?
renderDotGraph :: PrintDotRepr dg n => FilePath -> dg n -> IO FilePath
renderDotGraph path g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) Png path


-----------------
-- plot_script --
-----------------

-- TODO make the title work
plotScript :: Function
plotScript = newMacro
  "plot_script"
  [Exactly str] -- ^ title
  (Exactly png) -- ^ graph
  mPlotScript
  [ReadsScript]

-- | This inserts a plot_dot call with the complete dot structure in its str input.
mPlotScript :: MacroExpansion
mPlotScript scr (Fun t ms vs n _) | n == "plot_script" = Fun t ms vs "plot_dot" [ds]
  where
    dg = dotGraph scr
    ds = Lit str (show dg)
mPlotScript _ e = error "ortholang.modules.depgraph.mPlotScript" $ "bad expr arg: " ++ show e


----------------------------
-- create dot from script --
----------------------------

olColors :: [(String, ColorList)]
olColors =
 [ ("pink" , c 253 202 255)
 , ("blue" , c 197 255 255)
 , ("green", c 217 255 173)
 ]
 where
   c r g b = toColorList [RGB r g b]

-- TODO customize these
ex1Params :: GraphvizParams n String String () String
ex1Params = nonClusteredParams {globalAttributes = ga, fmtNode = fn, fmtEdge = fe}
  where fn (_,l)   = [textLabel $ T.pack l]
        fe (_,_,l) = [textLabel $ T.pack l]
        ga = [ GraphAttrs
                 [ RankDir FromTop
                 , BgColor [toWColor White]
                 ]
             , NodeAttrs
                [ shape     Ellipse
                , FillColor (fromJust $ Prelude.lookup "blue" olColors)
                , style     filled
                ]
             ]

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
inputVars (Com (CompiledExpr _ _ _)) = [] -- TODO should this be an error instead? their deps are accounted for

-- merges input edges, as explained here:
-- https://mike42.me/blog/2015-02-how-to-merge-edges-in-graphviz

-- TODO this needs to add tmpNodes, but ideally not extra ones. how to tell?
mkNodes' :: Script -> ([LNode String], NodeMap String)
mkNodes' scr = mkNodes new nodes''
  where
    vn (Assign (Var _ v) _) = v -- TODO move to Types?
    nodes = map vn $ filter keepAssign $ sAssigns scr -- TODO aha! this probably needs to include the tmpNodes
    tmpNodes = concatMap (\(Assign _ e) -> if length (inputVars e) > 1 then [digest e ++ "_inputs"] else []) $ sAssigns scr
    nodes' = filter (\n -> not $ n `elem` varNamesToIgnore) (nodes ++ tmpNodes)
    nodes'' = trace "ortholang.modules.depgraph.mkNodes'" ("nodes': " ++ show nodes') nodes'

-- TODO explain that result will be removed in the notebook
varNamesToIgnore :: [String]
varNamesToIgnore = ["result"]

-- specifically, edge labels
-- TODO only remove the current plot fn while leaving any others?
--      don't bother just for this, but subgraphs might be required for repeat + replace too
fnNamesToIgnore :: [String]
fnNamesToIgnore = ["plot_script", "plot_dot", "plot_depends", "plot_rdepends"]

mkInputEdges :: Assign -> [(String, String, String)]
mkInputEdges (Assign (Var _ v) e) = if length inputs < 2 then edgesLabeled else edgesMerged
  where
    tmpNode      = digest e ++ "_inputs"
    inputs       = filter (/= tmpNode) $ inputNodes tmpNode e
    edgesLabeled = map (\i -> (i, v, prefixOf e)) inputs
    edgesMerged  = map (\i -> (i, tmpNode, "")) inputs ++ [(tmpNode, v, prefixOf e)]

keepAssign :: Assign -> Bool
keepAssign (Assign (Var _ v) e) = not (v `elem` varNamesToIgnore)
                               && not (prefixOf e `elem` fnNamesToIgnore)

mkEdges' :: NodeMap String -> Script -> [LEdge String]
mkEdges' nodemap scr = justOrDie "mkEdges'" $ mkEdges nodemap edges'
  where
    loc = "ortholang.modules.depgraph.mkEdges'"
    as' = filter keepAssign $ sAssigns scr
    as'' = trace loc ("as': " ++ show as') as'
    edges = concatMap mkInputEdges as''
    edges' = trace loc ("edges: " ++ show edges) edges

{-|
Reads the script (only up to the point where the graph fn was called) and
generates a Haskell DotGraph data structure. It could be used directly with
renderDotGraph, but instead it will be  and passed to the
graphing function via an OrthoLang string. Which is kind of roundabout but
seems to work.
-}
dotGraph :: Script -> DotGraph Node
dotGraph scr = graphToDot ex1Params (gr :: Gr String String)
  where
    gr = mkGraph nodes edges
    (nodes, nodemap) = mkNodes' scr -- TODO aha! this probably needs to include the tmpnodes
    edges = mkEdges' nodemap scr
