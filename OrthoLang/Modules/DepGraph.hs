{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Generates dependency graphs using Graphviz. They are useful for debugging the
interpreter, as well as for tutorials and documentation. This is a work in
progress, and may fail in interesting or surprising ways when given edge cases!
Notable repeat/replace and lists of results are not handled properly yet.
-}

-- TODO get bin hash working
-- TODO see if bin hash was all that it needed to get files sorted out
-- TODO add plot_depends
-- TODO add plot_rdepends

-- Partially based on this tutorial:
-- http://haroldcarr.org/posts/2014-02-28-using-graphviz-via-haskell.html

-- TODO add graph label attributes? https://stackoverflow.com/a/6452088/429898
-- TODO how to handle complicated lists like ortholog_in_max.ol uses?
--      oh, i get it: have to add more nodes + edges per Expr when they're nested, not just one per assign
--      and that's important for having the initial inputs show up too

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
import Data.GraphViz.Attributes.Complete
import Data.Maybe (fromJust)
import System.FilePath (combine)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)
import Data.List (sort, nub, isSuffixOf)
import OrthoLang.Modules.Plots (png)

olModule :: Module
olModule = Module
  { mName = "DepGraph"
  , mDesc = "Graphs dependencies between OrthoLang variables"
  , mTypes = [png]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [plotDot, plotScript, plotDepends] -- TODO plot_rdepends (with common parts factored out)
  }


--------------
-- plot_dot --
--------------

-- | Hidden function for rendering the raw Haskell Graphviz data structure passed as a string
-- TODO also take a title?
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
  (mkDepGraphMacro selectAll)
  [ReadsScript]

selectAll :: Script -> [Expr] -> [Var]
selectAll scr _ = map aVar $ sAssigns scr

-- | This inserts a plot_dot call with the complete dot structure in its str input.
-- TODO implement the other two by applying a function to the script first?
mkDepGraphMacro :: (Script -> [Expr] -> [Var]) -> MacroExpansion
mkDepGraphMacro selectFn scr (Fun t ms vs n (_:es)) = Fun t ms vs "plot_dot" [ds] -- TODO use the title arg
  where
    vs = selectFn scr es
    dg = dotGraph $ filter (\a -> aVar a `elem` vs) $ sAssigns scr
    ds = Lit str (show dg)
mkDepGraphMacro _ _ e = error "ortholang.modules.depgraph.mkDepGraphMacro" $ "bad expr arg: " ++ show e


------------------
-- plot_depends --
------------------

-- TODO make the title work
plotDepends :: Function
plotDepends = newMacro
  "plot_depends"
  [Exactly str, AnyType "type of the expr whose depends will be plotted"]
  (Exactly png) -- ^ graph
  (mkDepGraphMacro selectDepends)
  [ReadsScript]

  -- Ref Type (Maybe Seed) [Var] Var -- do refs need a seed? yes! (i think?)
selectDepends :: Script -> [Expr] -> [Var]
selectDepends _ [expr] = depsOf expr
selectDepends _ es = error "ortholang.modules.depgraph.selectDepends" $ "bad exprs: " ++ show es


---------------------------
-- create dot from graph --
---------------------------

-- TODO remove?
olColors :: [(String, ColorList)]
olColors =
 [ ("pink" , c 253 202 255)
 , ("blue" , c 197 255 255)
 , ("green", c 217 255 173)
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

params :: GraphvizParams n NLabel ELabel () NLabel
params = nonClusteredParams {globalAttributes = ga, fmtNode = fn, fmtEdge = fe}
  where
    fn (_,NLVar l  ) = [textLabel $ T.pack l]
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
             ]
         , NodeAttrs
            [ shape     Ellipse
            , FillColor (fromJust $ Prelude.lookup "blue" olColors)
            , style     filled
            ]
         ]

{-|
Reads the script (only up to the point where the graph fn was called) and
generates a Haskell DotGraph data structure. It could be used directly with
renderDotGraph, but instead it will be  and passed to the
graphing function via an OrthoLang string. Which is kind of roundabout but
seems to work.
-}
dotGraph :: [Assign] -> DotGraph Node
dotGraph assigns = graphToDot params (gr :: Gr NLabel ELabel)
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
inputVars (Com (CompiledExpr _ _ _)) = [] -- TODO should this be an error instead? their deps are accounted for

-- merges input edges, as explained here:
-- https://mike42.me/blog/2015-02-how-to-merge-edges-in-graphviz

-- allAssigns :: Script -> [Assign]
-- allAssigns scr = filter keepAssign $ sAssigns scr ++ case sResult scr of
  -- Nothing -> []
  -- Just e  -> [Assign resultVar e]

-- TODO this needs to add tmpNodes, but ideally not extra ones. how to tell?
mkNodes' :: [Assign] -> ([LNode NLabel], NodeMap NLabel)
mkNodes' assigns = mkNodes new nodes'
  where
    -- as' = sAssigns scr ++ [Assign resultVar (sResult scr)]
    -- assigns' = filter keepAssign assigns
    varNodes = concatMap (\(Assign (Var _ v) _) -> [NLVar v]) assigns
    tmpNodes = concatMap (\(Assign (Var _ v) e) ->
                            if length (inputVars e) < 2 then [] else [NLTmp (prefixOf e) (digest e)])
                         assigns
    nodes = varNodes ++ tmpNodes
    nodes' = trace "ortholang.modules.depgraph.mkNodes'" ("nodes: " ++ show nodes) nodes

-- TODO explain that result will be removed in the notebook
varNamesToIgnore :: [String]
varNamesToIgnore = ["result"]

-- specifically, edge labels
-- TODO only remove the current plot fn while leaving any others?
--      don't bother just for this, but subgraphs might be required for repeat + replace too
fnNamesToIgnore :: [String]
fnNamesToIgnore = ["plot_script", "plot_dot", "plot_depends", "plot_rdepends"]

-- TODO how would you go about also adding indirect inputs here? probably need a fn that works on only exprs
mkInputEdges :: Assign -> [(NLabel, NLabel, ELabel)]
mkInputEdges (Assign (Var _ v) e) = directInputs
  where
    directInputs = if length inputs < 2 then edgesLabeled else edgesMerged
    tmpNode      = NLTmp (prefixOf e) (digest e)
    inputs       = filter (/= (digest e)) $ inputNodes (digest e) e
    edgesLabeled = map (\i -> (NLVar i, NLVar v, ELArrow (prefixOf e))) inputs
    edgesMerged  = map (\i -> (NLVar i, tmpNode, ELTail)) inputs ++ [(tmpNode, NLVar v, ELHead)]

keepAssign :: Assign -> Bool
keepAssign (Assign (Var _ v) e) = not (v `elem` varNamesToIgnore)
                               && not (prefixOf e `elem` fnNamesToIgnore)

mkEdges' :: NodeMap NLabel -> [Assign] -> [LEdge ELabel]
mkEdges' nodemap assigns = justOrDie "mkEdges'" $ mkEdges nodemap edges'
  where
    loc = "ortholang.modules.depgraph.mkEdges'"
    -- as' = allAssigns scr
    as' = trace loc ("assigns: " ++ show assigns) assigns
    edges = concatMap mkInputEdges as'
    edges' = trace loc ("edges: " ++ show edges) edges
