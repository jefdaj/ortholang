{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Generates dependency graphs using Graphviz. They are useful for debugging the
interpreter, as well as for tutorials and documentation.

Partially based on this tutorial:

<http://haroldcarr.org/posts/2014-02-28-using-graphviz-via-haskell.html>
-}

module OrthoLang.Modules.DepGraph where

import OrthoLang.Types
import OrthoLang.Interpreter
import Data.GraphViz
import Data.Graph.Inductive hiding (nodes, edges)
import Data.GraphViz.Attributes.Complete (ColorList, Color(..), toColorList)
import Data.Maybe (fromJust)
import System.FilePath (combine)

import OrthoLang.Modules.Plots (png) -- TODO rename?

olModule :: Module
olModule = Module
  { mName = "DepGraph"
  , mDesc = "Graphs dependencies between OrthoLang variables"
  , mTypes = [png]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [graphScript] -- TODO graph_depends, graph_rdepends (with common parts factored out)
  }

graphScript :: Function
graphScript = newMacro
  "graph_script" -- TODO plot_script?
  [Exactly str] -- ^ title
  (Exactly png) -- ^ graph
  mDepgraphScript
  [ReadsScript]

-- TODO figure out how to add a title to the graph
mDepgraphScript :: MacroExpansion
mDepgraphScript scr expr = undefined

-- TODO different dir, obviously
-- createImage :: PrintDotRepr dg n => (FilePath, dg n) -> IO FilePath
-- createImage (n, g) = createImageInDir "/tmp" n Png g

-- createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
-- createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)

-- TODO does it also need to return a path? want to use bin cache
renderGraph :: PrintDotRepr dg n => FilePath -> GraphvizOutput -> dg n -> IO ()
renderGraph = undefined

olColors :: [(String, ColorList)]
olColors =
 [ ("pink" , c 253 202 255)
 , ("blue" , c 197 255 255)
 , ("green", c 217 255 173)
 ]
 where
   c r g b = toColorList [RGB r g b]

dotGraph :: Script -> Gr String String
dotGraph scr = mkGraph nodes edges
  where
    vn (Assign (Var _ v) _) = v -- TODO move to Types?
    as' = sAssigns scr ++ case sResult scr of
                            Nothing -> []
                            Just e -> [Assign resultVar e]
    (nodes, nodemap) = mkNodes new $ map vn as'
    edges = fromJust $ mkEdges nodemap
          $ concatMap (\(Assign (Var _ v) e) -> map (\(Var _ d) -> (d, v, "")) (depsOf e)) as'
