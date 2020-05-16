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
import OrthoLang.Interpreter
import OrthoLang.Debug (error)
import Prelude hiding (error)
import Data.GraphViz
import Data.Graph.Inductive hiding (nodes, edges)
import Data.GraphViz.Attributes.Complete -- (ColorList, Color(..), toColorList)
import Data.Maybe (fromJust)
import System.FilePath (combine)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)

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
    vn (Assign (Var _ v) _) = v -- TODO move to Types?
    as' = sAssigns scr ++ case sResult scr of
                            Nothing -> []
                            Just e -> [Assign resultVar e]
    (nodes, nodemap) = mkNodes new $ map vn as'
    edges = fromJust $ mkEdges nodemap
          $ concatMap (\(Assign (Var _ v) e) ->
                          map (\(Var _ d) -> (d, v, "")) (depsOf e)) as'
