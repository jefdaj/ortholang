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
import Data.GraphViz
import Data.Graph.Inductive hiding (nodes, edges)
import Control.Monad (forM_)
import Data.GraphViz.Attributes.Complete (ColorList, Color(..), toColorList)
import Data.Maybe (fromJust)
import System.FilePath (combine)

doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = forM_ cases createImage

-- TODO different dir, obviously
createImage :: PrintDotRepr dg n => (FilePath, dg n) -> IO FilePath
createImage (n, g) = createImageInDir "/tmp" n Png g

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)

olModule :: Module
olModule = undefined

----------------------
-- from text script --
----------------------

type MockVar = (String, [String])

type MockScript = ([MockVar], String)

olColors :: [(String, ColorList)]
olColors =
 [ ("pink" , c 253 202 255)
 , ("blue" , c 197 255 255)
 , ("green", c 217 255 173)
 ]
 where c r g b = toColorList [RGB r g b]

mkVarGraph :: MockScript -> Gr String String
mkVarGraph (vs, r) = mkGraph nodes edges
  where
    rv = ("result", [r])
    vs' = vs ++ [rv]
    (nodes, nodemap) = mkNodes new $ Prelude.map fst vs'
    edges = fromJust $ mkEdges nodemap
          $ Prelude.concatMap (\(v, ds) -> Prelude.map (\d -> (d, v, "")) ds) vs'

mockScript :: MockScript
mockScript = (vs, "three")
  where
    vs =
      [ ("one"  , [])
      , ("two"  , ["one"])
      , ("three", ["one", "two"])
      , ("four" , ["two", "three"])
      ]

mockNodes :: ([LNode String], NodeMap String)
mockNodes = mkNodes new $ Prelude.map fst $ fst mockScript

mockGraph :: Gr String String
mockGraph = mkVarGraph mockScript
