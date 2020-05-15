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

import           Control.Monad   (forM_)
import           Data.GraphViz
import           System.FilePath
import           Data.Graph.Inductive
import           Data.Graph.Inductive.Example
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import           Data.Word

doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = forM_ cases createImage

-- TODO different dir, obviously
createImage :: PrintDotRepr dg n => (FilePath, dg n) -> IO FilePath
createImage (n, g) = createImageInDir "/tmp" n Png g

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)


olModule :: Module
olModule = undefined
