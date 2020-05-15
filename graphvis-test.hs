{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Graph.Inductive
-- import           Data.Graph.Inductive.Example
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import qualified Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import           Data.Word
import           WriteRunDot -- also a local test file
import Data.List (sort, nub)
import Data.Maybe (fromJust)
-- import qualified Data.Sequence as DS


-----------------------
-- blog post example --
-----------------------

ex1 :: Gr Text Text
ex1 = mkGraph

        [ (1,"one")
        , (2,"three")
        , (3,"three")
        , (4,"three")
        , (5,"three")
        , (6,"three")
        ]

        [ (1,3,"")
        , (2,3,"")
        ]

ex1Params :: GraphvizParams n L.Text L.Text () L.Text
ex1Params = nonClusteredParams {globalAttributes = ga, fmtNode = fn, fmtEdge = fe}
  where fn (_,l)   = [textLabel l]
        fe (_,_,l) = [textLabel l]
        ga = [ GraphAttrs
                 [ RankDir FromTop
                 , BgColor [toWColor White]
                 ]
             , NodeAttrs
                [ shape     Ellipse
                , FillColor (myColorCL 1)
                , style     filled
                ]
             ]

myColorCL :: Word8 -> ColorList
myColorCL n | n == 1 = c $ (RGB 229 255 204)
            | n == 2 = c $ (RGB 175 177 112)
            | n == 3 = c $ (RGB 226 206 179)
            | n == 4 = c $ (RGB 172 126 100)
            | otherwise = c $ (RGB 255 255 255)
 where c rgb = toColorList [rgb]

myColor :: Word8 -> Attribute
myColor n = Color $ myColorCL n


-----------------------
-- from simple lists --
-----------------------

type MockVar = (Text, [Text])

type MockScript = [MockVar]

-- mkNodes2 :: MockScript -> [(Int, Text)]
-- mkNodes2 vs = Prelude.zip [1..] $ Prelude.map fst vs

mockScript :: MockScript
mockScript =
  [ ("one"  , [])
  , ("two"  , ["one"])
  , ("three", ["one", "two"])
  , ("four" , ["two", "three"])
  ]

mockNodes :: ([LNode Text], NodeMap Text)
mockNodes = mkNodes new $ Prelude.map fst mockScript

mkVarGraph :: MockScript -> Gr Text Text
mkVarGraph scr = mkGraph nodes edges
  where
    (nodes, nodemap) = mkNodes new $ Prelude.map fst scr
    edges = fromJust $ mkEdges nodemap $ Prelude.concatMap (\(v, ds) -> Prelude.map (\d -> (d, v, "")) ds) scr

mockGraph :: Gr Text Text
mockGraph = mkVarGraph mockScript

-- mkEdges :: (Text, [Text]) -> [(Int, Int)]
-- mkEdges (var, deps) = Prelude.map (\d -> (d, var)) deps

mkVarGraph2 = DotGraph
  { strictGraph = False
  , directedGraph = True
  , graphID = Just (Str "G")
  , graphStatements = undefined -- TODO wtf how do you make these
  }


----------
-- main --
----------

main :: IO ()
main = do
    doDots [ ("ex1" , graphToDot ex1Params ex1) ]
    doDots [ ("mock" , graphToDot ex1Params mockGraph) ]
    -- doDots [ ("ex2" , ex2)
    --        , ("ex3" , ex3)
    --        , ("ex4" , ex4)
    --        ]
