{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Graph.Inductive
import           Data.Graph.Inductive.Example
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import           Data.Word
import           WriteRunDot -- also a local test file

ex1 :: Gr Text Text
ex1 = mkGraph [ (1,"one")
              , (3,"three")
              ]
              [ (1,3,"edge label") ]

ex1Params :: GraphvizParams n L.Text L.Text () L.Text
ex1Params = nonClusteredParams { globalAttributes = ga
                               , fmtNode          = fn
                               , fmtEdge          = fe
                               }
  where fn (_,l)   = [textLabel l]
        fe (_,_,l) = [textLabel l]

        ga = [ GraphAttrs [ RankDir   FromLeft
                          , BgColor   [toWColor White]
                          ]
             , NodeAttrs  [ shape     BoxShape
                          , FillColor (myColorCL 2)
                          , style     filled
                          ]
             ]

myColorCL :: Word8 -> ColorList
myColorCL n | n == 1 = c $ (RGB 127 108 138)
            | n == 2 = c $ (RGB 175 177 112)
            | n == 3 = c $ (RGB 226 206 179)
            | n == 4 = c $ (RGB 172 126 100)
 where c rgb = toColorList [rgb]

myColor :: Word8 -> Attribute
myColor n = Color $ myColorCL n

main :: IO ()
main = do
    doDots [ ("ex1" , graphToDot ex1Params ex1) ]
    -- doDots [ ("ex2" , ex2)
    --        , ("ex3" , ex3)
    --        , ("ex4" , ex4)
    --        ]
