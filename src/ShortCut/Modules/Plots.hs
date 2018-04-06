module ShortCut.Modules.Plots where

{- Finally, ShortCut gets plotting!
 - I think this will go a long way toward convincing people it's useful.
 - The plot type is just an image for now.
 - TODO should "showing" a plot mean opening it in an image viewer? not yet
 - TODO how to name the axes?
 -}

import ShortCut.Core.Types

cutModule :: CutModule
cutModule = CutModule
  { mName = "plots"
  , mFunctions =
    [ histogram
    , linegraph
    , bargraph
    , scatterplot
    , venndiagram
    ]
  }

plot :: CutType
plot = CutType
  { tExt  = "png"
  , tDesc = "png image of a plot"
  , tShow = \_ f -> return $ "png image '" ++ f ++ "'"
  }

histogram = undefined
linegraph = undefined
bargraph = undefined
scatterplot = undefined
venndiagram = undefined
