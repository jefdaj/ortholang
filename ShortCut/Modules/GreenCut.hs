module ShortCut.Modules.GreenCut
  where

import Development.Shake
import ShortCut.Core.Types

cutModule :: CutModule
cutModule = CutModule
  { mName = "GreenCut"
  , mDesc = "A re-implementation of the original GreenCut(2) ortholog-finding algorithm"
  , mTypes = []
  , mFunctions =
      [ greencutTwoOrthologs
      ]
  }

greencutTwoOrthologs = undefined
