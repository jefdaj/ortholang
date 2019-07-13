module ShortCut.Modules.GreenCut
  where

-- TODO filter_identity{,_each} (BlastHits module)
-- TODO filter_bitscore{,_each} (BlastHits module)
-- TODO greencutTwoOrthogroups

import Development.Shake
import ShortCut.Core.Types

cutModule :: CutModule
cutModule = CutModule
  { mName = "GreenCut"
  , mDesc = "A re-implementation of the original GreenCut(2) ortholog-finding algorithm"
  , mTypes = []
  , mFunctions =
      [ -- greencutTwoOrthogroups
      ]
  }

greencutTwoOrthogroups = undefined
