module ShortCut.Modules.PRS where

import ShortCut.Core.Types
import ShortCut.Modules.PRS.Permute
import ShortCut.Modules.PRS.Repeat
import ShortCut.Modules.PRS.Summarize

cutModule :: CutModule
cutModule = CutModule
  { mName = "prs"
  , mFunctions =
    [ leaveOneOut    -- permute
    , repeatEach     -- repeat
    , commonElements -- summarize
    ]
  }

-- TODO break this up into 3 modules again?
--      summarize functions are useful separately, but not the rest?
--      maybe that means you should combine permute + repeat for users
--      that would make the "repeat only" one simple to use too

-- TODO write some more interesting functions:
--   permutations that split into testing + validation data
--   a permutation that just cycles, for the repeat only function
--   ...
