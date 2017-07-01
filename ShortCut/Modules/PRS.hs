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
