module ShortCut.Modules.Sets where

import Prelude      hiding (div)
import ShortCut.Core.Types (CutModule(..), CutFunction(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "sets"
  , mFunctions = [union, diff, intersect]
  }

union :: CutFunction
union = CutFunction
  { fName = "union"
  , fAccepts = undefined
  , fReturns = undefined
  }

diff :: CutFunction
diff = CutFunction
  { fName = "difference"
  , fAccepts = undefined
  , fReturns = undefined
  }

intersect :: CutFunction
intersect = CutFunction
  { fName = "intersection"
  , fAccepts = undefined
  , fReturns = undefined
  }
