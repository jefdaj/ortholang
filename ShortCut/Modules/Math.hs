module ShortCut.Modules.Math where

import Prelude      hiding (div)
import ShortCut.Core.Types (CutModule(..), CutFunction(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "math"
  , mFunctions = [add, sub, mul, div]
  }

add :: CutFunction
add = CutFunction
  { fName = "add"
  , fAccepts = undefined
  , fReturns = undefined
  }

sub :: CutFunction
sub = CutFunction
  { fName = "subtract"
  , fAccepts = undefined
  , fReturns = undefined
  }

mul :: CutFunction
mul = CutFunction
  { fName = "multiply"
  , fAccepts = undefined
  , fReturns = undefined
  }

div :: CutFunction
div = CutFunction
  { fName = "divide"
  , fAccepts = undefined
  , fReturns = undefined
  }
