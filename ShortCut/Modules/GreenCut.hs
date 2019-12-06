module ShortCut.Modules.GreenCut
  where

-- import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimpleScript)
import ShortCut.Modules.Blast (bht)

cutModule :: CutModule
cutModule = CutModule
  { mName = "GreenCut"
  , mDesc = "A re-implementation of the original GreenCut(2) ortholog-finding algorithm"
  , mTypes = [gcr]
  , mFunctions =
      [ greencutTwoFamilies
      ]
  }

-- TODO what should the file look like?
gcr :: CutType
gcr = CutType
  { tExt  = "gcr"
  , tDesc = "GreenCut results"
  , tShow = defaultShow
  }
 
greencutTwoFamilies :: CutFunction
greencutTwoFamilies = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [bht, bht] gcr
  , fTypeDesc  = mkTypeDesc name  [bht, bht] gcr
  , fFixity    = Prefix
  , fRules     = rSimpleScript $ name ++ ".py"
  }
  where
    name = "greencut2_families"
