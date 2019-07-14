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
      [ greencutTwoOrthogroups
      ]
  }

-- TODO what should the file look like?
gcr :: CutType
gcr = CutType
  { tExt  = "spr"
  , tDesc = "GreenCut results"
  , tShow = defaultShow
  }
 
greencutTwoOrthogroups :: CutFunction
greencutTwoOrthogroups = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [bht, bht] gcr
  , fTypeDesc  = mkTypeDesc name  [bht, bht] gcr
  , fFixity    = Prefix
  , fRules     = rSimpleScript $ name ++ ".py"
  }
  where
    name = "greencut2_orthogroups"
