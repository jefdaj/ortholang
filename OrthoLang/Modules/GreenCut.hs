module OrthoLang.Modules.GreenCut
  where

-- import Development.Shake
import OrthoLang.Core
import OrthoLang.Modules.Blast (bht)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "GreenCut"
  , mDesc = "A re-implementation of the original GreenCut(2) ortholog-finding algorithm"
  , mTypes = [gcr]
  , mFunctions =
      [ greencutTwoFamilies
      ]
  }

-- TODO what should the file look like?
gcr :: Type
gcr = Type
  { tExt  = "gcr"
  , tDesc = "GreenCut results"
  , tShow = defaultShow
  }
 
greencutTwoFamilies :: Function
greencutTwoFamilies = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [bht, bht] gcr
  , fTypeDesc  = mkTypeDesc name  [bht, bht] gcr
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimpleScript $ name ++ ".py"
  }
  where
    name = "greencut2_families"
