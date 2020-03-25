module OrthoLang.Modules.GreenCut
  where

-- import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Compile (defaultTypeCheck)
import OrthoLang.Core.Compile (rSimpleScript)
import OrthoLang.Modules.Blast (bht)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "GreenCut"
  , mDesc = "A re-implementation of the original GreenCut(2) ortholog-finding algorithm"
  , mTypes = [gcr]
  , mFunctions =
      [ greencutTwoFamilies
      ]
  }

-- TODO what should the file look like?
gcr :: OrthoLangType
gcr = OrthoLangType
  { tExt  = "gcr"
  , tDesc = "GreenCut results"
  , tShow = defaultShow
  }
 
greencutTwoFamilies :: OrthoLangFunction
greencutTwoFamilies = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [bht, bht] gcr
  , fTypeDesc  = mkTypeDesc name  [bht, bht] gcr
  , fFixity    = Prefix, fTags = []
  , fNewRules = Nothing, fOldRules = rSimpleScript $ name ++ ".py"
  }
  where
    name = "greencut2_families"
