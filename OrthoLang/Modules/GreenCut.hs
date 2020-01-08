module OrthoLang.Modules.GreenOrthoLang
  where

-- import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Compile.Basic (defaultTypeCheck, rSimpleScript)
import OrthoLang.Modules.Blast (bht)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "GreenOrthoLang"
  , mDesc = "A re-implementation of the original GreenOrthoLang(2) ortholog-finding algorithm"
  , mTypes = [gcr]
  , mFunctions =
      [ greencutTwoFamilies
      ]
  }

-- TODO what should the file look like?
gcr :: OrthoLangType
gcr = OrthoLangType
  { tExt  = "gcr"
  , tDesc = "GreenOrthoLang results"
  , tShow = defaultShow
  }
 
greencutTwoFamilies :: OrthoLangFunction
greencutTwoFamilies = OrthoLangFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [bht, bht] gcr
  , fTypeDesc  = mkTypeDesc name  [bht, bht] gcr
  , fFixity    = Prefix
  , fRules     = rSimpleScript $ name ++ ".py"
  }
  where
    name = "greencut2_families"
