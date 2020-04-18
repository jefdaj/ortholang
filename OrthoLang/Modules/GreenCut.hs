module OrthoLang.Modules.GreenCut
  where

-- import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.Blast (bht)

olModule :: Module
olModule = Module
  { mName = "GreenCut"
  , mDesc = "A re-implementation of the original GreenCut(2) ortholog-finding algorithm"
  , mTypes = [gcr]
  , mGroups = []
  , mEncodings = []
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
  -- , fTypeCheck = defaultTypeCheck name [bht, bht] gcr
  -- , fTypeDesc  = mkTypeDesc name  [bht, bht] gcr
  , fInputs = [Exactly bht, Exactly bht]
  , fOutput = Exactly gcr
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rSimpleScript $ name ++ ".py"
  }
  where
    name = "greencut2_families"
