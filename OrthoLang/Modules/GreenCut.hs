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
  , mRules = []
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
greencutTwoFamilies = newFnS2
  "greencut2_families"
  (Exactly bht, Exactly bht)
  (Exactly gcr)
  "greencut2_families.py"
  []
  id
