module OrthoLang.Modules.NewRulesTest where

import OrthoLang.Core.Types
import OrthoLang.Core.Compile.Basic (defaultTypeCheck)
import OrthoLang.Modules.SeqIO (faa)


orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "NewRulesTest"
  , mDesc = "Test module for the 'new rules' infrastructure"
  , mTypes = [str, faa]
  , mFunctions =
      [ newRulesTest1
      ]
  }

newRulesTest1 :: OrthoLangFunction
newRulesTest1 = let name = "newrulestest1" in OrthoLangFunction
  { fNames     = [name]
  , fTypeDesc  = mkTypeDesc  name [str, faa] str
  , fTypeCheck = defaultTypeCheck [str, faa] str
  , fFixity    = Prefix, fTags = []
  , fOldRules = undefined
  , fNewRules = Just (return ())
  }
