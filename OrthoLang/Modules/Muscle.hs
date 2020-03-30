module OrthoLang.Modules.Muscle
  where

-- import Development.Shake
import OrthoLang.Core
import OrthoLang.Modules.SeqIO (faa)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "MUSCLE"
  , mDesc = "Align sequences with MUSCLE"
  , mTypes = [faa, aln]
  , mFunctions = [muscle, muscleEach]
  }

aln :: Type
aln = Type
  { tExt  = "aln"
  , tDesc = "multiple sequence alignment"
  -- , tShow = \_ _ f -> return $ "multiple sequence alignment \"" ++ f ++ "\"" -- TODO actually show?
  , tShow = defaultShow
  }

muscle :: Function
muscle = let name = "muscle" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [faa] aln
  , fTypeDesc  = name ++ " : faa -> aln"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimpleScript "muscle.sh"
  }

muscleEach :: Function
muscleEach = let name = "muscle_each" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [ListOf faa] (ListOf aln)
  , fTypeDesc  = name ++ " : faa.list -> aln.list"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMapSimpleScript 1 "muscle.sh"
  }
