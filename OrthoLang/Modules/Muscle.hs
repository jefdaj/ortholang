module OrthoLang.Modules.Muscle
  where

-- import Development.Shake
import OrthoLang.Core
import OrthoLang.Modules.SeqIO (faa)

olModule :: Module
olModule = Module
  { mName = "MUSCLE"
  , mDesc = "Align sequences with MUSCLE"
  , mTypes = [faa, aln]
  , mGroups = []
  , mEncodings = []
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
  -- , fTypeCheck = defaultTypeCheck name [faa] aln
  -- , fTypeDesc  = name ++ " : faa -> aln"
  , fInputs = [Exactly faa]
  , fOutput = Exactly aln
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rSimpleScript "muscle.sh"
  }

muscleEach :: Function
muscleEach = let name = "muscle_each" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [ListOf faa] (ListOf aln)
  -- , fTypeDesc  = name ++ " : faa.list -> aln.list"
  , fInputs = [Exactly (ListOf faa)]
  , fOutput =  Exactly (ListOf aln)
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rMapSimpleScript 1 "muscle.sh"
  }
