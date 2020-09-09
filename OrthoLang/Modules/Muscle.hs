module OrthoLang.Modules.Muscle
  where

import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO (faa)

olModule :: Module
olModule = Module
  { mName = "MUSCLE"
  , mDesc = "Align sequences with MUSCLE"
  , mTypes = [faa, aln]
  , mGroups = []
  , mEncodings = []
  , mRules = []
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
muscle = newFnS1
  "muscle"
  (Exactly faa)
  (Exactly aln)
  "muscle.sh"
  [] -- TODO nondeterministic?
  id

muscleEach :: Function
muscleEach = newFnA1
  "muscle_each"
  (Exactly $ ListOf faa)
  (Exactly $ ListOf aln)
  (newMap1of1 "muscle")
  [] -- TODO nondeterministic?
