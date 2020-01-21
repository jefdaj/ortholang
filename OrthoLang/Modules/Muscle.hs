module OrthoLang.Modules.Muscle
  where

-- import Development.Shake
import OrthoLang.Core.Types

import OrthoLang.Core.Compile.Basic (defaultTypeCheck, rSimpleScript)
import OrthoLang.Core.Compile.Map   (rMapSimpleScript)
-- import OrthoLang.Core.Paths         (OrthoLangPath, fromOrthoLangPath)
import OrthoLang.Modules.SeqIO      (faa)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "MUSCLE"
  , mDesc = "Align sequences with MUSCLE"
  , mTypes = [faa, aln]
  , mFunctions = [muscle, muscleEach]
  }

aln :: OrthoLangType
aln = OrthoLangType
  { tExt  = "aln"
  , tDesc = "multiple sequence alignment"
  -- , tShow = \_ _ f -> return $ "multiple sequence alignment '" ++ f ++ "'" -- TODO actually show?
  , tShow = defaultShow
  }

muscle :: OrthoLangFunction
muscle = let name = "muscle" in OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [faa] aln
  , fTypeDesc  = name ++ " : faa -> aln"
  , fFixity    = Prefix
  , fRules     = rSimpleScript "muscle.sh"
  }

muscleEach :: OrthoLangFunction
muscleEach = let name = "muscle_each" in OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [ListOf faa] (ListOf aln)
  , fTypeDesc  = name ++ " : faa.list -> aln.list"
  , fFixity    = Prefix
  , fRules     = rMapSimpleScript 1 "muscle.sh"
  }
