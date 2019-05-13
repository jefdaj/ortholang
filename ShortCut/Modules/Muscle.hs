module ShortCut.Modules.Muscle
  where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimpleScript)
import ShortCut.Core.Compile.Map   (rMapSimpleScript)
import ShortCut.Core.Paths         (CutPath, fromCutPath)
import ShortCut.Modules.SeqIO      (faa)

cutModule :: CutModule
cutModule = CutModule
  { mName = "MUSCLE"
  , mDesc = "Align sequences with MUSCLE"
  , mTypes = [faa, aln]
  , mFunctions = [muscle, muscleEach]
  }

aln :: CutType
aln = CutType
  { tExt  = "aln"
  , tDesc = "multiple sequence alignment"
  -- , tShow = \_ _ f -> return $ "multiple sequence alignment '" ++ f ++ "'" -- TODO actually show?
  , tShow = defaultShow
  }

muscle :: CutFunction
muscle = let name = "muscle" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [faa] aln
  , fDesc = Nothing, fTypeDesc  = name ++ " : faa -> aln"
  , fFixity    = Prefix
  , fRules     = rSimpleScript "muscle.sh"
  }

muscleEach :: CutFunction
muscleEach = let name = "muscle_each" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf faa] (ListOf aln)
  , fDesc = Nothing, fTypeDesc  = name ++ " : faa.list -> aln.list"
  , fFixity    = Prefix
  , fRules     = rMapSimpleScript 1 "muscle.sh"
  }
