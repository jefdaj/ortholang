module ShortCut.Modules.Muscle
  where

import ShortCut.Core.Types
import ShortCut.Modules.SeqIO (faa)
-- import ShortCut.Core.Compile.Basic (defaultTypeCheck, aSimpleScript)
import ShortCut.Core.Compile.Basic (defaultTypeCheck)

cutModule :: CutModule
cutModule = CutModule
  { mName = "muscle"
  , mFunctions = [muscle]
  }

aln :: CutType
aln = undefined
  { tExt  = "aln"
  , tDesc = "multiple sequence alignment"
  , tShow = \_ _ f -> return $ "multiple sequence alignment '" ++ f ++ "'" -- TODO actually show?
  }

muscle :: CutFunction
muscle = let name = "muscle" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [faa] aln
  , fTypeDesc  = name ++ " : faa -> aln"
  , fFixity    = Prefix
  , fRules     = undefined
  }
