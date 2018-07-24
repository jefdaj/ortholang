module ShortCut.Modules.Muscle
  where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Modules.SeqIO (faa)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths (CutPath, fromCutPath)
import ShortCut.Core.Actions (debugA, wrappedCmdWrite)

cutModule :: CutModule
cutModule = CutModule
  { mName = "muscle"
  , mFunctions = [muscle]
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
  , fTypeDesc  = name ++ " : faa -> aln"
  , fFixity    = Prefix
  , fRules     = rSimple aMuscle
  }

-- TODO is it parallel?
aMuscle :: CutConfig -> Locks -> [CutPath] -> Action ()
aMuscle cfg ref [out, fa] = do
  wrappedCmdWrite False True cfg ref out'' [fa'] [] []
    "muscle" ["-clwstrict", "-in", fa', "-out", out']
  where
    out'  = fromCutPath cfg out
    out'' = debugA cfg "aMuscle" out' [out', fa']
    fa'   = fromCutPath cfg fa
aMuscle _ _ args = error $ "bad argument to aMuscle: " ++ show args
