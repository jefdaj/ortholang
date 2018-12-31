module Detourrr.Modules.Muscle
  where

import Development.Shake
import Detourrr.Core.Types
import Detourrr.Modules.SeqIO (faa)
import Detourrr.Core.Compile.Basic (defaultTypeCheck, rSimple)
import Detourrr.Core.Paths (CutPath, fromCutPath)
import Detourrr.Core.Actions (debugA, wrappedCmdWrite)
import Detourrr.Core.Compile.Map  (rMap)

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
  , fRules     = rSimple aMuscle
  }

muscleEach :: CutFunction
muscleEach = let name = "muscle_each" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf faa] (ListOf aln)
  , fDesc = Nothing, fTypeDesc  = name ++ " : faa.list -> aln.list"
  , fFixity    = Prefix
  , fRules     = rMap 1 aMuscle
  }

-- TODO is it parallel?
aMuscle :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aMuscle cfg ref _ [out, fa] = do
  wrappedCmdWrite False True cfg ref out'' [fa'] [] []
    "muscle" ["-clwstrict", "-in", fa', "-out", out']
  where
    out'  = fromCutPath cfg out
    out'' = debugA cfg "aMuscle" out' [out', fa']
    fa'   = fromCutPath cfg fa
aMuscle _ _ _ args = error $ "bad argument to aMuscle: " ++ show args
