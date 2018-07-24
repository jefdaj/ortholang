module ShortCut.Modules.Hmmer
  where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Modules.SeqIO (faa)
import ShortCut.Modules.Muscle (aln)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths (CutPath, fromCutPath)
import ShortCut.Core.Actions (debugA, wrappedCmdWrite, readLit)
import Data.Scientific (formatScientific, FPFormat(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "hmmer"
  , mFunctions = [hmmbuild, hmmsearch]
  }

hmm :: CutType
hmm = CutType
  { tExt  = "hmm"
  , tDesc = "hidden markov model"
  -- , tShow = \_ _ f -> return $ "hidden markov model '" ++ f ++ "'"
  , tShow = defaultShow
  }

hht :: CutType
hht = CutType
  { tExt  = "hht"
  , tDesc = "HMMER hits table"
  , tShow = defaultShow -- TODO is this OK?
  }

hmmbuild :: CutFunction
hmmbuild = let name = "hmmbuild" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [aln] hmm
  , fTypeDesc  = name ++ " : aln -> hmm" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rSimple aHmmbuild
  }

hmmsearch :: CutFunction
hmmsearch = let name = "hmmsearch" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, hmm, faa] hht
  , fTypeDesc  = name ++ " : num hmm faa -> hht" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rSimple aHmmsearch
  }

-- TODO is it parallel?
-- TODO reverse order? currently matches blast fns but not native hmmbuild args
aHmmbuild :: CutConfig -> Locks -> [CutPath] -> Action ()
aHmmbuild cfg ref [out, fa] = do
  wrappedCmdWrite False True cfg ref out'' [fa'] [] [] "hmmbuild" [out', fa']
  where
    out'  = fromCutPath cfg out
    out'' = debugA cfg "aHmmbuild" out' [out', fa']
    fa'   = fromCutPath cfg fa
aHmmbuild _ _ args = error $ "bad argument to aHmmbuild: " ++ show args

-- TODO is it parallel?
-- TODO reverse order? currently matches blast fns but not native hmmsearch args
aHmmsearch :: CutConfig -> Locks -> [CutPath] -> Action ()
aHmmsearch cfg ref [out, e, hm, fa] = do
  eStr <- readLit cfg ref e'
  let eDec = formatScientific Fixed Nothing (read eStr) -- format as decimal
  wrappedCmdWrite False True cfg ref out'' [e', hm', fa'] [] []
    "hmmsearch" ["-E", eDec, "--tblout", out', hm', fa']
  where
    out'  = fromCutPath cfg out
    out'' = debugA cfg "aHmmsearch" out' [out', fa']
    e'    = fromCutPath cfg e
    hm'   = fromCutPath cfg hm
    fa'   = fromCutPath cfg fa
aHmmsearch _ _ args = error $ "bad argument to aHmmsearch: " ++ show args
