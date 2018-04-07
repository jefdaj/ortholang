module ShortCut.Modules.PsiBlast where

{- This actually uses PSI-BLAST-exB, which is supposed to be dramatically
 - faster. I haven't compared them directly yet, but it does seem fast.
 -}

-- import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile.Basic (defaultTypeCheck)
import ShortCut.Modules.BlastDB    (pdb)
import ShortCut.Modules.SeqIO      (faa)

cutModule :: CutModule
cutModule = CutModule
  { mName = "psiblast"
  , mFunctions =
    [ psiblastTrain
    , psiblastTrainAll
    , psiblastTrainDb
    ]
  }

-- TODO checkpoint PSSM type? not unless needed

pssm :: CutType
pssm = CutType
  { tExt  = "pssm"
  , tDesc = "PSI-BLAST position-specific substitution matrix as ASCII"
  , tShow  = defaultShow
  }

-----------------
-- train pssms --
-----------------

psiblastTrainDb :: CutFunction
psiblastTrainDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [faa, pdb] pssm
  , fTypeDesc  = mkTypeDesc name  [faa, pdb] pssm
  , fFixity    = Prefix
  , fRules     = undefined
  }
  where
    name = "psiblast_train_db"

psiblastTrain :: CutFunction
psiblastTrain = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [faa, faa] pssm
  , fTypeDesc  = mkTypeDesc name  [faa, faa] pssm
  , fFixity    = Prefix
  , fRules     = undefined
  }
  where
    name = "psiblast_train"

psiblastTrainAll :: CutFunction
psiblastTrainAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [faa, ListOf faa] pssm
  , fTypeDesc  = mkTypeDesc name  [faa, ListOf faa] pssm
  , fFixity    = Prefix
  , fRules     = undefined
  }
  where
    name = "psiblast_train_all"

-- TODO psiblast              : faa  faa      -> bht
-- TODO psiblast_all          : faa  faa.list -> bht
-- TODO psiblast_db           : faa  pdb      -> bht
-- TODO psiblast_pssm         : pssm faa      -> bht
-- TODO psiblast_pssm_db      : pssm pdb      -> bht
-- TODO psiblast_db_each      : faa  pdb.list -> bht.list
-- TODO psiblast_pssm_each    : pssm faa.list -> bht.list
-- TODO psiblast_pssm_db_each : pssm pdb.list -> bht.list
