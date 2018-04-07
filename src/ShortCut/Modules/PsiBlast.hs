module ShortCut.Modules.PsiBlast where

{- This actually uses PSI-BLAST-exB, which is supposed to be dramatically
 - faster. I haven't compared them directly yet, but it does seem fast.
 -}

import Development.Shake
import ShortCut.Core.Types

cutModule :: CutModule
cutModule = CutModule
  { mName = "psiblast"
  , mFunctions = []
  }

-- TODO checkpoint PSSM type? not unless needed

pssm :: CutType
pssm = CutType
  { tExt  = "pssm"
  , tDesc = "PSI-BLAST position-specific substitution matrix (ASCII)"
  , tShow  = defaultShow
  }

-- TODO psiblast_train        : faa  faa      -> pssm
-- TODO psiblast_train_all    : faa  faa.list -> pssm
-- TODO psiblast_train_db     : faa  pdb      -> pssm
-- TODO psiblast              : faa  faa      -> bht
-- TODO psiblast_all          : faa  faa.list -> bht
-- TODO psiblast_db           : faa  pdb      -> bht
-- TODO psiblast_pssm         : pssm faa      -> bht
-- TODO psiblast_pssm_db      : pssm pdb      -> bht
-- TODO psiblast_db_each      : faa  pdb.list -> bht.list
-- TODO psiblast_pssm_each    : pssm faa.list -> bht.list
-- TODO psiblast_pssm_db_each : pssm pdb.list -> bht.list
