module ShortCut.Modules.PsiBlast where

-- TODO incorporate deltablast too?
-- TODO checkpoint PSSM type? not unless needed

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions       (readLit, readPath, wrappedCmdOut,
                                    wrappedCmdWrite, debugL, debugA, debugNeed)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rExpr, debugRules)
import ShortCut.Core.Paths         (CutPath, fromCutPath, toCutPath, cacheDir, exprPath)
import ShortCut.Core.Util          (stripWhiteSpace)
import ShortCut.Modules.BlastDB    (pdb)
import ShortCut.Modules.SeqIO      (faa)
import Data.Scientific             (formatScientific, FPFormat(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "psiblast"
  , mFunctions =
    [ psiblastTrain
    , psiblastTrainAll
    , psiblastTrainDb
    ]
  }

pssm :: CutType
pssm = CutType
  { tExt  = "pssm"
  , tDesc = "PSI-BLAST position-specific substitution matrix as ASCII"
  , tShow  = defaultShow
  }

-----------------------
-- psiblast_train_db --
-----------------------

psiblastTrainDb :: CutFunction
psiblastTrainDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] pssm
  , fTypeDesc  = mkTypeDesc name  [num, faa, pdb] pssm
  , fFixity    = Prefix
  , fRules     = rPsiblastTrainDb
  }
  where
    name = "psiblast_train_db"

psiblastCache :: CutConfig -> CutPath
psiblastCache cfg = cacheDir cfg "psiblast"

-- All the psiblast_train* functions eventually call this,
-- but they might edit the expression to build the db from fastas first
rPsiblastTrainDb :: RulesFn
rPsiblastTrainDb st@(_, cfg, ref) expr@(CutFun _ _ _ _ [e, fa, db]) = do
  (ExprPath ePath' ) <- rExpr st e
  (ExprPath faPath') <- rExpr st fa
  (ExprPath dbPath') <- rExpr st db
  let ePath  = toCutPath cfg ePath'
      faPath = toCutPath cfg faPath'
      dbPath = toCutPath cfg dbPath'
      oPath  = exprPath st expr
      oPath' = debugRules cfg "rPsiblastTrainDb" expr $ fromCutPath cfg oPath
  oPath' %> \_ -> aPsiblastTrainDb cfg ref oPath ePath faPath dbPath
  return (ExprPath oPath')
rPsiblastTrainDb _ _ = error "bad argument to rPsiblastTrainDb"

aPsiblastTrainDb :: CutConfig -> Locks
                 -> CutPath -> CutPath -> CutPath -> CutPath
                 -> Action ()
aPsiblastTrainDb cfg ref oPath ePath faPath dbPath = do
  let oPath'  = fromCutPath cfg oPath
      ePath'  = fromCutPath cfg ePath
      faPath' = fromCutPath cfg faPath
      dbPath' = fromCutPath cfg dbPath
  debugNeed cfg "aPsiblastTrainDb" [ePath', faPath', dbPath']
  eStr  <- readLit  cfg ref ePath' -- TODO is converting to decimal needed?
  dbPre <- readPath cfg ref dbPath'
  let eDec = formatScientific Fixed Nothing $ read eStr
      cDir = fromCutPath cfg $ psiblastCache cfg
      dbPre' = fromCutPath cfg dbPre
      args =
        -- , "-num_threads", "8"    -- TODO add this in the wrapper script
        -- , "-out", undefined      -- TODO include this?
        -- , "-out_pssm", undefined -- TODO include this?
        [ "-save_pssm_after_last_round"
        , "-comp_based_stats", "1"  -- prevent an unnecessary warning
        , "-num_alignments"  , "0"  -- don't print actual alignments (huge text)
        , "-num_iterations"  , "99" -- keep iterating until convergence
        , "-query"           , faPath'
        , "-evalue"          , eDec
        , "-db"              , dbPre'
        , "-out_ascii_pssm"  , oPath'
        ]
  -- make sure to get the exb version instead of whatever the system assumes
  -- TODO is this needed, or will it end up the default?
  psiblastBin <- fmap stripWhiteSpace $
                   wrappedCmdOut cfg ref [] [] [] "which" ["psiblast"]
  debugL cfg $ "psiblast binary: " ++ psiblastBin
  let oPath'' = debugA cfg "aPsiblastTrainDb" oPath' [faPath', eDec, dbPath']
  wrappedCmdWrite cfg ref oPath''
    [dbPre' ++ ".*"]        -- inPtns TODO is this right?
    []                      -- extra outPaths to lock TODO more -out stuff?
    [AddEnv "BLASTDB" cDir] -- opts TODO Shell? more specific cache?
    psiblastBin args        -- TODO package and find psiblast-exb (in wrapper?)

------------------------
-- psiblast_train_all --
------------------------

psiblastTrainAll :: CutFunction
psiblastTrainAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] pssm
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] pssm
  , fFixity    = Prefix
  , fRules     = rPsiblastTrainAll
  }
  where
    name = "psiblast_train_all"

rPsiblastTrainAll :: RulesFn
rPsiblastTrainAll st (CutFun _ salt _ _ [e, fa, fas]) = rExpr st trainExpr
  where
    dbExpr    = CutFun pdb  salt (depsOf fas) "makeblastdb_prot" [fas]
    trainExpr = CutFun pssm salt (depsOf dbExpr) "psiblast_train_db" [e, fa, dbExpr]
rPsiblastTrainAll _ _ = error "bad argument to rPsiblastTrainAll"

--------------------
-- psiblast_train --
--------------------

psiblastTrain :: CutFunction
psiblastTrain = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, faa] pssm
  , fTypeDesc  = mkTypeDesc name  [num, faa, faa] pssm
  , fFixity    = Prefix
  , fRules     = rPsiblastTrain
  }
  where
    name = "psiblast_train"

rPsiblastTrain :: RulesFn
rPsiblastTrain st (CutFun _ salt _ _ [e, q, s]) = rExpr st trainExpr
  where
    listExpr  = CutList pdb salt (depsOf s) [s]
    dbExpr    = CutFun  pdb salt (depsOf listExpr) "makeblastdb_prot" [listExpr]
    trainExpr = CutFun pssm salt (depsOf dbExpr) "psiblast_train_db" [e, q, dbExpr]
rPsiblastTrain _ _ = error "bad argument to rPsiblastTrain"

-- TODO psiblast              : faa  faa      -> bht
-- TODO psiblast_all          : faa  faa.list -> bht
-- TODO psiblast_db           : faa  pdb      -> bht
-- TODO psiblast_pssm         : pssm faa      -> bht
-- TODO psiblast_pssm_db      : pssm pdb      -> bht
-- TODO psiblast_db_each      : faa  pdb.list -> bht.list
-- TODO psiblast_pssm_each    : pssm faa.list -> bht.list
-- TODO psiblast_pssm_db_each : pssm pdb.list -> bht.list
