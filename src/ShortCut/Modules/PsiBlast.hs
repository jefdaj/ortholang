module ShortCut.Modules.PsiBlast where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions       (readLit, readPath, wrappedCmdWrite, debugA, debugNeed)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rExpr, debugRules)
import ShortCut.Core.Paths         (CutPath, fromCutPath, toCutPath, cacheDir, exprPath)
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

-- TODO checkpoint PSSM type? not unless needed

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
  let oPath'' = debugA cfg "aPsiblastTrainDb" oPath' [faPath', eDec, dbPath']
  wrappedCmdWrite cfg ref oPath''
    [dbPre' ++ ".*"]        -- inPtns TODO is this right?
    []                      -- extra outPaths to lock TODO more -out stuff?
    [AddEnv "BLASTDB" cDir] -- opts TODO Shell? more specific cache?
    "psiblast" args         -- TODO package and find psiblast-exb (in wrapper?)

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

-- for reference:
-- inserts a "makeblastdb" call and reuses the _db compiler from above
-- rMkBlastFromFa :: BlastDesc -> RulesFn
-- rMkBlastFromFa d@(_, _, _, dbType) st (CutFun rtn salt deps _ [e, q, s])
--   = rules st (CutFun rtn salt deps name1 [e, q, dbExpr])
--   where
--     rules = fRules $ mkBlastFromDb d
--     name1 = fName  $ mkBlastFromDb d
--     name2 = "makeblastdb" ++ if dbType == ndb then "_nucl" else "_prot"
--     faList = CutList (typeOf s) salt (depsOf s) [s]
--     dbExpr = CutFun dbType salt (depsOf faList) name2 [faList] 
-- rMkBlastFromFa _ _ _ = error "bad argument to rMkBlastFromFa"



-- TODO psiblast              : faa  faa      -> bht
-- TODO psiblast_all          : faa  faa.list -> bht
-- TODO psiblast_db           : faa  pdb      -> bht
-- TODO psiblast_pssm         : pssm faa      -> bht
-- TODO psiblast_pssm_db      : pssm pdb      -> bht
-- TODO psiblast_db_each      : faa  pdb.list -> bht.list
-- TODO psiblast_pssm_each    : pssm faa.list -> bht.list
-- TODO psiblast_pssm_db_each : pssm pdb.list -> bht.list
