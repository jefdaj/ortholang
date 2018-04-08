module ShortCut.Modules.PsiBlast where

-- TODO incorporate deltablast too?
-- TODO checkpoint PSSM type? not unless needed

{- There are a lot of these and it gets confusing! Some naming conventions:
 -
 - anything with "train" trains and returns one or more pssms
 - anything without "train" runs a regular blast search and returns hits
 - anything with "_all" takes a list of fastas and creates one db from it
 - anything with "_db" takes a blast database directly
 - anything without "_db" auto-builds the db from one or more fastas
 - anything with "_each" vectorizes its last argument
 -
 - The difference between "_each" and "_all" is that "_each" returns a list of
 - results, whereas "_all" collapses it into one big result. Of course that's
 - only meaningful for a couple functions.
 -
 - So for example...
 -
 - psiblast_train_all : num faa faa.list -> pssm
 -   auto-builds one blast db from a list of fasta files
 -   trains a pssm for the query fasta on it
 -   returns the pssm
 -
 - psiblast_each : num faa faa.list -> bht.list
 -   auto-builds one db per subject fasta
 -   trains a pssm for the query fasta against each one
 -   runs a final psiblast search against each one using the pssm
 -   returns a list of hit tables
 -
 - The most explicit ones (like psiblast_pssm_db) are simplest to implement.
 - The others generally edit the AST to auto-build PSSMs and/or databases
 - before calling the explicit ones.
 -
 - TODO write more detailed help descriptions for each function
 -}

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Actions       (readLit, readPath, wrappedCmdOut,
                                    wrappedCmdWrite, debugL, debugA, debugNeed)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rExpr, debugRules)
import ShortCut.Core.Paths         (CutPath, fromCutPath, toCutPath, cacheDir,
                                    exprPath)
import ShortCut.Core.Util          (stripWhiteSpace)
import ShortCut.Modules.BlastDB    (pdb)
import ShortCut.Modules.Blast      (bht)
import ShortCut.Modules.SeqIO      (faa)
import Data.Scientific             (formatScientific, FPFormat(..))
import ShortCut.Core.Compile.Each  (rEach)

cutModule :: CutModule
cutModule = CutModule
  { mName = "psiblast"
  , mFunctions =

    -- search with fasta queries (pssm stuff hidden)
    [ psiblast       -- num faa faa      -> bht
    , psiblastEach   -- num faa faa.list -> bht.list
    , psiblastAll    -- num faa faa.list -> bht
    , psiblastDb     -- num faa pdb      -> bht
    , psiblastDbEach -- num faa pdb.list -> bht.list

    -- explicitly train pssms
    , psiblastTrain       -- num faa faa      -> pssm
    , psiblastTrainEach   -- num faa faa.list -> pssm.list
    , psiblastTrainAll    -- num faa faa.list -> pssm
    , psiblastTrainDb     -- num faa pdb      -> pssm
    , psiblastTrainDbEach -- num faa pdb.list -> pssm.list

    -- search with explicit pssm queries
    , psiblastPssm       -- num pssm faa      -> bht
    , psiblastPssmEach   -- num pssm faa.list -> bht.list
    , psiblastPssmAll    -- num pssm faa.list -> bht
    , psiblastPssmDb     -- num pssm pdb      -> bht
    , psiblastPssmDbEach -- num pssm pdb.list -> bht.list

    -- TODO _all functions that just concatenate the hit tables?
    -- not unless I can figure out how to do it without misleading evalues

    -- TODO functions that combine multiple dbs using blast_aliastool?
    -- psiblastDbAll
    -- psiblastPssmDbAll
    -- psiblastTrainDbAll
    -- ...
    ]
  }

pssm :: CutType
pssm = CutType
  { tExt  = "pssm"
  , tDesc = "PSI-BLAST position-specific substitution matrix as ASCII"
  , tShow  = defaultShow
  }

--------------------
-- base functions --
--------------------

-- Base rules for running psiblast with a single query and subject to get a
-- single hit table or pssm
rPsiblastBase :: [String] -> RulesFn
rPsiblastBase args st@(_, cfg, ref) expr@(CutFun _ _ _ _ [e, q, db]) = do
  (ExprPath ePath' ) <- rExpr st e
  (ExprPath qPath' ) <- rExpr st q
  (ExprPath dbPath') <- rExpr st db
  let ePath  = toCutPath cfg ePath'
      qPath  = toCutPath cfg qPath'
      dbPath = toCutPath cfg dbPath'
      oPath  = exprPath st expr
      oPath' = debugRules cfg "rPsiblast" expr $ fromCutPath cfg oPath
  oPath' %> \_ -> aPsiblastBase args cfg ref oPath ePath qPath dbPath
  return (ExprPath oPath')
rPsiblastBase _ _ _ = error "bad argument to rPsiblast"

-- Base rules for running psiblast with one query and a list of subjects
-- to get a list of hit tables or pssms
rPsiblastBaseEach :: [String] -> RulesFn
rPsiblastBaseEach args = rEach aPsiblastHack
  where
    aPsiblastHack cfg ref [o,e,q,d] = aPsiblastBase args cfg ref o e q d
    aPsiblastHack _ _ _ = error "bad argument to rPsiblastBaseEach"

-- All the other functions eventually call this one or more times after some
-- prep work. It runs psiblast with an evalue, query, and subject and returns a
-- pssm or blast hits table depending on the args
aPsiblastBase :: [String] -> CutConfig -> Locks
              -> CutPath -> CutPath -> CutPath -> CutPath
              -> Action ()
aPsiblastBase args cfg ref oPath ePath qPath dbPath = do
  let oPath'  = fromCutPath cfg oPath
      ePath'  = fromCutPath cfg ePath
      qPath'  = fromCutPath cfg qPath -- might be a fasta or pssm
      dbPath' = fromCutPath cfg dbPath
  debugNeed cfg "aPsiblastTrainDb" [ePath', qPath', dbPath']
  eStr  <- readLit  cfg ref ePath' -- TODO is converting to decimal needed?
  dbPre <- readPath cfg ref dbPath'
  let eDec = formatScientific Fixed Nothing $ read eStr
      cDir = fromCutPath cfg $ cacheDir cfg "psiblast"
      dbPre' = fromCutPath cfg dbPre
      args' =
        ["-query", qPath', "-evalue", eDec, "-db", dbPre'] ++ args ++ [oPath']
        -- , "-num_threads", "8"    -- TODO add this in the wrapper script
        -- , "-out", undefined      -- TODO include this?
        -- , "-out_pssm", undefined -- TODO include this?
  -- make sure to get the exb version instead of whatever the system assumes
  -- TODO is this needed, or will it end up the default?
  psiblastBin <- fmap stripWhiteSpace $
                   wrappedCmdOut cfg ref [] [] [] "which" ["psiblast"]
  debugL cfg $ "psiblast binary: " ++ psiblastBin
  let oPath'' = debugA cfg "aPsiblastTrainDb" oPath' [eDec, qPath', dbPath']
  wrappedCmdWrite cfg ref oPath''
    [dbPre' ++ ".*"]        -- inPtns TODO is this right?
    []                      -- extra outPaths to lock TODO more -out stuff?
    [AddEnv "BLASTDB" cDir] -- opts TODO Shell? more specific cache?
    psiblastBin args'       -- TODO package and find psiblast-exb (in wrapper?)

-------------------------------
-- search with fasta queries --
-------------------------------

psiblast :: CutFunction
psiblast = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, faa, faa] bht
  , fFixity    = Prefix
  , fRules     = rPsiblast
  }
  where
    name = "psiblast"

rPsiblast :: RulesFn
rPsiblast st expr@(CutFun _ salt _ _ [e, q, s]) = rExpr st bhtExpr
  where
    dbExpr  = CutFun  pdb salt (depsOf s) "makeblastdb_prot" [s]
    bhtExpr = CutFun  bht salt (depsOf expr) "psiblast_db" [e, q, dbExpr]
rPsiblast _ _ = error "bad argument to rPsiblast"

psiblastEach :: CutFunction
psiblastEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rPsiblastEach
  }
  where
    name = "psiblast_each"

rPsiblastEach :: RulesFn
rPsiblastEach st expr = rPsiblastBaseEach searchArgs st expr'
  where
    expr' = undefined expr -- TODO convert single faa -> db here right?

psiblastAll :: CutFunction
psiblastAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] bht
  , fFixity    = Prefix
  , fRules     = rPsiblastAll
  }
  where
    name = "psiblast_all"

rPsiblastAll :: RulesFn
rPsiblastAll st expr@(CutFun _ salt _ _ [e, fa, fas]) = rExpr st bhtExpr
  where
    listExpr = CutList pdb salt (depsOf fa) [fa]
    dbExpr   = CutFun  pdb salt (depsOf listExpr) "makeblastdb_prot" [listExpr]
    bhtExpr  = CutFun  bht salt (depsOf expr) "psiblast_pssm" [e, dbExpr, fas]
rPsiblastAll _ _ = error "bad argument to rPsiblastAll"

psiblastDb :: CutFunction
psiblastDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] bht
  , fTypeDesc  = mkTypeDesc name  [num, faa, pdb] bht
  , fFixity    = Prefix
  , fRules     = rPsiblastDb
  }
  where
    name = "psiblast_db"

rPsiblastDb :: RulesFn
rPsiblastDb st expr@(CutFun _ salt _ _ [e, fa, db]) = rExpr st bhtExpr
  where
    pssmExpr = CutFun pssm salt (depsOf expr)     "psiblast_train"   [e, fa      , db]
    bhtExpr  = CutFun bht  salt (depsOf pssmExpr) "psiblast_pssm_db" [e, pssmExpr, db]
rPsiblastDb _ _ = error "bad argument to rPsiblastDb"

psiblastDbEach :: CutFunction
psiblastDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rPsiblastBaseEach searchArgs
  }
  where
    name = "psiblast_db_each"

----------------------------
-- explicitly train pssms --
----------------------------

{- Because the PSSM training and final BLAST use different arguments, their
 - shared compiler takes those as... an argument. These go after the shared
 - args but before the outfile. It's a little awkward but DRYs up the code.
 -}
trainingArgs :: [String]
trainingArgs =
  [ "-comp_based_stats", "1"  -- prevent an unnecessary warning
  , "-num_alignments"  , "0"  -- don't print actual alignments (huge text)
  , "-num_iterations"  , "99" -- keep iterating until convergence
  , "-save_pssm_after_last_round"
  , "-out_ascii_pssm" -- < outPath will be appended here
  ]

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
rPsiblastTrain st (CutFun _ salt _ _ [e, q, s]) = rExpr st expr
  where
    ss   = CutList pdb  salt (depsOf s ) [s]
    db   = CutFun  pdb  salt (depsOf ss) "makeblastdb_prot"  [ss]
    expr = CutFun  pssm salt (depsOf db) "psiblast_train_db" [e, q, db]
rPsiblastTrain _ _ = error "bad argument to rPsiblastTrain"

psiblastTrainEach :: CutFunction
psiblastTrainEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] (ListOf pssm)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] (ListOf pssm)
  , fFixity    = Prefix
  , fRules     = rPsiblastTrainEach
  }
  where
    name = "psiblast_train_each"

rPsiblastTrainEach :: RulesFn
rPsiblastTrainEach st (CutFun _ salt _ _ [e, q, ss]) = rExpr st pssms
  where
    dbs   = CutFun  pdb salt (depsOf ss ) "makeblastdb_prot_each"  [ss]
    pssms = CutFun pssm salt (depsOf dbs) "psiblast_train_db_each" [e, q, dbs]
rPsiblastTrainEach _ _ = error "bad argument to rPsiblastTrain"

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

psiblastTrainDb :: CutFunction
psiblastTrainDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] pssm
  , fTypeDesc  = mkTypeDesc name  [num, faa, pdb] pssm
  , fFixity    = Prefix
  , fRules     = rPsiblastBase trainingArgs
  }
  where
    name = "psiblast_train_db"

-- TODO remove? not sure when you would use this one
psiblastTrainDbEach :: CutFunction
psiblastTrainDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf pssm)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf pssm)
  , fFixity    = Prefix
  , fRules     = rPsiblastBaseEach trainingArgs
  }
  where
    name = "psiblast_train_db_each"

---------------------------------------
-- search with explicit pssm queries --
---------------------------------------

psiblastPssm :: CutFunction
psiblastPssm = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, pssm, faa] bht
  , fFixity    = Prefix
  , fRules     = rPsiblastPssm
  }
  where
    name = "psiblast_pssm"

rPsiblastPssm :: RulesFn
rPsiblastPssm st expr@(CutFun _ salt _ _ [e, q, fa]) = rExpr st expr'
  where
    fas   = CutList pdb salt (depsOf fa  ) [fa]
    db    = CutFun  pdb salt (depsOf fas ) "makeblastdb_prot" [fas]
    expr' = CutFun  bht salt (depsOf expr) "psiblast_pssm_db" [e, q, db]
rPsiblastPssm _ _ = error "bad argument to rPsiblastPssm"

psiblastPssmAll :: CutFunction
psiblastPssmAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf faa] bht
  , fFixity    = Prefix
  , fRules     = rPsiblastPssmAll
  }
  where
    name = "psiblast_pssm_all"

rPsiblastPssmAll :: RulesFn
rPsiblastPssmAll st expr@(CutFun _ salt _ _ [e, q, fas]) = rExpr st bhtExpr
  where
    dbExpr  = CutFun pdb salt (depsOf fas ) "makeblastdb_prot" [fas]
    bhtExpr = CutFun bht salt (depsOf expr) "psiblast_pssm_db" [e, q, dbExpr]
rPsiblastPssmAll _ _ = error "bad argument to rPsiblastPssm"

psiblastPssmEach :: CutFunction
psiblastPssmEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf faa] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf faa] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rPsiblastPssmEach
  }
  where
    name = "psiblast_pssm_each"

searchArgs :: [String]
searchArgs = ["-outfmt", "6", "-out"]

rPsiblastPssmEach :: RulesFn
rPsiblastPssmEach st expr = rPsiblastBaseEach searchArgs st expr'
  where
    expr' = undefined expr -- TODO convert single faa -> db here right?

-- TODO would be nice to name the pssm so it doesnt just say Query_1
psiblastPssmDb :: CutFunction
psiblastPssmDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, pdb] bht
  , fTypeDesc  = mkTypeDesc name  [num, pssm, pdb] bht
  , fFixity    = Prefix
  , fRules     = rPsiblastBase searchArgs
  }
  where
    name = "psiblast_pssm_db"

psiblastPssmDbEach :: CutFunction
psiblastPssmDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf pdb] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rPsiblastPssmDbEach
  }
  where
    name = "psiblast_pssm_db_each"

rPsiblastPssmDbEach :: RulesFn
rPsiblastPssmDbEach st expr = rPsiblastBaseEach searchArgs st expr'
  where
    expr' = undefined expr -- TODO convert single faa -> db here right?
