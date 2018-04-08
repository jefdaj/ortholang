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
 - Here are all the types so far:
 -   psiblast_train         : num faa  faa      -> pssm
 -   psiblast_train_db      : num faa  pdb      -> pssm
 -   psiblast_train_all     : num faa  faa.list -> pssm
 -   psiblast_train_db_each : num faa  pdb.list -> pssm.list
 -   psiblast               : num faa  faa      -> bht      (TODO fix vectorizing)
 -   psiblast_all           : num faa  faa.list -> bht      (TODO what's wrong with it?)
 -   psiblast_each          : num faa  faa.list -> bht.list (TODO write this)
 -   psiblast_db            : num faa  pdb      -> bht      (TODO fix this)
 -   psiblast_pssm          : num pssm faa      -> bht      (TODO fix this)
 -   psiblast_pssm_each     : num pssm faa.list -> bht.list (TODO fix this)
 -   psiblast_pssm_all      : num pssm faa.list -> bht      (TODO fix this)
 -   psiblast_pssm_db       : num pssm pdb      -> bht
 -
 - These aren't all the functions that could be written, just the ones that
 - seemed relatively easy and useful at the time.
 -
 - TODO write more detailed help descriptions for each function
 -}

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions       (readLit, readPath, wrappedCmdOut,
                                    wrappedCmdWrite, debugL, debugA, debugNeed)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rExpr, debugRules)
import ShortCut.Core.Paths         (CutPath, fromCutPath, toCutPath, cacheDir, exprPath)
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
    [ psiblastTrain
    , psiblastTrainAll
    , psiblastTrainDb
    , psiblastPssmDb
    , psiblastDb
    , psiblastPssm
    , psiblastAll
    , psiblastOne
    , psiblastTrainDbEach -- TODO remove? not sure if useful
    , psiblastDbEach
    , psiblastEach
    ]
  }

pssm :: CutType
pssm = CutType
  { tExt  = "pssm"
  , tDesc = "PSI-BLAST position-specific substitution matrix as ASCII"
  , tShow  = defaultShow
  }

----------------------------
-- main psiblast function --
----------------------------

psiblastCache :: CutConfig -> CutPath
psiblastCache cfg = cacheDir cfg "psiblast"

-- All the psiblast* functions eventually call this,
-- but they might edit the expression first or pass different args
rPsiblast :: [String] -> RulesFn
rPsiblast args st@(_, cfg, ref) expr@(CutFun _ _ _ _ [e, q, db]) = do
  (ExprPath ePath' ) <- rExpr st e
  (ExprPath qPath' ) <- rExpr st q
  (ExprPath dbPath') <- rExpr st db
  let ePath  = toCutPath cfg ePath'
      qPath  = toCutPath cfg qPath'
      dbPath = toCutPath cfg dbPath'
      oPath  = exprPath st expr
      oPath' = debugRules cfg "rPsiblast" expr $ fromCutPath cfg oPath
  oPath' %> \_ -> aPsiblast args cfg ref oPath ePath qPath dbPath
  return (ExprPath oPath')
rPsiblast _ _ _ = error "bad argument to rPsiblast"

aPsiblast :: [String] -> CutConfig -> Locks
          -> CutPath -> CutPath -> CutPath -> CutPath -> Action ()
aPsiblast args cfg ref oPath ePath qPath dbPath = do
  let oPath'  = fromCutPath cfg oPath
      ePath'  = fromCutPath cfg ePath
      qPath'  = fromCutPath cfg qPath -- might be a fasta or pssm
      dbPath' = fromCutPath cfg dbPath
  debugNeed cfg "aPsiblastTrainDb" [ePath', qPath', dbPath']
  eStr  <- readLit  cfg ref ePath' -- TODO is converting to decimal needed?
  dbPre <- readPath cfg ref dbPath'
  let eDec = formatScientific Fixed Nothing $ read eStr
      cDir = fromCutPath cfg $ psiblastCache cfg
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

-----------------------
-- psiblast_train_db --
-----------------------

psiblastTrainDb :: CutFunction
psiblastTrainDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] pssm
  , fTypeDesc  = mkTypeDesc name  [num, faa, pdb] pssm
  , fFixity    = Prefix
  , fRules     = rPsiblast trainingArgs
  }
  where
    name = "psiblast_train_db"

{- Because the PSSM training and final BLAST use different arguments, their
 - shared compiler takes those as... an argument. These go after the shared
 - args but before the outfile. It's a little weird but DRYs up the code a lot.
 -}
trainingArgs :: [String]
trainingArgs =
  [ "-comp_based_stats", "1"  -- prevent an unnecessary warning
  , "-num_alignments"  , "0"  -- don't print actual alignments (huge text)
  , "-num_iterations"  , "99" -- keep iterating until convergence
  , "-save_pssm_after_last_round"
  , "-out_ascii_pssm" -- < outPath will be appended here
  ]

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

----------------------
-- psiblast_pssm_db --
----------------------

-- TODO would be nice to name the pssm so it doesnt just say Query_1
psiblastPssmDb :: CutFunction
psiblastPssmDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, pdb] bht
  , fTypeDesc  = mkTypeDesc name  [num, pssm, pdb] bht
  , fFixity    = Prefix
  , fRules     = rPsiblast searchArgs
  }
  where
    name = "psiblast_pssm_db"

searchArgs :: [String]
searchArgs = ["-outfmt", "6", "-out"]

-----------------
-- psiblast_db --
-----------------

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

-------------------
-- psiblast_pssm --
-------------------

-- TODO should this be a single and psiblast_pssm_each the map version?
psiblastPssm :: CutFunction
psiblastPssm = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf faa] bht
  , fFixity    = Prefix
  , fRules     = rPsiblastPssm
  }
  where
    name = "psiblast_pssm"

rPsiblastPssm :: RulesFn
rPsiblastPssm st expr@(CutFun _ salt _ _ [e, q, fas]) = rExpr st bhtExpr
  where
    dbExpr  = CutFun pdb salt (depsOf fas ) "makeblastdb_prot" [fas]
    bhtExpr = CutFun bht salt (depsOf expr) "psiblast_pssm_db" [e, q, dbExpr]
rPsiblastPssm _ _ = error "bad argument to rPsiblastPssm"

------------------
-- psiblast_all --
------------------

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

--------------
-- psiblast --
--------------

-- can't call it just rPsiblast because that was taken
psiblastOne :: CutFunction
psiblastOne = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, faa, faa] bht
  , fFixity    = Prefix
  , fRules     = rPsiblastOne
  }
  where
    name = "psiblast"

rPsiblastOne :: RulesFn
rPsiblastOne st expr@(CutFun _ salt _ _ [e, q, s]) = rExpr st bhtExpr
  where
    dbExpr  = CutFun  pdb salt (depsOf s) "makeblastdb_prot" [s]
    bhtExpr = CutFun  bht salt (depsOf expr) "psiblast_db" [e, q, dbExpr]
rPsiblastOne _ _ = error "bad argument to rPsiblastOne"

--------------------
-- psiblast*_each --
--------------------

-- This vectorizes aPsiblast calls so they work on list expressions...
-- it's a little mind-bending, but hey at least the code is short!
mkPsiblastEach :: [String] -> RulesFn
mkPsiblastEach args = rEach aPsiblastHack
  where
    aPsiblastHack cfg ref [o,e,q,d] = aPsiblast args cfg ref o e q d
    aPsiblastHack _ _ _ = error "bad argument to mkPsiblastEach"

-- TODO remove? not sure when you would use this one
psiblastTrainDbEach :: CutFunction
psiblastTrainDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf pssm)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf pssm)
  , fFixity    = Prefix
  , fRules     = mkPsiblastEach trainingArgs
  }
  where
    name = "psiblast_train_db_each"

psiblastDbEach :: CutFunction
psiblastDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = mkPsiblastEach searchArgs
  }
  where
    name = "psiblast_db_each"

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
rPsiblastEach st expr = mkPsiblastEach searchArgs st expr'
  where
    expr' = undefined expr -- TODO convert single faa -> db here right?

-- TODO shit, guess I need to copy from above, but substitute in the rEach call?
-- TODO or better, make each versions of the basic fns and edit their exprs

-- rPsiblastDbEach :: RulesFn
-- rPsiblastDbEach = rEach aPsiblastDb

-- rPsiblastPssmEach :: RulesFn
-- rPsiblastPssmEach = rEach aPsiblastPssm

-- TODO write split_fastas so you can try actually running this!
-- TODO and the rest of these, which should straightforward now:
-- psiblast_db_each      : faa  pdb.list -> bht.list
-- psiblast_pssm_each    : pssm faa.list -> bht.list
-- psiblast_pssm_db_each : pssm pdb.list -> bht.list
-- psiblast_each         : faa faa.list -> bht.list
