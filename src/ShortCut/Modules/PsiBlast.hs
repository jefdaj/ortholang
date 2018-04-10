module ShortCut.Modules.PsiBlast where

-- TODO write more detailed help descriptions for each function
-- TODO incorporate deltablast too?
-- TODO checkpoint PSSM type? not unless needed
-- TODO would be nice to name the pssms so it doesnt just say "Query_1"

-- TODO _all functions that just concatenate the hit tables?
--      not unless I can figure out how to do it without misleading evalues

-- TODO functions that combine multiple dbs using blast_aliastool?
--      psiblastDbAll, psiblastPssmDbAll, psiblastTrainDbAll...

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Actions       (readLit, readPath, wrappedCmdOut,
                                    wrappedCmdWrite, debugL, debugA, debugNeed,
                                    writeCachedLines)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rExpr, debugRules)
import ShortCut.Core.Paths         (CutPath, fromCutPath, toCutPath, cacheDir,
                                    exprPath)
import ShortCut.Core.Util          (stripWhiteSpace)
import ShortCut.Modules.BlastDB    (pdb)
import ShortCut.Modules.Blast      (bht)
import ShortCut.Modules.SeqIO      (faa)
import Data.Scientific             (formatScientific, FPFormat(..))
import ShortCut.Core.Compile.Vectorize  (rVectorize)
import System.FilePath             ((<.>), takeFileName)
import System.Directory            (removeFile)
import Control.Monad               (when)

cutModule :: CutModule
cutModule = CutModule
  { mName = "psiblast"
  , mFunctions =

    {- There are a lot of these! Some naming conventions:
     -
     - A fn with "train" trains and returns one or more pssms ; one without
     - "train" runs a regular blast search and returns hits.
     -
     - A fn with "db" takesone or more blast databases directly; one without
     - "db" auto-builds the db(s) from one or more fastas.
     -
     - A fn with "all" takes a list of fastas and creates one db from it.
     -
     - A fn with "each" vectorizes its last argument. The difference between
     - "each" and "all" is that "each" returns a list of results, whereas "all"
     - collapses it into one big result.
     -
     - A fn with "pssms" (not "pssm") takes a list of pssm queries and combines
     - their hits into one big table. TODO less confusing name for that?
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
     -}
  
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

    -- search with explicit single pssm queries
    , psiblastPssm       -- num pssm faa      -> bht
    , psiblastPssmEach   -- num pssm faa.list -> bht.list
    , psiblastPssmAll    -- num pssm faa.list -> bht
    , psiblastPssmDb     -- num pssm pdb      -> bht
    , psiblastPssmDbEach -- num pssm pdb.list -> bht.list

    -- search with lists of explicit pssm queries and concat hits
    -- TODO for psiblast_pssms, make a list of psiblast_pssm calls plus one concat_bht
    -- , psiblastPssms       -- num pssm.list faa      -> bht
    -- , psiblastPssmsEach   -- num pssm.list faa.list -> bht.list
    -- , psiblastPssmsAll    -- num pssm.list faa.list -> bht
    -- , psiblastPssmsDb     -- num pssm.list pdb      -> bht
    -- , psiblastPssmsDbEach -- num pssm.list pdb.list -> bht.list

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
rPsiblastBase :: Bool -> [String] -> RulesFn
rPsiblastBase writingPssm args st@(_, cfg, ref) expr@(CutFun _ _ _ _ [e, q, db]) = do
  (ExprPath ePath' ) <- rExpr st e
  (ExprPath qPath' ) <- rExpr st q
  (ExprPath dbPath') <- rExpr st db
  let ePath  = toCutPath cfg ePath'
      qPath  = toCutPath cfg qPath'
      dbPath = toCutPath cfg dbPath'
      oPath  = exprPath st expr
      oPath' = debugRules cfg "rPsiblast" expr $ fromCutPath cfg oPath
  oPath' %> \_ -> aPsiblastBase writingPssm args cfg ref oPath ePath qPath dbPath
  return (ExprPath oPath')
rPsiblastBase _ _ _ _ = error "bad argument to rPsiblast"

-- Base rules for running psiblast with one query and a list of subjects
-- to get a list of hit tables or pssms
rPsiblastBaseEach :: Bool -> [String] -> RulesFn
rPsiblastBaseEach w args = rVectorize 3 aPsiblastHack
  where
    aPsiblastHack cfg ref [o,e,q,d] = aPsiblastBase w args cfg ref o e q d
    aPsiblastHack _ _ _ = error "bad argument to rPsiblastBaseEach"

-- All the other functions eventually call this one or more times after some
-- prep work. It runs psiblast with an evalue, query, and subject and returns a
-- pssm or blast hits table depending on the args
aPsiblastBase :: Bool -> [String] -> CutConfig -> Locks
              -> CutPath -> CutPath -> CutPath -> CutPath
              -> Action ()
aPsiblastBase writingPssm args cfg ref oPath ePath qPath dbPath = do

  let oPath'  = fromCutPath cfg oPath
      tPath'  = if writingPssm then oPath' <.> "tmp" else oPath' -- see below
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
        ["-query", qPath', "-evalue", eDec, "-db", dbPre'] ++ args ++ [tPath']
        -- , "-num_threads", "8"    -- TODO add this in the wrapper script
        -- , "-out", undefined      -- TODO include this?
        -- , "-out_pssm", undefined -- TODO include this?

  -- make sure to get the exb version instead of whatever the system assumes
  -- TODO is this needed, or will it end up the default?
  psiblastBin <- fmap stripWhiteSpace $
                   wrappedCmdOut cfg ref [] [] [] "which" ["psiblast"]
  debugL cfg $ "psiblast binary: " ++ psiblastBin

  let oPath'' = debugA cfg "aPsiblastTrainDb" oPath' [eDec, qPath', dbPath']
  wrappedCmdWrite cfg ref tPath'
    [dbPre' ++ ".*"]        -- inPtns TODO is this right?
    []                      -- extra outPaths to lock TODO more -out stuff?
    [AddEnv "BLASTDB" cDir] -- opts TODO Shell? more specific cache?
    psiblastBin args'       -- TODO package and find psiblast-exb (in wrapper?)

  {- By default PSSMs get a blank first line, but I figured out that you can
   - also put a sequence ID there and it will use it in place of the annoying
   - "Query_1" in hit tables. So here we write the PSSM to a tempfile, replace
   - the first line, then write that to the final outfile.
   - (If we aren't writing a PSSM, then tPath' is already the final file)
   -}
  when writingPssm $ do
    querySeqId <- fmap (head . lines) $ readFile' qPath'
    pssmLines  <- fmap         lines  $ readFile' tPath'
    let dbName     = takeFileName dbPre'
        queryInfo  = unwords [querySeqId, "(trained on " ++ dbName ++ ")"]
        pssmWithId = queryInfo : tail pssmLines
    writeCachedLines cfg ref oPath'' pssmWithId
    liftIO $ removeFile tPath'

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
rPsiblast st (CutFun _ salt _ _ [e, q, s]) = rExpr st expr
  where
    ss   = CutList faa salt (depsOf s ) [s]
    db   = CutFun  pdb salt (depsOf ss) "makeblastdb_prot" [ss]
    expr = CutFun  bht salt (depsOf db) "psiblast_db" [e, q, db]
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
rPsiblastEach st expr@(CutFun _ n _ _ [e, q, ss]) = rExpr st pssms
  where
    sss   = CutList (ListOf faa ) n (depsOf ss  ) [ss]
    dbs   = CutFun  (ListOf pdb ) n (depsOf sss ) "makeblastdb_prot_each"  [sss]
    pssms = CutFun  (ListOf pssm) n (depsOf expr) "psiblast_db_each" [e, q, dbs]
rPsiblastEach _ _ = error "bad argument to rPsiblastEach"

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
rPsiblastAll st (CutFun _ salt _ _ [e, fa, fas]) = rExpr st expr
  where
    db   = CutFun pdb salt (depsOf fas ) "makeblastdb_prot" [fas]
    expr = CutFun bht salt (depsOf expr) "psiblast_db" [e, fa, db]
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
rPsiblastDb st expr@(CutFun _ salt _ _ [e, fa, db]) = rExpr st expr'
  where
    query = CutFun pssm salt (depsOf expr ) "psiblast_train_db" [e, fa   , db]
    expr' = CutFun bht  salt (depsOf query) "psiblast_pssm_db"  [e, query, db]
rPsiblastDb _ _ = error "bad argument to rPsiblastDb"

psiblastDbEach :: CutFunction
psiblastDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rPsiblastBaseEach False searchArgs
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
rPsiblastTrainEach st expr@(CutFun _ n _ _ [e, q, ss]) = rExpr st pssms
  where
    sss   = CutList (ListOf faa ) n (depsOf ss  ) [ss]
    dbs   = CutFun  (ListOf pdb ) n (depsOf sss ) "makeblastdb_prot_each"  [sss]
    pssms = CutFun  (ListOf pssm) n (depsOf expr) "psiblast_train_db_each" [e, q, dbs]
rPsiblastTrainEach _ _ = error "bad argument to rPsiblastTrainEach"

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
  , fRules     = rPsiblastBase True trainingArgs
  }
  where
    name = "psiblast_train_db"

psiblastTrainDbEach :: CutFunction
psiblastTrainDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf pssm)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf pssm)
  , fFixity    = Prefix
  , fRules     = rPsiblastBaseEach True trainingArgs
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

rPsiblastPssmEach :: RulesFn
rPsiblastPssmEach st expr@(CutFun _ n _ _ [e, q, ss]) = rExpr st pssms
  where
    sss   = CutList (ListOf faa ) n (depsOf ss  ) [ss]
    dbs   = CutFun  (ListOf pdb ) n (depsOf sss ) "makeblastdb_prot_each" [sss]
    pssms = CutFun  (ListOf pssm) n (depsOf expr) "psiblast_pssm_db_each" [e, q, dbs]
rPsiblastPssmEach _ _ = error "bad argument to rPsiblastPssmEach"

searchArgs :: [String]
searchArgs = ["-outfmt", "6", "-out"]

psiblastPssmDb :: CutFunction
psiblastPssmDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, pdb] bht
  , fTypeDesc  = mkTypeDesc name  [num, pssm, pdb] bht
  , fFixity    = Prefix
  , fRules     = rPsiblastBase False searchArgs
  }
  where
    name = "psiblast_pssm_db"

psiblastPssmDbEach :: CutFunction
psiblastPssmDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf pdb] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rPsiblastBaseEach False searchArgs
  }
  where
    name = "psiblast_pssm_db_each"
