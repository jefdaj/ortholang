module OrthoLang.Modules.PsiBlast where

-- TODO simplify by using explicit psiblast_train and psiblast_search everywhere?

-- TODO write more detailed help descriptions for each function
-- TODO incorporate deltablast too?
-- TODO checkpoint PSSM type? not unless needed

-- TODO functions that combine multiple dbs using blast_aliastool?
--      psiblastDbAll, psiblastPssmDbAll, psiblastTrainDbAll...

-- TODO automatically add fn name to type signatures rather than passing each time

import Development.Shake

import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Modules.BlastDB    (pdb)
import OrthoLang.Modules.Blast      (bht)
import OrthoLang.Modules.SeqIO      (mkConcat, faa)
import OrthoLang.Modules.Singletons (withSingleton)

import Control.Monad             (when)
import Data.Scientific           (formatScientific, FPFormat(..))
import System.Directory          (createDirectoryIfMissing)
import System.Exit               (ExitCode(..))
import System.FilePath           ((<.>), takeDirectory, takeFileName)
import Data.Maybe (fromJust)

dbg :: String -> String -> Action ()
dbg name = debugA ("ortholang.modules.psiblast." ++ name)

olModule :: Module
olModule = Module
  -- TODO move doc to its file
  { mName = "PsiBLAST"
  , mDesc = "Iterated PsiBLAST (BLAST+) searches using position-specific substitution matrixes.\n\
            \\n\
            \There are a lot of these! Some naming conventions:\n\
            \\n\
            \* A fn with `train` trains and returns one or more pssms ; one without\n\
            \`train` runs a regular blast search and returns hits.\n\
            \\n\
            \* A fn with `db` takes one or more blast databases directly; one without\n\
            \`db` auto-builds the db(s) from one or more fastas.\n\
            \\n\
            \* A fn with `all` takes a list of fastas and creates one db from it.\n\
            \\n\
            \* A fn with `each` maps over its last argument. The difference between\n\
            \`each` and `all` is that `each` returns a list of results, whereas `all`\n\
            \summarizes them into one thing.\n\
            \\n\
            \* A fn with `pssms` (plural) takes a list of pssm queries and combines\n\
            \their hits into one big table.\n\
            \\n\
            \So for example...\n\n\
            \\n\
            \```\n\
            \psiblast_train_all : num faa faa.list -> pssm\n\
            \  auto-builds one blast db from a list of fasta files\n\
            \  trains a pssm for the query fasta on it\n\
            \  returns the pssm\n\
            \```\n\
            \\n\
            \```\n\
            \psiblast_each : num faa faa.list -> bht.list\n\
            \  auto-builds one db per subject fasta\n\
            \  trains a pssm for the query fasta against each one\n\
            \  runs a final psiblast search against each one using the pssm\n\
            \  returns a list of hit tables\n\
            \```\n\
            \\n\
            \TODO individual help descriptions for each fn"

  , mTypes = [faa, bht, pssm, pdb]
  , mGroups = []
  , mEncodings = []
  , mRules = []
  , mFunctions =

    -- runs without obvious errors, but needs more careful verification:
    [ psiblast             -- num faa       faa      -> bht
    , psiblastAll          -- num faa       faa.list -> bht (TODO test broken?)
    , psiblastDb           -- num faa       pdb      -> bht
    , psiblastDbEach       -- num faa       pdb.list -> bht.list
    , psiblastEach         -- num faa       faa.list -> bht.list (TODO test broken?)
    , psiblastEachPssm     -- num pssm.list faa      -> bht.list (TODO fix broken)
    , psiblastEachPssmDb   -- num pssm.list pdb      -> bht.list
    , psiblastPssm         -- num pssm      faa      -> bht
    , psiblastPssmAll      -- num pssm      faa.list -> bht
    , psiblastPssmDb       -- num pssm      pdb      -> bht
    , psiblastPssmDbEach   -- num pssm      pdb.list -> bht.list
    , psiblastPssmEach     -- num pssm      faa.list -> bht.list

    -- TODO these are all duplicated?
    , psiblastPssms        -- num pssm.list faa      -> bht
    , psiblastPssmsAll     -- num pssm.list faa      -> bht
    -- , psiblastPssmsEach    -- num pssm.list faa.list -> bht.list

    , psiblastPssmsDb      -- num pssm.list pdb      -> bht
    , psiblastTrain        -- num faa       faa      -> pssm
    , psiblastTrainAll     -- num faa       faa.list -> pssm
    , psiblastTrainDb      -- num faa       pdb      -> pssm

    -- TODO remove all these if using multi-pssm files?
    --      or just rewrite the fn signatures to have .pssms or something?
    , psiblastTrainDbEach  -- num faa       pdb.list -> pssm.list
    , psiblastTrainEach    -- num faa       faa.list -> pssm.list
    , psiblastTrainPssms   -- num faa.list  faa      -> pssm.list
    , psiblastTrainPssmsDb -- num faa.list  pdb      -> pssm.list

    -- not written yet (may not be needed):
    -- , psiblastPssmsBothMap -- num pssm.list faa.list -> bht.list.list
    -- , psiblastPssmsAll     -- num pssm.list faa.list -> bht
    -- , psiblastPssmsDbEach  -- num pssm.list pdb.list -> bht.list
   ]
  }

pssm :: Type
pssm = Type
  { tExt  = "pssm"
  , tDesc = "PSI-BLAST position-specific substitution matrix as ASCII"
  , tShow  = defaultShow
  }

--------------------
-- base functions --
--------------------

-- Train a PSSM on a blast database
aPsiblastTrainDb :: NewAction3
aPsiblastTrainDb = aPsiblastDb True trainingArgs

aPsiblastSearchDb :: NewAction3
aPsiblastSearchDb = aPsiblastDb False searchArgs

-- TODO remove
-- aPsiblastDb' :: Bool -> [String] -> [Path] -> Action ()
-- aPsiblastDb' writingPssm args [oPath, ePath,  qPath, dbPath] = do
--   cfg <- fmap fromJust getShakeExtra
--   let loc = "modules.psiblast.aPsiblastDb'"
--       oPath'  = fromPath loc cfg oPath
--       ePath'  = fromPath loc cfg ePath
--       qPath'  = fromPath loc cfg qPath -- might be a fasta or pssm
--       dbPath' = fromPath loc cfg dbPath
--   aPsiblastDb writingPssm args (ExprPath oPath') ePath' qPath' dbPath'
-- aPsiblastDb' _ _ _ = fail "bad argument to aPsiblastDb'"

-- aPsiblastTrainDb' :: [Path] -> Action ()
-- aPsiblastTrainDb' = aPsiblastDb' True  trainingArgs

-- aPsiblastSearchDb' :: [Path] -> Action ()
-- aPsiblastSearchDb' = aPsiblastDb' False searchArgs

-- Base action for running psiblast. Use aPsiblastTrainDb to train a PSSM, or
-- aPsiblastSearchDb to search with an existing PSSM.
aPsiblastDb :: Bool -> [String] -> NewAction3
aPsiblastDb writingPssm args (ExprPath oPath') ePath' qPath' dbPath' = do

  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.psiblast.aPsiblastDb"
  -- let oPath'  = fromPath loc cfg oPath
      tPath'  = if writingPssm then oPath' <.> "tmp" else oPath' -- see below
      -- ePath'  = fromPath loc cfg ePath
      -- qPath'  = fromPath loc cfg qPath -- might be a fasta or pssm
      -- dbPath' = fromPath loc cfg dbPath
  need' loc [ePath', qPath', dbPath']

  eStr  <- readLit loc ePath'  -- TODO is converting to decimal needed?

  -- TODO is there something wrong with the map handlign here? or general makeblastdb?
  -- dbPrePath <- readPath dbPath' -- TODO is this right?
  -- let dbPrePath' = fromPath loc cfg dbPrePath

  -- this version works for withPdbSubject, but breaks something else?
  dbPre     <- readPath loc dbPath' -- TODO is this right?
  dbg "aPsiblastDb" $ "dbPre: " ++ show dbPre

  let eDec = formatScientific Fixed Nothing $ read eStr
      cDir = fromPath loc cfg $ cacheDir cfg "psiblast"
      dbPre' = fromPath loc cfg dbPre

        -- , "-num_threads", "8"    -- TODO add this in the wrapper script
        -- , "-out", undefined      -- TODO include this?
        -- , "-out_pssm", undefined -- TODO include this?

  -- make sure to get the exb version instead of whatever the system assumes
  -- TODO is this needed, or will it end up the default?
  -- psiblastBin <- fmap stripWhiteSpace $
  --                  wrappedCmdOut True cfg ref [] [] [] "which" ["psiblast"]
  -- dbg "aPsiblastDb" $ "binary: " ++ psiblastBin

  -- TODO what to do when no hits found? use seqid + nothing as output format
  --      and check for that in the search fn?

  -- TODO why would this be needed anyway?
  liftIO $ createDirectoryIfMissing True cDir

  -- before running psiblast, check whether the query is an empty pssm
  -- and if so, just return empty hits immediately
  lines2 <- fmap (take 2 . lines) $ readFileStrict' qPath'
  if (not writingPssm) && (length lines2 > 1) && (last lines2 == "<<emptypssm>>")
    then writeCachedLines loc oPath' ["<<emptybht>>"]
    else do

      -- TODO need Cwd here too, or maybe instead?
      let oPath'' = traceA loc oPath' [eDec, qPath', dbPath']

      runCmd $ CmdDesc
        { cmdBinary = "psiblast.sh"
        , cmdInPatterns = [dbPre' ++ ".*"]
        , cmdNoNeedDirs = []
        , cmdExtraOutPaths = []
        , cmdSanitizePaths = []
        , cmdOptions = []
        , cmdArguments = [tPath', cDir, qPath', eDec, dbPre'] ++ args
        , cmdParallel = False -- TODO true, but have to fix first
        , cmdFixEmpties = True
        , cmdExitCode = ExitSuccess
        , cmdOutPath = tPath' -- note that it isn't the final outpath
        , cmdRmPatterns = [dbPre' ++ "*"]
        }
    
      -- TODO instead of wrappedCmdWrite, check explicitly for tPath'
      --      and write a "no hits, empty pssm" message if needed
      -- TODO actually, do that as part of the if writing pssm thing below
    
      {- By default PSSMs get a blank first line, but I figured out that you can
       - also put a sequence ID there and it will use it in place of the annoying
       - "Query_1" in hit tables. So here we write the PSSM to a tempfile, replace
       - the first line, then write that to the final outfile.
       - (If we aren't writing a PSSM, then tPath' is already the final file)
       -}
      when writingPssm $ do
        let head' = headOrDie "aPsiblastDb failed to read querySeqId"
        querySeqId <- fmap (head' . words . head' . lines) $ readFileStrict' qPath'
        pssmLines <- fmap lines $ readFileStrict' tPath'
        let pssmLines' = if null pssmLines then ["<<emptypssm>>"] else tail pssmLines
            dbName     = takeFileName $ takeDirectory dbPre'
            trainInfo  = "(trained on " ++ dbName ++ " with e-value cutoff " ++ eStr ++ ")"
            queryInfo  = unwords [querySeqId, trainInfo]
            pssmWithId = queryInfo : pssmLines'
        writeCachedLines loc oPath'' pssmWithId
        -- liftIO $ removeFile tPath'
    
----------------------------------------------
-- helpers that make blast dbs and/or pssms --
----------------------------------------------

-- Wrap the 3rd arg of a function call in makeblastdb_faa_each
-- TODO do the first arg in BlastDB.hs and import here?
-- TODO fix passing dbprefix as db itself
withPdbSubjects :: Expr -> Expr
withPdbSubjects (Fun rtn seed deps name [a1, a2, xs ])
  =             (Fun rtn seed deps name [a1, a2, dbs])
  where
    dbs = Fun  (ListOf pdb) seed (depsOf xs) "makeblastdb_faa_each" [xs]
withPdbSubjects e = error $ "bad argument to withPdbSubjects: " ++ show e

-- Wraps a single faa or an faa.list in makeblastdb_faa
withPdbSubject :: Expr -> Expr
withPdbSubject (Fun rtn seed deps name [a1, a2, x ])
  =            (Fun rtn seed deps name [a1, a2, db])
  where
    db  = Fun  (ListOf pdb) seed (depsOf fas) "makeblastdb_faa_all" [fas]
    fas = case typeOf x of
            (ListOf _) -> x -- no need to wrap since already a list
            _          -> withSingleton x
withPdbSubject e = error $ "bad argument to withPdbSubject: " ++ show e

-- Wrap the faa query argument of a psiblast Function in psiblast_train_db
-- TODO sometimes tries to use path to path of db as path to db... where to fix?
withPssmQuery :: Expr -> Expr
withPssmQuery (Fun rtn seed deps name [n, q, s])
  =           (Fun rtn seed deps name [n, p, s])
  where
    p = Fun pssm seed deps "psiblast_train_db" [n, q, s]
withPssmQuery e = error $ "bad argument to withPssmQuery: " ++ show e

-------------------------------
-- search with fasta queries --
-------------------------------

-- psiblast :: Function
-- psiblast = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly faa, Exactly faa]
--   , fOutput = Exactly bht
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> rFun3 aPsiblastSearchDb s $ withPssmQuery $ withPdbSubject e
--   }
--   where
--     name = "psiblast"

psiblast :: Function
psiblast = newExprExpansion
  "psiblast"
  [Exactly num, Exactly faa, Exactly faa]
  (Exactly bht)
  mPsiblast
  [Nondeterministic]

mPsiblast :: ExprExpansion
mPsiblast _ _ (Fun r ms ds _ [e, qFa, sFa]) = Fun r ms ds "psiblast_pssm_db" [e, qPssm, sDb]
  where
    qPssm = Fun pssm ms (depsOf qFa) "psiblast_train_db" [e, qPssm, sDb]
    sDb   = Fun pdb  ms (depsOf sFa) "makeblastdb_faa" [sDb]
mPsiblast _ _ e = error "ortholang.modules.psiblast.mPsiblast" $ "bad argument: " ++ show e

psiblastEach :: Function
psiblastEach = newFnA3
  "psiblast_each"
  (Exactly num, Exactly faa, Exactly $ ListOf faa)
  (Exactly $ ListOf bht)
  (newMap3of3 "psiblast")
  [Nondeterministic]

-- TODO does this still work? is it worth keeping, or just rewrite anyway?
psiblastAll :: Function
psiblastAll = compose1 "psiblast_all" [Nondeterministic] psiblastEach (mkConcat bht) -- TODO name the mkConcat?

-- psiblastDb :: Function
-- psiblastDb = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly faa, Exactly pdb]
--   , fOutput = Exactly bht
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> rFun3 aPsiblastSearchDb s (withPssmQuery e)
--   }
--   where
--     name = "psiblast_db"

-- | Search variant that takes a FASTA query and blast DB, and trains a query PSSM before the search
psiblastDb :: Function
psiblastDb = newExprExpansion
  "psiblast_db"
  [Exactly num, Exactly faa, Exactly pdb]
  (Exactly bht)
  mPsiblastDb
  [Nondeterministic]

mPsiblastDb :: ExprExpansion
mPsiblastDb _ _ (Fun r ms ds _ [e, qFa, sDb]) = Fun r ms ds "psiblast_pssm_db" [e, qPssm, sDb]
  where
    qPssm = Fun pssm ms (depsOf qFa) "psilbast_train_db" [e, qPssm, sDb]
mPsiblastDb _ _ e = error "ortholang.modules.psiblast.mPsiblastDb" $ "bad argument: " ++ show e

-- TODO want map3of3 to read a pdb.list here and pass the individual paths,
--      but that interferes with one of the others right?
-- wait can psiblast just take a faa and pdb directly?
-- psiblastDbEach :: Function
-- psiblastDbEach = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly faa, Exactly (ListOf pdb)]
--   , fOutput = Exactly (ListOf bht)
--   , fTags = [Nondeterministic]
--   -- can't use withPssmQuery here because there's a list of things to train against
--   -- but won't aPsiblastDb default to working with this anyway? (not typechecked that way tho)
--   , fNewRules = NewNotImplemented, fOldRules = rMap 3 aPsiblastSearchDb'
--   }
--   where
--     name = "psiblast_db_each"

psiblastDbEach :: Function
psiblastDbEach = newFnA3
  "psiblast_db_each"
  (Exactly num, Exactly faa, Exactly $ ListOf pdb)
  (Exactly bht)
  (newMap3of3 "psiblast_db")
  [Nondeterministic]

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

-- psiblastTrain :: Function
-- psiblastTrain = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly faa, Exactly faa]
--   , fOutput = Exactly pssm
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> rFun3 aPsiblastTrainDb s $ withPdbSubject e
--   }
--   where
--     name = "psiblast_train"

psiblastTrain :: Function
psiblastTrain = newFnA3
  "psiblast_train"
  (Exactly num, Exactly faa, Exactly faa)
  (Exactly pssm)
  aPsiblastTrainDb -- TODO remove db from name?
  [Nondeterministic]


-- TODO better name!
-- psiblastTrainPssms :: Function
-- psiblastTrainPssms = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly (ListOf faa), Exactly faa]
--   , fOutput = Exactly (ListOf pssm)
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> (rMap 2 aPsiblastTrainDb') s $ withPdbSubject e
--   }
--   where
--     name = "psiblast_train_pssms"

psiblastTrainPssms :: Function
psiblastTrainPssms = newExprExpansion
  "psiblast_train_pssms"
  [Exactly num, Exactly (ListOf faa), Exactly faa]
  (Exactly $ ListOf pssm)
  undefined
  [Nondeterministic]

-- TODO appears to succeed, but something is messed up about the mapping?
--      (makes a list of length 1 from multiple faa subjects)
-- psiblastTrainEach :: Function
-- psiblastTrainEach = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly faa, Exactly (ListOf faa)]
--   , fOutput = Exactly (ListOf pssm)
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> rFun3 (map3of3 pdb pssm $ aPsiblastTrainDb) s (withPdbSubjects e)
--   }
--   where
--     name = "psiblast_train_each"

psiblastTrainEach :: Function
psiblastTrainEach = newExprExpansion
  "psiblast_train_each"
  [Exactly num, Exactly faa, Exactly $ ListOf faa]
  (Exactly $ ListOf pssm)
  undefined
  [Nondeterministic]

-- psiblastTrainAll :: Function
-- psiblastTrainAll = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly faa, Exactly (ListOf faa)]
--   , fOutput = Exactly pssm
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> rFun3 aPsiblastTrainDb s (withPdbSubject e)
--   }
--   where
--     name = "psiblast_train_all"

psiblastTrainAll :: Function
psiblastTrainAll = newFnA3
  "psiblast_train_all"
  (Exactly num, Exactly faa, Exactly $ ListOf faa)
  (Exactly pssm)
  (newMap3of3 "psiblast_train") -- TODO write in terms of train_all instead?
  [Nondeterministic]

psiblastTrainDb :: Function
psiblastTrainDb = newFnA3
  "psiblast_train_db"
  (Exactly num, Exactly faa, Exactly pdb)
  (Exactly pssm)
  aPsiblastTrainDb
  [Nondeterministic]

-- psiblastTrainDbEach :: Function
-- psiblastTrainDbEach = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly faa, Exactly (ListOf pdb)]
--   , fOutput = Exactly (ListOf pssm)
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = rFun3 $ map3of3 pdb pssm aPsiblastTrainDb
--   }
--   where
--     name = "psiblast_train_db_each"


psiblastTrainDbEach :: Function
psiblastTrainDbEach = newExprExpansion
  "psiblast_train_db_each"
  [Exactly num, Exactly faa, Exactly (ListOf pdb)]
  (Exactly $ ListOf pssm)
  undefined
  [Nondeterministic]

-- psiblastTrainPssmsDb :: Function
-- psiblastTrainPssmsDb = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly (ListOf faa), Exactly pdb]
--   , fOutput = Exactly (ListOf pssm)
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = rMap 2 aPsiblastTrainDb'
--   }
--   where
--     name = "psiblast_train_pssms_db"

psiblastTrainPssmsDb :: Function
psiblastTrainPssmsDb = newExprExpansion
  "psiblast_train_pssms_db"
  [Exactly num, Exactly (ListOf faa), Exactly pdb]
  (Exactly $ ListOf pssm)
  undefined
  [Nondeterministic]

---------------------------------------
-- search with explicit pssm queries --
---------------------------------------

-- psiblastPssm :: Function
-- psiblastPssm = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly pssm, Exactly faa]
--   , fOutput = Exactly bht
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> rFun3 aPsiblastSearchDb s $ withPdbSubject e
--   }
--   where
--     name = "psiblast_pssm"

-- TODO psiblast_search_pssm?
psiblastPssm :: Function
psiblastPssm = newExprExpansion
  "psiblast_pssm"
  [Exactly num, Exactly pssm, Exactly faa]
  (Exactly bht)
  undefined
  [Nondeterministic]

-- TODO why does this one fail? it's not even using rMap
-- psiblastPssmAll :: Function
-- psiblastPssmAll = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly pssm, Exactly (ListOf faa)]
--   , fOutput = Exactly bht
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> rFun3 aPsiblastSearchDb s $ withPdbSubject e
--   }
--   where
--     name = "psiblast_pssm_all"

psiblastPssmAll :: Function
psiblastPssmAll = newExprExpansion
  "psiblast_pssm_all"
  [Exactly num, Exactly pssm, Exactly (ListOf faa)]
  (Exactly bht)
  undefined
  [Nondeterministic]

-- psiblastPssmEach :: Function
-- psiblastPssmEach = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly pssm, Exactly (ListOf faa)]
--   , fOutput = Exactly (ListOf bht)
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> rFun3 (map3of3 pdb bht $ aPsiblastSearchDb) s (withPdbSubjects e)
--   }
--   where
--     name = "psiblast_pssm_each"

psiblastPssmEach :: Function
psiblastPssmEach = newExprExpansion
  "psiblast_pssm_each"
  [Exactly num, Exactly pssm, Exactly (ListOf faa)]
  (Exactly $ ListOf bht)
  undefined
  [Nondeterministic]

searchArgs :: [String]
searchArgs = ["-outfmt", "6", "-out"]

-- psiblastPssmDb :: Function
-- psiblastPssmDb = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = 
--   , fOutput = Exactly bht
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = rFun3 aPsiblastSearchDb
--   }
--   where
--     name = "psiblast_pssm_db"

-- | The base search function which takes an explicit PSSM and blast DB
psiblastPssmDb :: Function
psiblastPssmDb = newFnA3
  "psiblast_pssm_db"
  (Exactly num, Exactly pssm, Exactly pdb)
  (Exactly bht)
  aPsiblastSearchDb
  [Nondeterministic]

-- psiblastPssmDbEach :: Function
-- psiblastPssmDbEach = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly pssm, Exactly (ListOf pdb)]
--   , fOutput = Exactly (ListOf bht)
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = rFun3 $ map3of3 pdb bht aPsiblastSearchDb
--   }
--   where
--     name = "psiblast_pssm_db_each"

psiblastPssmDbEach :: Function
psiblastPssmDbEach = newFnA3
  "psiblast_pssm_db_each"
  (Exactly num, Exactly pssm, Exactly $ ListOf pdb)
  (Exactly $ ListOf bht)
  (newMap3of3 "psiblast_train_db") -- TODO right function here?
  [Nondeterministic]

---------------------------------------
-- search with lists of pssm queries --
---------------------------------------

-- TODO better name? this one's pretty bad!
-- TODO would a user ever want to use this one directly?
-- psiblastEachPssmDb :: Function
-- psiblastEachPssmDb = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly (ListOf pssm), Exactly pdb]
--   , fOutput = Exactly (ListOf bht)
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = rMap 2 aPsiblastSearchDb'
--   }
--   where
--     name = "psiblast_each_pssm_db"

-- TODO what should it be called?
psiblastEachPssmDb :: Function
psiblastEachPssmDb = newFnA3
  "psiblast_each_pssm_db"
  (Exactly num, Exactly (ListOf pssm), Exactly pdb)
  (Exactly $ ListOf bht)
  (newMap2of3 "psiblast_pssm_db")
  [Nondeterministic]

psiblastPssmsDb :: Function
psiblastPssmsDb = compose1 "psiblast_pssms_db" [Nondeterministic] psiblastEachPssmDb (mkConcat bht) -- TODO name the mkConcat?

-- TODO withPdbSubject fails with rMap? psiblastTrainPssms and psiblastEachPssm
-- TODO OK this is weird, why does it fail but psiblastPssms below can use it correctly?
--      specific incompatibility with withPdbSubject?
-- psiblastEachPssm :: Function
-- psiblastEachPssm = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly num, Exactly (ListOf pssm), Exactly faa]
--   , fOutput = Exactly (ListOf bht)
--   , fTags = [Nondeterministic]
--   , fNewRules = NewNotImplemented, fOldRules = \s e -> (rMap 2 aPsiblastSearchDb') s (withPdbSubject e)
--   }
--   where
--     name = "psiblast_each_pssm"

-- TODO new pattern for these names?
psiblastEachPssm :: Function
psiblastEachPssm = newFnA3
  "psiblast_each_pssm"
  (Exactly num, Exactly (ListOf pssm), Exactly faa)
  (Exactly $ ListOf bht)
  (newMap2of3 "psiblast_pssm")
  [Nondeterministic]

-- TODO wait this should return a list right? making it the same as psiblast_each_pssm?
psiblastPssms :: Function
psiblastPssms = compose1 "psiblast_pssms" [Nondeterministic] psiblastEachPssm (mkConcat bht)

-- TODO wait, this is the same as above?
psiblastPssmsAll :: Function
psiblastPssmsAll = compose1 "psiblast_pssms_all" [Nondeterministic] psiblastEachPssm (mkConcat bht)

-- TODO think how to write this!
--      it needs the effect of map3of3 psiblast_pssms_all ^
--      but can't actually do that the straightforward way
--      could it use psiblastPssmAll and concat_each?
-- TODO test this
-- TODO wait this is the same as above too??
psiblastPssmsEach :: Function
psiblastPssmsEach = compose1 "psiblast_pssms_each" [Nondeterministic] psiblastEachPssm (mkConcat bht)
