module ShortCut.Modules.PsiBlast where

-- TODO write more detailed help descriptions for each function
-- TODO incorporate deltablast too?
-- TODO checkpoint PSSM type? not unless needed
-- TODO would be nice to name the pssms so it doesnt just say "Query_1"

-- TODO _all functions that just concatenate the hit tables?
--      not unless I can figure out how to do it without misleading evalues

-- TODO functions that combine multiple dbs using blast_aliastool?
--      psiblastDbAll, psiblastPssmDbAll, psiblastTrainDbAll...

-- TODO automatically add fn name to type signatures rather than passing each time

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Actions       (readLit, readPath, wrappedCmdOut,
                                    wrappedCmdWrite, debugL, debugA, debugNeed,
                                    writeCachedLines, debugTrackWrite)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rExpr)
import ShortCut.Core.Paths         (fromCutPath, cacheDir)
import ShortCut.Core.Util          (stripWhiteSpace)
import ShortCut.Modules.BlastDB    (pdb)
import ShortCut.Modules.Blast      (bht)
import ShortCut.Modules.SeqIO      (faa)
import Data.Scientific             (formatScientific, FPFormat(..))
import System.FilePath             ((<.>), takeFileName)
import System.Directory            (removeFile)
import Control.Monad               (when)
import ShortCut.Core.Compile.Map   (rFun3, map2of3, map3of3)
import ShortCut.Modules.SeqIO      (mkConcat)
import ShortCut.Core.Compile.Compose (compose1)

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

    -- TODO test/practice mapping with psiblastEach
  
    -- working base cases (no helper fns used):
    [ psiblastTrainDb      -- num faa      pdb      -> pssm
    , psiblastPssmDb       -- num pssm     pdb      -> bht

    -- working with a mapNofM and no expr editing:
    , psiblastTrainDbEach  -- num faa       pdb.list -> pssm.list
    , psiblastTrainPssmsDb -- num faa.list  pdb      -> pssm.list
    , psiblastPssmDbEach   -- num pssm      pdb.list -> bht.list
    , psiblastEachPssmDb   -- num pssm.list pdb      -> bht.list
    , psiblastEachPssm     -- num pssm.list faa      -> bht.list

    -- working with compose1 of a working fn above + concat
    , psiblastPssmsDb  -- num pssm.list pdb -> bht
    , psiblastPssmsAll -- num pssm.list faa -> bht

    -- working except when withPdbSubject broken:
    , psiblastTrain        -- num faa      faa      -> pssm
    , psiblastTrainPssms   -- num faa.list faa      -> pssm.list
    , psiblastTrainAll     -- num faa      faa.list -> pssm
    , psiblastPssm         -- num pssm     faa      -> bht
    , psiblastPssmAll      -- num pssm     faa.list -> bht
    , psiblast             -- num faa      faa      -> bht

    -- not working, reason unknown but use withPdbSubjects:
    , psiblastEach         -- num faa  faa.list -> bht.list
    , psiblastPssmEach     -- num pssm faa.list -> bht.list
    , psiblastTrainEach    -- num faa  faa.list -> pssm.list TODO fix accidental singleton

    -- not working, reason unknown but use withPssmQuery:
    , psiblastDb           -- num faa      pdb      -> bht
    , psiblastDbEach       -- num faa      pdb.list -> bht.list

    -- not working, probably because psiblastEach doesn't
    , psiblastAll             -- num faa       faa.list -> bht

    -- not written yet (may not be needed):
    -- , psiblastPssms        -- num pssm.list faa      -> bht.list TODO write/fix
    -- , psiblastPssmsBothVec -- num pssm.list faa.list -> bht.list.list
    -- , psiblastPssmsEach    -- num pssm.list faa.list -> bht.list
    -- , psiblastPssmsAll     -- num pssm.list faa.list -> bht
    -- , psiblastPssmsDbEach  -- num pssm.list pdb.list -> bht.list
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

-- Train a PSSM on a blast database
aPsiblastTrainDb :: Action3
aPsiblastTrainDb = aPsiblastDb True trainingArgs

-- Psiblast search with a PSSM against a BLAST database
aPsiblastSearchDb :: Action3
aPsiblastSearchDb = aPsiblastDb False searchArgs

-- Base action for running psiblast. Use aPsiblastTrainDb to train a PSSM, or
-- aPsiblastSearchDb to search with an existing PSSM.
aPsiblastDb :: Bool -> [String] -> Action3
aPsiblastDb writingPssm args cfg ref oPath ePath qPath dbPath = do

  let oPath'  = fromCutPath cfg oPath
      tPath'  = if writingPssm then oPath' <.> "tmp" else oPath' -- see below
      ePath'  = fromCutPath cfg ePath
      qPath'  = fromCutPath cfg qPath -- might be a fasta or pssm
      dbPath' = fromCutPath cfg dbPath
  debugNeed cfg "aPsiblastDb" [ePath', qPath', dbPath']

  eStr  <- readLit  cfg ref ePath'  -- TODO is converting to decimal needed?

  -- TODO is there something wrong with the map handlign here? or general makeblastdb?
  -- dbPrePath <- readPath cfg ref dbPath' -- TODO is this right?
  -- let dbPrePath' = fromCutPath cfg dbPrePath

  -- this version works for withPdbSubject, but breaks something else?
  dbPre     <- readPath cfg ref dbPath' -- TODO is this right?
  debugL cfg $ "aPsiblastDb dbPre: " ++ show dbPre

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
  -- psiblastBin <- fmap stripWhiteSpace $
  --                  wrappedCmdOut cfg ref [] [] [] "which" ["psiblast"]
  -- debugL cfg $ "psiblast binary: " ++ psiblastBin

  -- TODO what to do when no hits found? use seqid + nothing as output format
  --      and check for that in the search fn?

  let oPath'' = debugA cfg "aPsiblastDb" oPath' [eDec, qPath', dbPath']
  wrappedCmdWrite cfg ref tPath'
    [dbPre' ++ ".*"]        -- inPtns TODO is this right?
    []                      -- extra outPaths to lock TODO more -out stuff?
    [AddEnv "BLASTDB" cDir] -- opts TODO Shell? more specific cache?
    -- psiblastBin args'
    "psiblast" args'

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
    querySeqId <- fmap (head . lines) $ readFile' qPath'
    pssmLines  <- fmap (tail . lines) $ readFile' tPath'
    let dbName     = takeFileName dbPre'
        queryInfo  = unwords [querySeqId, "(trained on " ++ dbName ++ ")"]
        pssmWithId = queryInfo : pssmLines
    writeCachedLines cfg ref oPath'' pssmWithId
    -- TODO put back? liftIO $ removeFile tPath'

----------------------------------------------
-- helpers that make blast dbs and/or pssms --
----------------------------------------------

-- Wrap the 3rd arg of a function call in makeblastdb_prot_each
-- TODO do the first arg in BlastDB.hs and import here?
-- TODO fix passing dbprefix as db itself
withPdbSubjects :: CutExpr -> CutExpr
withPdbSubjects (CutFun rtn salt deps name [a1, a2, fas])
  =             (CutFun rtn salt deps name [a1, a2, dbs])
  where
    fass = CutList (typeOf fas) salt (depsOf fas) [fas]
    -- TODO fix this using _all :D
    dbs  = CutFun  (ListOf pdb) salt (depsOf fass) "makeblastdb_prot_each" [fass]
withPdbSubjects e = error $ "bad argument to withPdbSubjects: " ++ show e

singleton :: CutExpr -> CutExpr
singleton e = CutList (typeOf e) (saltOf e) (depsOf e) [e]

-- Wraps a single faa or an faa.list in makeblastdb_prot
withPdbSubject :: CutExpr -> CutExpr
withPdbSubject (CutFun rtn salt deps name [a1, a2, x ])
  =            (CutFun rtn salt deps name [a1, a2, db])
  where
    fas = case typeOf x of
            (ListOf _) -> x -- no need to wrap since already a list
            _          -> singleton x
    -- TODO fix this using regular or _all?
    db  = CutFun  (ListOf pdb) salt (depsOf fas) "makeblastdb_prot" [fas]
withPdbSubject e = error $ "bad argument to withPdbSubject: " ++ show e

-- TODO remove this, or withPdbSubject?
-- withProtDB :: CutExpr -> CutExpr
-- withProtDB (CutFun rtn salt deps name [a1, a2, fa])
--   =        (CutFun rtn salt deps name [a1, a2, db])
--   where
--     fas = CutList (typeOf fas) salt (depsOf fa ) [fa]
--     db  = CutFun  (ListOf pdb) salt (depsOf fas) "makeblastdb_prot" [fas]
-- withProtDB e = error $ "bad argument to withProtDB: " ++ show e

-- Wrap the faa query argument of a psiblast CutFunction in psiblast_train_db
-- TODO sometimes tries to use path to path of db as path to db... where to fix?
withPssmQuery :: CutExpr -> CutExpr
withPssmQuery (CutFun rtn salt deps name [n, q, s])
  =           (CutFun rtn salt deps name [n, p, s])
  where
    p = CutFun pssm salt deps "psiblast_train_db" [n, q, s]
withPssmQuery e = error $ "bad argument to withPssmQuery: " ++ show e

-- Wrap the faa query argument of a psiblast CutFunction in psiblast_train_db
-- TODO is this needed? it certainly doesn't work without that fn existing yet
withPssmQueries :: CutExpr -> CutExpr
withPssmQueries (CutFun rtn salt deps name [n, qs, s])
  =             (CutFun rtn salt deps name [n, ps, s])
  where
    ps = CutFun (ListOf pssm) salt deps "psiblast_train_each" [n, qs, s]
withPssmQueries e = error $ "bad argument to withPssmQueries: " ++ show e

-- Wrap the 3rd arg of a function call in makeblastdb_prot
-- TODO do the first arg in BlastDB.hs and import here?
-- TODO write a non-mapped version based on this too?
-- TODO name prot or nucl properly?
-- withProtDBs :: CutExpr -> CutExpr
-- withProtDBs (CutFun rtn salt deps name [a1, a2, fas])
--   =          (CutFun rtn salt deps name [a1, a2, dbs])
--   where
--     fass = CutList (typeOf fas) salt (depsOf fas) [fas]
--     dbs  = CutFun  (ListOf pdb) salt (depsOf fass) "makeblastdb_prot_each" [fass]
-- withProtDBs e = error $ "bad argument to withProtDBs: " ++ show e


-------------------------------
-- search with fasta queries --
-------------------------------

psiblast :: CutFunction
psiblast = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, faa, faa] bht
  , fFixity    = Prefix
  -- , fRules     = rPsiblast
  , fRules     = \s e -> rFun3 aPsiblastSearchDb s $ withPssmQuery $ withPdbSubject e
  }
  where
    name = "psiblast"

psiblastEach :: CutFunction
psiblastEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] (ListOf bht)
  , fFixity    = Prefix
  -- , fRules = \s e -> rFun3 (map3of3 faa bht $ aPsiblastSearch) s (withProtDBs e)
  , fRules = \s e -> rFun3 (map3of3 faa bht $ aPsiblastSearchDb) s
                           (withPssmQuery $ withPdbSubjects e)
  }
  where
    name = "psiblast_each"

psiblastAll :: CutFunction
psiblastAll = compose1 name
  (mkTypeDesc name [num, faa, ListOf faa] bht)
  psiblastEach
  (ListOf bht)
  (mkConcat bht)
  where
    name = "psiblast_all"

-- psiblastAll :: CutFunction
-- psiblastAll = CutFunction
--   { fName      = name
--   , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] bht
--   , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] bht
--   , fFixity    = Prefix
--   , fRules     = rPsiblastAll
--   -- , fRules = rApplyConcatBht $ \s e ->
--   --     rFun3 (map3of3 faa bht aPsiblastSearch) s (withPssmQuery $ withPdbSubject e)
--   }
--   where
--     name = "psiblast_all"
-- 
-- -- TODO rewrite with new functions, but this one needs a concat
-- rPsiblastAll :: RulesFn
-- rPsiblastAll st (CutFun _ salt _ _ [e, fa, fas]) = rExpr st expr
--   where
--     db   = CutFun pdb salt (depsOf fas ) "makeblastdb_prot" [fas]
--     expr = CutFun bht salt (depsOf expr) "psiblast_db" [e, fa, db]
-- rPsiblastAll _ _ = error "bad argument to rPsiblastAll"

psiblastDb :: CutFunction
psiblastDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] bht
  , fTypeDesc  = mkTypeDesc name  [num, faa, pdb] bht
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 aPsiblastSearchDb s (withPssmQuery e)
  }
  where
    name = "psiblast_db"

-- TODO want map3of3 to read a pdb.list here and pass the individual paths,
--      but that interferes with one of the others right?
psiblastDbEach :: CutFunction
psiblastDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 (map3of3 pdb bht aPsiblastSearchDb) s (withPssmQuery e)
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
  , fRules     = \s e -> rFun3 aPsiblastTrainDb s $ withPdbSubject e
  }
  where
    name = "psiblast_train"

-- TODO better name!
psiblastTrainPssms :: CutFunction
psiblastTrainPssms = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf faa, faa] (ListOf pssm)
  , fTypeDesc  = mkTypeDesc name  [num, ListOf faa, faa] (ListOf pssm)
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 (map2of3 faa pssm aPsiblastTrainDb) s $ withPdbSubject e
  }
  where
    name = "psiblast_train_pssms"

-- TODO appears to succeed, but something is messed up about the mapping?
--      (makes a list of length 1 from multiple faa subjects)
psiblastTrainEach :: CutFunction
psiblastTrainEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] (ListOf pssm)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] (ListOf pssm)
  , fFixity    = Prefix
  , fRules = \s e -> rFun3 (map3of3 faa pssm $ aPsiblastTrainDb) s (withPdbSubjects e)
  }
  where
    name = "psiblast_train_each"

psiblastTrainAll :: CutFunction
psiblastTrainAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] pssm
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] pssm
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 aPsiblastTrainDb s (withPdbSubject e)
  }
  where
    name = "psiblast_train_all"

psiblastTrainDb :: CutFunction
psiblastTrainDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] pssm
  , fTypeDesc  = mkTypeDesc name  [num, faa, pdb] pssm
  , fFixity    = Prefix
  , fRules     = rFun3 aPsiblastTrainDb
  }
  where
    name = "psiblast_train_db"

psiblastTrainDbEach :: CutFunction
psiblastTrainDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf pssm)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf pssm)
  , fFixity    = Prefix
  , fRules     = rFun3 $ map3of3 pdb pssm aPsiblastTrainDb
  }
  where
    name = "psiblast_train_db_each"

psiblastTrainPssmsDb :: CutFunction
psiblastTrainPssmsDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf faa, pdb] (ListOf pssm)
  , fTypeDesc  = mkTypeDesc name  [num, ListOf faa, pdb] (ListOf pssm)
  , fFixity    = Prefix
  , fRules     = rFun3 $ map2of3 faa pssm aPsiblastTrainDb
  }
  where
    name = "psiblast_train_pssms_db"

---------------------------------------
-- search with explicit pssm queries --
---------------------------------------

psiblastPssm :: CutFunction
psiblastPssm = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, pssm, faa] bht
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 aPsiblastSearchDb s $ withPdbSubject e
  }
  where
    name = "psiblast_pssm"

psiblastPssmAll :: CutFunction
psiblastPssmAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf faa] bht
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 aPsiblastSearchDb s $ withPdbSubject e
  }
  where
    name = "psiblast_pssm_all"

psiblastPssmEach :: CutFunction
psiblastPssmEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf faa] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf faa] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 (map3of3 faa bht $ aPsiblastSearchDb) s (withPdbSubjects e)
  }
  where
    name = "psiblast_pssm_each"

searchArgs :: [String]
searchArgs = ["-outfmt", "6", "-out"]

psiblastPssmDb :: CutFunction
psiblastPssmDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, pdb] bht
  , fTypeDesc  = mkTypeDesc name  [num, pssm, pdb] bht
  , fFixity    = Prefix
  , fRules     = rFun3 aPsiblastSearchDb
  }
  where
    name = "psiblast_pssm_db"

psiblastPssmDbEach :: CutFunction
psiblastPssmDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf pdb] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rFun3 $ map3of3 pdb bht aPsiblastSearchDb
  }
  where
    name = "psiblast_pssm_db_each"

---------------------------------------
-- search with lists of pssm queries --
---------------------------------------

-- TODO better name? this one's pretty bad!
-- TODO would a user ever want to use this one directly?
psiblastEachPssmDb :: CutFunction
psiblastEachPssmDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf pssm, pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, ListOf pssm, pdb] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rFun3 $ map2of3 pssm bht $ aPsiblastSearchDb
  }
  where
    name = "psiblast_each_pssm_db"

psiblastPssmsDb :: CutFunction
psiblastPssmsDb = compose1 name
  (mkTypeDesc name [num, ListOf pssm, pdb] bht)
  psiblastEachPssmDb
  (ListOf bht)
  (mkConcat bht)
  where
    name = "psiblast_pssms_db"

psiblastEachPssm :: CutFunction
psiblastEachPssm = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf pssm, faa] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, ListOf pssm, faa] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 (map2of3 pssm bht aPsiblastSearchDb) s (withPdbSubject e)
  }
  where
    name = "psiblast_each_pssm"

-- TODO write this!
-- psiblastPssms :: CutFunction
-- psiblastPssms = compose1 name
--   (mkTypeDesc name [num, ListOf pssm, faa] bht)
--   psiblastEachPssm
--   (ListOf bht)
--   (mkConcat bht)
--   where
--     name = "psiblast_pssms"

psiblastPssmsAll :: CutFunction
psiblastPssmsAll = compose1 name
  (mkTypeDesc name [num, ListOf pssm, faa] bht)
  psiblastEachPssm
  (ListOf bht)
  (mkConcat bht)
  where
    name = "psiblast_pssms_all"


-- num pssm.list faa -> bht      TODO rewrite after rPsiblastPssmsDb
-- psiblastPssms :: CutFunction
-- psiblastPssms = CutFunction
--   { fName      = name
--   , fTypeCheck = defaultTypeCheck [num, ListOf pssm, faa] bht
--   , fTypeDesc  = mkTypeDesc name  [num, ListOf pssm, faa] bht
--   , fFixity    = Prefix
--   -- , fRules     = rPsiblastPssms
--   }
--   where
--     name = "psiblast_pssms"

-- TODO rewrite with new functions
-- TODO deduplicate this from rPsiblastPssm
-- rPsiblastPssms :: RulesFn
-- rPsiblastPssms st expr@(CutFun _ salt _ _ [e, qs, fa]) = rExpr st expr'
--   where
--     fas   = CutList pdb salt (depsOf fa  ) [fa]
--     db    = CutFun  pdb salt (depsOf fas ) "makeblastdb_prot" [fas]
--     expr' = CutFun  bht salt (depsOf expr) "psiblast_pssms_db" [e, qs, db]
-- rPsiblastPssms _ _ = error "bad argument to rPsiblastPssms"
