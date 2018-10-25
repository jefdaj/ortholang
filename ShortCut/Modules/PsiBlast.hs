module ShortCut.Modules.PsiBlast where

-- TODO write more detailed help descriptions for each function
-- TODO incorporate deltablast too?
-- TODO checkpoint PSSM type? not unless needed

-- TODO functions that combine multiple dbs using blast_aliastool?
--      psiblastDbAll, psiblastPssmDbAll, psiblastTrainDbAll...

-- TODO automatically add fn name to type signatures rather than passing each time

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Actions       (readLit, readPath, 
                                    wrappedCmdWrite, debugL, debugA, debugNeed,
                                    writeCachedLines)
import ShortCut.Core.Compile.Basic (defaultTypeCheck)
import ShortCut.Core.Paths         (fromCutPath, cacheDir)
import ShortCut.Modules.BlastDB    (pdb)
import ShortCut.Modules.Blast      (bht)
import ShortCut.Modules.SeqIO      (faa)
import Data.Scientific             (formatScientific, FPFormat(..))
import System.FilePath             ((<.>), takeFileName)
-- import System.Directory            (removeFile)
import Control.Monad               (when)
import ShortCut.Core.Compile.Map   (rFun3, map3of3, singleton)
import ShortCut.Modules.SeqIO      (mkConcat)
import ShortCut.Core.Compile.Compose (compose1)
import ShortCut.Core.Compile.Vectorize (rVectorize)
import System.Directory            (createDirectoryIfMissing)

cutModule :: CutModule
cutModule = CutModule
  { mName = "PsiBLAST"
  , mDesc = "PsiBLAST (BLAST+) searches using position-specific substitution matrixes"
  , mTypes = [faa, pdb, bht, pssm]
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
    -- , psiblastPssmsBothVec -- num pssm.list faa.list -> bht.list.list
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

aPsiblastDb' :: Bool -> [String] -> CutConfig -> Locks -> [CutPath] -> Action ()
aPsiblastDb' writingPssm args cfg ref [oPath, ePath,  qPath, dbPath] =
  aPsiblastDb writingPssm args cfg ref oPath ePath qPath dbPath
aPsiblastDb' _ _ _ _ _ = error "bad argument to aPsiblastDb'"

aPsiblastTrainDb'  = aPsiblastDb' True  trainingArgs
aPsiblastSearchDb' = aPsiblastDb' False searchArgs

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
      args' = ["-query", qPath', "-evalue", eDec, "-db", dbPre'] ++ args ++ [tPath']
           ++ ["&&", "touch", tPath'] -- in case no pssm created because no hits

        -- , "-num_threads", "8"    -- TODO add this in the wrapper script
        -- , "-out", undefined      -- TODO include this?
        -- , "-out_pssm", undefined -- TODO include this?

  -- make sure to get the exb version instead of whatever the system assumes
  -- TODO is this needed, or will it end up the default?
  -- psiblastBin <- fmap stripWhiteSpace $
  --                  wrappedCmdOut True cfg ref [] [] [] "which" ["psiblast"]
  -- debugL cfg $ "psiblast binary: " ++ psiblastBin

  -- TODO what to do when no hits found? use seqid + nothing as output format
  --      and check for that in the search fn?

  -- TODO why would this be needed anyway?
  liftIO $ createDirectoryIfMissing True cDir

  -- before running psiblast, check whether the query is an empty pssm
  -- and if so, just return empty hits immediately
  lines2 <- fmap (take 2 . lines) $ readFile' qPath'
  if (not writingPssm) && (length lines2 > 1) && (last lines2 == "<<emptypssm>>")
    then writeCachedLines cfg ref oPath' ["<<emptybht>>"]
    else do

      -- TODO need Cwd here too, or maybe instead?
      let oPath'' = debugA cfg "aPsiblastDb" oPath' [eDec, qPath', dbPath']
      wrappedCmdWrite False True cfg ref tPath'
        [dbPre' ++ ".*"]        -- inPtns TODO is this right?
        []                      -- extra outPaths to lock TODO more -out stuff?
        [Shell, AddEnv "BLASTDB" cDir] -- opts TODO Shell? more specific cache?
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
        querySeqId <- fmap (head . words . head . lines) $ readFile' qPath'
        pssmLines <- fmap lines $ readFile' tPath'
        let pssmLines' = if null pssmLines then ["<<emptypssm>>"] else tail pssmLines
            dbName     = takeFileName dbPre'
            trainInfo  = "(trained on " ++ dbName ++ " with e-value cutoff " ++ eStr ++ ")"
            queryInfo  = unwords [querySeqId, trainInfo]
            pssmWithId = queryInfo : pssmLines'
        writeCachedLines cfg ref oPath'' pssmWithId
        -- liftIO $ removeFile tPath'
    
----------------------------------------------
-- helpers that make blast dbs and/or pssms --
----------------------------------------------

-- Wrap the 3rd arg of a function call in makeblastdb_prot_each
-- TODO do the first arg in BlastDB.hs and import here?
-- TODO fix passing dbprefix as db itself
withPdbSubjects :: CutExpr -> CutExpr
withPdbSubjects (CutFun rtn salt deps name [a1, a2, xs ])
  =             (CutFun rtn salt deps name [a1, a2, dbs])
  where
    dbs = CutFun  (ListOf pdb) salt (depsOf xs) "makeblastdb_prot_each" [xs]
withPdbSubjects e = error $ "bad argument to withPdbSubjects: " ++ show e

-- Wraps a single faa or an faa.list in makeblastdb_prot
withPdbSubject :: CutExpr -> CutExpr
withPdbSubject (CutFun rtn salt deps name [a1, a2, x ])
  =            (CutFun rtn salt deps name [a1, a2, db])
  where
    db  = CutFun  (ListOf pdb) salt (depsOf fas) "makeblastdb_prot_all" [fas]
    fas = case typeOf x of
            (ListOf _) -> x -- no need to wrap since already a list
            _          -> singleton x
withPdbSubject e = error $ "bad argument to withPdbSubject: " ++ show e

-- Wrap the faa query argument of a psiblast CutFunction in psiblast_train_db
-- TODO sometimes tries to use path to path of db as path to db... where to fix?
withPssmQuery :: CutExpr -> CutExpr
withPssmQuery (CutFun rtn salt deps name [n, q, s])
  =           (CutFun rtn salt deps name [n, p, s])
  where
    p = CutFun pssm salt deps "psiblast_train_db" [n, q, s]
withPssmQuery e = error $ "bad argument to withPssmQuery: " ++ show e

-------------------------------
-- search with fasta queries --
-------------------------------

psiblast :: CutFunction
psiblast = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, faa] bht
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, faa, faa] bht
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
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = rPsiblastEach
  }
  where
    name = "psiblast_each"

rPsiblastEach :: RulesFn
rPsiblastEach st (CutFun rtn salt deps name [e, fa, fas])
  -- = rFun3 (map3of3 pdb bht $ aPsiblastSearchDb) st expr'
  = (rVectorize 3 aPsiblastSearchDb') st expr'
  where
    ps    = CutFun (ListOf pdb) salt deps "psiblast_train_db_each" [e, fa, dbs]
    dbs   = CutFun (ListOf pdb) salt (depsOf fas) "makeblastdb_prot_each" [fas]
    expr' = CutFun rtn salt deps name [e, ps, dbs]
rPsiblastEach _ _ = error "bad argument to rPsiblastEach"

psiblastAll :: CutFunction
psiblastAll = compose1 name
  (mkTypeDesc name [num, faa, ListOf faa] bht)
  psiblastEach
  (ListOf bht)
  (mkConcat bht)
  where
    name = "psiblast_all"

psiblastDb :: CutFunction
psiblastDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] bht
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, faa, pdb] bht
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 aPsiblastSearchDb s (withPssmQuery e)
  }
  where
    name = "psiblast_db"

-- TODO want map3of3 to read a pdb.list here and pass the individual paths,
--      but that interferes with one of the others right?
-- wait can psiblast just take a faa and pdb directly?
psiblastDbEach :: CutFunction
psiblastDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf bht)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf bht)
  , fFixity    = Prefix
  -- can't use withPssmQuery here because there's a list of things to train against
  -- but won't aPsiblastDb default to working with this anyway? (not typechecked that way tho)
  -- , fRules = \s e -> rFun3 (map3of3 pdb bht $ aPsiblastSearchDb) s e
  , fRules     = rVectorize 3 aPsiblastSearchDb'
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
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, faa, faa] pssm
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
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, ListOf faa, faa] (ListOf pssm)
  , fFixity    = Prefix
  -- , fRules     = \s e -> rFun3 (map2of3 faa pssm aPsiblastTrainDb) s $ withPdbSubject e
  , fRules     = \s e -> (rVectorize 2 aPsiblastTrainDb') s $ withPdbSubject e
  }
  where
    name = "psiblast_train_pssms"

-- TODO appears to succeed, but something is messed up about the mapping?
--      (makes a list of length 1 from multiple faa subjects)
psiblastTrainEach :: CutFunction
psiblastTrainEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] (ListOf pssm)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] (ListOf pssm)
  , fFixity    = Prefix
  , fRules = \s e -> rFun3 (map3of3 pdb pssm $ aPsiblastTrainDb) s (withPdbSubjects e)
  }
  where
    name = "psiblast_train_each"

psiblastTrainAll :: CutFunction
psiblastTrainAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] pssm
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] pssm
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 aPsiblastTrainDb s (withPdbSubject e)
  }
  where
    name = "psiblast_train_all"

psiblastTrainDb :: CutFunction
psiblastTrainDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] pssm
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, faa, pdb] pssm
  , fFixity    = Prefix
  , fRules     = rFun3 aPsiblastTrainDb
  }
  where
    name = "psiblast_train_db"

psiblastTrainDbEach :: CutFunction
psiblastTrainDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf pssm)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf pssm)
  , fFixity    = Prefix
  , fRules     = rFun3 $ map3of3 pdb pssm aPsiblastTrainDb
  }
  where
    name = "psiblast_train_db_each"

psiblastTrainPssmsDb :: CutFunction
psiblastTrainPssmsDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf faa, pdb] (ListOf pssm)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, ListOf faa, pdb] (ListOf pssm)
  , fFixity    = Prefix
  -- , fRules     = rFun3 $ map2of3 faa pssm aPsiblastTrainDb
  , fRules     = rVectorize 2 aPsiblastTrainDb'
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
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, pssm, faa] bht
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 aPsiblastSearchDb s $ withPdbSubject e
  }
  where
    name = "psiblast_pssm"

-- TODO why does this one fail? it's not even using rVectorize
psiblastPssmAll :: CutFunction
psiblastPssmAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf faa] bht
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf faa] bht
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 aPsiblastSearchDb s $ withPdbSubject e
  }
  where
    name = "psiblast_pssm_all"

psiblastPssmEach :: CutFunction
psiblastPssmEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf faa] (ListOf bht)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf faa] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 (map3of3 pdb bht $ aPsiblastSearchDb) s (withPdbSubjects e)
  }
  where
    name = "psiblast_pssm_each"

searchArgs :: [String]
searchArgs = ["-outfmt", "6", "-out"]

psiblastPssmDb :: CutFunction
psiblastPssmDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, pdb] bht
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, pssm, pdb] bht
  , fFixity    = Prefix
  , fRules     = rFun3 aPsiblastSearchDb
  }
  where
    name = "psiblast_pssm_db"

psiblastPssmDbEach :: CutFunction
psiblastPssmDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, pssm, ListOf pdb] (ListOf bht)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, pssm, ListOf pdb] (ListOf bht)
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
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, ListOf pssm, pdb] (ListOf bht)
  , fFixity    = Prefix
  -- , fRules     = rFun3 $ map2of3 pssm bht $ aPsiblastSearchDb
  , fRules     = rVectorize 2 aPsiblastSearchDb'
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

-- TODO withPdbSubject fails with rVectorize? psiblastTrainPssms and psiblastEachPssm
-- TODO OK this is weird, why does it fail but psiblastPssms below can use it correctly?
--      specific incompatibility with withPdbSubject?
psiblastEachPssm :: CutFunction
psiblastEachPssm = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf pssm, faa] (ListOf bht)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [num, ListOf pssm, faa] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = \s e -> (rVectorize 2 aPsiblastSearchDb') s (withPdbSubject e)
  }
  where
    name = "psiblast_each_pssm"

-- TODO wait this should return a list right? making it the same as psiblast_each_pssm?
psiblastPssms :: CutFunction
psiblastPssms = compose1 name
  (mkTypeDesc name [num, ListOf pssm, faa] bht)
  psiblastEachPssm
  (ListOf bht)
  (mkConcat bht)
  where
    name = "psiblast_pssms"

psiblastPssmsAll :: CutFunction
psiblastPssmsAll = compose1 name
  (mkTypeDesc name [num, ListOf pssm, faa] bht)
  psiblastEachPssm
  (ListOf bht)
  (mkConcat bht)
  where
    name = "psiblast_pssms_all"

-- TODO think how to write this!
--      it needs the effect of map3of3 psiblast_pssms_all ^
--      but can't actually do that the straightforward way
--      could it use psiblastPssmAll and concat_each?
-- TODO test this
psiblastPssmsEach :: CutFunction
psiblastPssmsEach = compose1 name
  -- (mkTypeDesc name [num, ListOf pssm, faa] bht)
  (mkTypeDesc name [num, ListOf pssm, ListOf faa] (ListOf bht))
  psiblastEachPssm
  (ListOf bht)
  (mkConcat bht)
  where
    name = "psiblast_pssms_each"
