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
import ShortCut.Core.Compile.Basic (defaultTypeCheck)
import ShortCut.Core.Paths         (fromCutPath, cacheDir)
import ShortCut.Core.Util          (stripWhiteSpace)
import ShortCut.Modules.BlastDB    (pdb)
import ShortCut.Modules.Blast      (bht)
import ShortCut.Modules.SeqIO      (faa, aConcat)
import Data.Scientific             (formatScientific, FPFormat(..))
-- import ShortCut.Core.Compile.Vectorize  (rVectorize)
import System.FilePath             ((<.>), takeFileName)
import System.Directory            (removeFile)
import Control.Monad               (when)
import ShortCut.Core.Compile.Map   (rFun3, map2of3, map3of3, rApply1)

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

    -- TODO update tests
    -- one per psiblast fn so its easy to isolate the failures
    -- see if there's an easy way to remove psiblast_train_db from the others
    -- also test the mapNofN and rapply1 fns
    --
    -- TODO fix the 6 failing functions!
    -- TODO LOOK NOT IN RAPPLY1 FIRST, SINCE SOME OF THESE DON'T USE IT
    -- TODO WAIT DAMMIT PROBABLY THOSE MAP FUNCTIONS YOU DIDN'T TEST!!
    -- TODO hey and some of these don't have separate test cuts, so do that!
    --      (better than single + mapped together because that's confusing)

    -- TODO test/practice mapping with psiblastEach

    -- TODO these failing ones all use rFun3 + withPssmQuery:
    -- psiblast_each
    -- psiblast_db_each
  
    -- search with fasta queries (pssm stuff hidden)
    [ psiblast       -- num faa faa      -> bht
    , psiblastEach   -- num faa faa.list -> bht.list TODO fix script failure
    , psiblastAll    -- num faa faa.list -> bht      TODO fix no rule for pdb.list
    , psiblastDb     -- num faa pdb      -> bht
    , psiblastDbEach -- num faa pdb.list -> bht.list TODO fix script failure

    -- explicitly train pssms
    , psiblastTrain       -- num faa faa      -> pssm
    , psiblastTrainEach   -- num faa faa.list -> pssm.list TODO fix script failure
    , psiblastTrainAll    -- num faa faa.list -> pssm
    , psiblastTrainDb     -- num faa pdb      -> pssm
    , psiblastTrainDbEach -- num faa pdb.list -> pssm.list

    -- search with explicit single pssm queries
    , psiblastPssm       -- num pssm faa      -> bht
    , psiblastPssmEach   -- num pssm faa.list -> bht.list
    , psiblastPssmAll    -- num pssm faa.list -> bht
    , psiblastPssmDb     -- num pssm pdb      -> bht
    , psiblastPssmDbEach -- num pssm pdb.list -> bht.list

    -- search with lists of explicit pssm queries
    , psiblastPssms      -- num pssm.list faa -> bht      TODO fix no rule for bht
    , psiblastEachPssmDb -- num pssm.list pdb -> bht.list TODO fix no rule for pssm.tmp
    , psiblastPssmsDb    -- num pssm.list pdb -> bht      TODO fix no rule for bht

    -- twice-vectorized functions (figure these out)
    -- , psiblastPssmsBothVec -- num pssm.list faa.list -> bht.list.list TODO write, test
    -- , psiblastPssmsEach   -- num pssm.list faa.list -> bht.list       TODO write, test
    -- , psiblastPssmsAll    -- num pssm.list faa.list -> bht            TODO write, test
    -- , psiblastPssmsDbEach -- num pssm.list pdb.list -> bht.list       TODO write, test

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

  let oPath'' = debugA cfg "aPsiblastDb" oPath' [eDec, qPath', dbPath']
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
   -
   - TODO why would there ever be duplicate "(trained on..." messages? that's weird
   -}
  when writingPssm $ do
    querySeqId <- fmap (head . words) $ readFile' qPath'
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
  , fRules = \s e -> rFun3 (map3of3 faa bht $ aPsiblastSearchDb) s (withPssmQuery $ withPdbSubjects e)
  }
  where
    name = "psiblast_each"

-- Wrap the 3rd arg of a function call in makeblastdb_prot_each
-- TODO do the first arg in BlastDB.hs and import here?
-- TODO fix passing dbprefix as db itself
withPdbSubjects :: CutExpr -> CutExpr
withPdbSubjects (CutFun rtn salt deps name [a1, a2, fas])
  =             (CutFun rtn salt deps name [a1, a2, dbs])
  where
    fass = CutList (typeOf fas) salt (depsOf fas) [fas]
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
    db  = CutFun  (ListOf pdb) salt (depsOf fas) "makeblastdb_prot" [fas]
withPdbSubject e = error $ "bad argument to withPdbSubject: " ++ show e

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

-- Converts a psiblast function that needs a premade blast db into one that
-- starts from faa. The db/faa is always the 3rd arg.
-- TODO wrap the expr and compile like before instead of doing it in terms of Actions?
-- TODO if keeping this, how to make the tmpfile names stay useful?
-- makeblastdbAnd :: Action3 -> Action3
-- makeblastdbAnd act3 = \cfg locks out a1 a2 a3 -> do

psiblastAll :: CutFunction
psiblastAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] bht
  , fFixity    = Prefix
  , fRules = rApplyConcatBht $ \s e ->
      rFun3 (map3of3 faa bht aPsiblastSearchDb) s (withPssmQuery $ withPdbSubject e)
  }
  where
    name = "psiblast_all"

psiblastDb :: CutFunction
psiblastDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] bht
  , fTypeDesc  = mkTypeDesc name  [num, faa, pdb] bht
  , fFixity    = Prefix
  , fRules     = rFun3 aPsiblastSearchDb
  }
  where
    name = "psiblast_db"

-- TODO withPssmQuery -> script fails trying to use dbprefix lines as the db itself in train_db
-- TODO withPssmQueries -> no fn psiblast_train_pssms (true, not implemented!)
psiblastDbEach :: CutFunction
psiblastDbEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf pdb] (ListOf bht)
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 (map3of3 pdb bht aPsiblastSearchDb) s (withPssmQueries e)
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

psiblastTrainEach :: CutFunction
psiblastTrainEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] (ListOf pssm)
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] (ListOf pssm)
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 (map3of3 pdb pssm $ aPsiblastTrainDb) s
                               (withPdbSubjects e)
  }
  where
    name = "psiblast_train_each"

psiblastTrainAll :: CutFunction
psiblastTrainAll = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, ListOf faa] pssm
  , fTypeDesc  = mkTypeDesc name  [num, faa, ListOf faa] pssm
  , fFixity    = Prefix
  , fRules     = \s e -> rFun3 aPsiblastTrainDb s $ withPssmQuery $ withPdbSubject e
  }
  where
    name = "psiblast_train_all"

-- TODO this fails as part of psiblast_db_each but not alone?
--      (maybe that edits the type to be wrong?)
psiblastTrainDb :: CutFunction
psiblastTrainDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, faa, pdb] pssm
  , fTypeDesc  = mkTypeDesc name  [num, faa, pdb] pssm
  , fFixity    = Prefix
  , fRules     = rFun3 $ aPsiblastTrainDb
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
  , fRules     = \s e -> rFun3 (map3of3 pdb bht $ aPsiblastSearchDb) s (withPdbSubjects e)
  }
  where
    name = "psiblast_pssm_each"

searchArgs :: [String]
searchArgs = ["-outfmt", "6", "-out"]

-- TODO this is the easiest one! why is it failing??
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

-- TODO remove now that it's not used to implement the others?
-- TODO or rename psiblast_pssms_dbs? that's much less confusing if you adjust others to match
-- TODO does it work with the new sketchy map2of3?
-- TODO make a test cut i guess
psiblastEachPssmDb :: CutFunction
psiblastEachPssmDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf pssm, pdb] (ListOf bht)
  , fTypeDesc  = mkTypeDesc name  [num, ListOf pssm, pdb] (ListOf bht)
  , fFixity    = Prefix

  -- TODO oh i get it, this needs to make a single protein db not a list:
  -- TODO wait no, it doesn't need one at all
  -- , fRules = \s e -> rFun3 (map2of3 pssm bht $ aPsiblastSearchDb) s (withPdbSubject e)
  , fRules = rFun3 $ map2of3 pssm bht $ aPsiblastSearchDb

  }
  where
    name = "psiblast_each_pssm_db"

-- TODO does this work with the new map2of3?
psiblastPssmsDb :: CutFunction
psiblastPssmsDb = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf pssm, pdb] bht
  , fTypeDesc  = mkTypeDesc name  [num, ListOf pssm, pdb] bht
  , fFixity    = Prefix
  , fRules     = rApplyConcatBht $ rFun3 $ map2of3 pssm bht $ aPsiblastSearchDb
  }
  where
    name = "psiblast_pssms_db"

{- OK so the function is going to produce a bht.list, and we just want to
 - concat that to bht right? Not so hard! The complication is that instead of
 - using the regular rExpr compiler we need to use the new parameterized
 - version with the properly modified Action3.
 -
 - And we can't wrap it in concat_bht first, because then we'd lose the chance
 - to apply the current action.
 -
 - Any easier way? Well first of all it's not strictly necessary for the cuts right?
 - Like, could just explicitly add concat_bht and concat_bht_each around these calls.
 -
 - But if skipping this, need to try to do the twice-vectorized ones without it.
 -}

-- Temporary workaround to get rApplyConcatBht working
-- rListFun3 :: Action3 -> RulesFn
-- rListFun3 act3 st@(_,cfg,ref) expr@(CutFun _ _ _ _ [exprs]) = do
--   -- elems <- mapM (rFun3 act3 st) exprs
--   elems <- rFun3 act3 st exprs
--   let out    = exprPath st expr
--       out'   = fromCutPath cfg out
--       elems' = map (\(ExprPath p) -> toCutPath cfg p) elems
--   out' %> \_ -> writePaths cfg ref out' elems'
--   return $ ExprPath out'
-- rListFun3 _ _ _ = error "bad argument to rListFun3"

-- rApply1 fnName fnAct fnType rules st@(_,cfg,ref) expr = do
-- rApplyConcatBht :: String -> CutType -> Action3 -> RulesFn
-- rApply1 :: String -> Action1 -> CutType -> RulesFn -> RulesFn
-- rApply1 fnName fnAct fnType rules st@(_,cfg,ref) expr = do
rApplyConcatBht :: RulesFn -> RulesFn
rApplyConcatBht = rApply1 "concat_bht" aConcat' bht
  where
    aConcat' c r o fs = aConcat c r [o, fs]
-- aConcat :: CutConfig -> Locks -> [CutPath] -> Action ()
-- aConcat cfg ref [oPath, fsPath] = do

-- rApplyConcatBht act3 st@(_,cfg,ref) e@(CutFun _ _ _ _ _) = do
--   (ExprPath list') <- rListFun3 act3 st e
--   let out  = exprPath st e -- TODO use a new path for the overall expression!
--       out' = fromCutPath cfg out
--       list = toCutPath cfg list'
--   out' %> \_ -> do aConcat cfg ref [out, list]
--   return $ ExprPath out'
-- rApplyConcatBht _ _ e = error $ "bad argument to rApplyConcatBht: " ++ show e

-- wrap a <whatever>.list in concat_<whatever> to make a <whatever>
-- Note that the initial expr will be mis-annotated a s rtn when it's really a rtn.list;
-- this function corrects it. (yes that's confusing and may be a code smell)
-- TODO THIS IS BROKEN BECAUSE IT'S WRAPPING THE EXPRESSION BUT THEN STILL PASSING TO RFUN3
--      SHOULD HAVE A BUILT-IN RLIST INSTEAD?
-- I think it's OK to have this take an Action3 and apply it to each list element,
-- since it's a special case that will come up a lot in this module at least.
-- rApplyConcatBht :: Action3 -> RulesFn
-- rApplyConcatBht act3 st@(_,cfg,ref) e@(CutList _ _ deps exprs) = do
-- 
--   -- first, have to compile elems with specifically (rFun3 act3),
--   -- not the regular rExpr
--   -- TODO that's gotta be fixable/a bad idea right?
--   -- elemPaths <- mapM (rFun3 act3 st) exprs
--   -- let list' = exprPath st e
--   -- list' %> \_ -> writePaths 
--   (ExprPath listPath) <- rListFun3 act3 st expr
-- 
--   -- now we do more or less what rSimple and/or rConcat would do
--   let out   = exprPath st e -- TODO use a new path for the overall expression!
--       out'  = fromCutPath cfg out
--       elems' = map (\(ExprPath p) -> toCutPath cfg p) elemPaths
--   out' %> \_ -> do
--     writePaths cfg ref tmp elems'
--     aConcat cfg ref [out, tmp]
--   return $ ExprPath out'
-- rApplyConcatBht _ _ e = error $ "bad argument to rConcat: " ++ show e

psiblastPssms :: CutFunction
psiblastPssms = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf pssm, faa] bht
  , fTypeDesc  = mkTypeDesc name  [num, ListOf pssm, faa] bht
  , fFixity    = Prefix
  , fRules     = rApplyConcatBht $ rFun3 $ map2of3 pssm bht $ aPsiblastSearchDb
  }
  where
    name = "psiblast_pssms"
