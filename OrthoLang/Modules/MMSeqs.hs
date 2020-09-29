module OrthoLang.Modules.MMSeqs
  where

-- TODO add variants: _each, _all, _rev, _rev_each, _rbh

-- TODO test failures are because createdb sometimes doesn't create the final .mmseqsdb file, but is otherwise OK?

-- TODO keep intermediate files at least until we can line them up with sonicparanoid if needed
-- TODO default to the same settings sonicparanoid uses at first, both to be compatible and until you know better
-- TODO length function should be polymorphic enough to also do dbs

-- TODO mmseqs_search_all : fa.list mms -> mmr (this is mmseqs search)
-- TODO mmseqs_search_profile?
-- TODO mmseqs_cluster
-- TODO mmseqs_linclust
-- TODO mmseqs_clusterupdate?
-- TODO mmseqs_taxonomy? maybe later
-- TODO might need to provide a giant tmpdir when it goes on the server
-- TODO should we createindex before searching? not yet for small stuff
-- TODO caution that you can't use fna as both subject and query, just one or the other
--      (that can be done but requires a separate script with a few steps)

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Interpreter.Paths (pathDigest)
import OrthoLang.Locks

import OrthoLang.Modules.Blast   (bht)
import OrthoLang.Modules.Singletons (withSingleton)
import OrthoLang.Modules.SeqIO   (fa, fna, faa)
import System.Directory          (createDirectoryIfMissing)
import System.Exit               (ExitCode(..))
import System.FilePath           ((</>), (<.>), (-<.>), takeDirectory, dropExtension)
import Data.Maybe (fromJust)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "MMSeqs"
  , mDesc = "Many-against-many sequence searching: ultra fast and sensitive search and clustering suite"
  , mTypes = [faa, fna, bht, mms]
  , mGroups = []
  , mEncodings = []
  , mRules = []
  , mFunctions =
      [ mmseqsCreateDbAll
      , mmseqsCreateDb
      , mmseqsSearchDb
      , mmseqsSearchDbDb
      , mmseqsSearch
      ]
  }

-- note that this has an internal nucleic or amino acid type, but the program always handles either
mms :: Type
mms = Type
  { tExt  = "mms"
  , tDesc = "MMSeqs2 sequence database"
  , tShow = \cfg ref path -> do
      path' <- fmap (-<.> "lookup") $ resolveSymlinks (Just [tmpdir cfg]) path
      Stdout out <- withReadLock ref path' $ cmd "wc" ["-l", path']
      let n = headOrDie "failed to read first word of MMSeqs2 db description" $ words out
      -- h5    <- fmap (take 5 . lines) $ withReadLock ref path $ readFileStrict' path'
      let d = "MMSeqs2 database (" ++ n ++ " sequences)" -- TODO include a hash?
      return d
  }

-- TODO mht type, because it's not really equivalent to blast!
-- TODO and then make that a member of ht

-- TODO is this needed, or can we just --format-output directly?
-- ali :: Type
-- ali

-------------------------
-- mmseqs_createdb_all --
-------------------------

mmseqsCreateDbAll :: Function
mmseqsCreateDbAll = newFnA1
  "mmseqs_createdb_all"
  (ListSigs $ Some fa "any fasta type")
  (Exactly mms)
  aMmseqsCreateDbAll -- TODO factor this out of the rules one
  [] -- TODO nondeterministic?

{- Looks like mmseqs2 expects later commands to be smart about index naming? It
 - doesn't always create the plain no-suffix index file, but expects that
 - instead of a .index or whatever. We'll symlink to the .index file and remove
 - its suffix later for now.
 -}
aMmseqsCreateDbAll :: NewAction1
aMmseqsCreateDbAll (ExprPath out') fasPath' = do
  cfg <- fmap fromJust getShakeExtra
  let loc         = "modules.mmseqs.aMmseqsCreateDbAll"
      fasPath     = toPath loc cfg fasPath'
      (PathDigest d) = pathDigest fasPath
      createDbDir = tmpdir cfg </> "cache" </> "mmseqs" </> "createdb" </> d
      dbPath      = createDbDir </> "result" -- TODO take hash from somewhere else?
      index       = dbPath <.> "index" -- mmseqs2 always writes this one?
      out         = toPath loc cfg out'
  faPaths <- readPaths loc fasPath'
  let faPaths' = map (fromPath loc cfg) faPaths
  liftIO $ createDirectoryIfMissing True createDbDir
  -- TODO does mmseqs no longer always write a plain .mmseqs2db file? maybe we have to touch that ourselves?
  runCmd $ CmdDesc
    { cmdParallel = False -- TODO true?
    , cmdFixEmpties = True
    , cmdOutPath = out'
    , cmdInPatterns = [dbPath ++ "*"]
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [] -- TODO should there be some?
    , cmdOptions =[Cwd createDbDir] -- TODO remove?
    , cmdBinary = "mmseqs-createdb-all.sh"
    , cmdArguments = [dbPath] ++ faPaths'
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out', dbPath ++ "*"] -- TODO adjust the code to handle patterns!
    }
  symlink out $ toPath loc cfg index

---------------------
-- mmseqs_createdb --
---------------------

mmseqsCreateDb :: Function
mmseqsCreateDb = newExprExpansion
  "mmseqs_createdb"
  [Some fa "any fasta type"]
  (Exactly mms)
  mMmseqsCreateDb
  [] -- TODO nondeterministic?

mMmseqsCreateDb :: ExprExpansion
mMmseqsCreateDb _ _ (Fun r ms ds _ [e]) = Fun r ms ds "mmseqs_createdb_all" [withSingleton e]
mMmseqsCreateDb _ _ e = error "orhtolang.modules.mmseqs.mMmseqsCreateDb" $ "bad argument: " ++ show e

----------------------
-- mmseqs_search_db --
----------------------

-- TODO for now this should also handle the convertalis step
-- TODO any reason to have a version that takes the query as a db? seems unnecessary

-- | This is the "base" search function, but it still expands to two different functions:
--
--   * first, the actual search which returns an mms database
--   * then an extractalis script to convert that to a BLAST-format hit table
--
mmseqsSearchDbDb :: Function
mmseqsSearchDbDb = newFnA3
  "mmseqs_search_db_db" -- TODO is this an ok name?
  (Exactly num, Exactly mms, Exactly mms)
  (Exactly bht) -- TODO exact same format, right?
  -- mMmseqsSearchDb
  aMmseqsSearchDbDb
  [Hidden, Nondeterministic] -- TODO is it nondeterministic?

-- | Version that takes a query fasta, similar to other search functions, and does the MMSeqs DB thing
mmseqsSearchDb :: Function
mmseqsSearchDb = newExprExpansion
  "mmseqs_search_db"
  [Exactly num, Some fa "any fasta file", Exactly mms]
  (Exactly bht) -- TODO exact same format, right?
  mMmseqsSearchDb
  [Nondeterministic] -- TODO is it nondeterministic?

mMmseqsSearchDb :: ExprExpansion
mMmseqsSearchDb _ _ (Fun r ms ds _ [e, qFa, sDb]) = Fun r ms ds "mmseqs_search_db_db" [e, qDb, sDb]
  where
    qDb = Fun mms Nothing ds "mmseqs_createdb" [qFa]
mMmseqsSearchDb _ _ e = error "ortholang.modules.mmseqs.mMmseqsSearchDb" $ "bad argument: " ++ show e


-- looks like it will work, but not needed if we can just --format-output like blast in the first place
-- mMmseqsSearchDb :: ExprExpansion
-- mMmseqsSearchDb _ _ (Fun r ms ds _ es) = Fun r ms ds "mmseqs_convert_alis" [fn]
--   where
--     fn = Fun ali
-- mMmseqsSearchDb _ _ e = error "orhtolang.modules.mmseqs.mMmseqsSearchDb" $ "bad argument: " ++ show e

-- TODO mmseqs_search_db_ali function returning an aln database (only if needed)
-- TODO mmseqs_convert_alis function to convert mmseqs aln database -> hit table (only if needed)

-- (num, (Some fa "any fasta file"), (EncodedAs mmseqsdb (Some fa "also any fasta file"))) bht
-- shown as "num fa1 fa2.mmseqsdb -> bht, where fa1 is any fasta file, fa2 is also any fasta file"
-- tMmseqsSearchDb :: String -> TypeChecker
-- TODO can this error be maintained despite the vague mms type? maybe not...
-- tMmseqsSearch n [x, y] | x == fna && y == fna = error $ "Both " ++ n ++ " args can't be fna; translate one first."
-- tMmseqsSearchDb _ [x, y, z] | x == num && y `elem` [fna, faa] && z == mms = Right bht
-- tMmseqsSearchDb n types = fail $ n ++ " requires a number, fasta, and mmseqs2 db. Instead, got: " ++ show types

{- Even though all the MMseqs2 commands make some kind of database, the files
 - it will include are obscure to me. The search ones here have .0, .1 ... .n
 - suffixes and no .index or plain one with no extension. Easiest solution for
 - now seems to be depending on the .0 one and remembering to remove that
 - suffix later when needed.
 -}
-- rMmseqsSearchDb :: RulesFn
-- rMmseqsSearchDb scr e@(Fun _ seed _ _ [n, q, s]) = do
--   (ExprPath ePath) <- rExpr scr n
--   (ExprPath qPath) <- rExpr scr $ Fun mms seed (depsOf q) "mmseqs_createdb" [q]
--   (ExprPath sPath) <- rExpr scr s -- note: the subject should already have been converted to a db
--   cfg  <- fmap fromJust getShakeExtraRules
--   dRef <- fmap fromJust getShakeExtraRules
--   let loc = "modules.mmseqs.rMmseqsSearchDb"
--       out    = exprPath cfg dRef scr e
--       out'   = debugRules "rMmseqsSearch" e $ fromPath loc cfg out
--       -- createDbDir  = tmpdir cfg </> "cache" </> "mmseqs" </> "search" </> digest e
--       -- createDbDir  = tmpdir cfg </> "cache" </> "mmseqs" </> "createdb" -- TODO be more or less specific?
--       searchDbDir  = tmpdir cfg </> "cache" </> "mmseqs" </> "search"
--       outDbBase = searchDbDir </> digest loc out <.> "mmseqs2db"
--       -- outDb0    = outDbBase <.> "0" -- TODO remember to remove the .0 when referencing it!
--       outDbIndex = outDbBase <.> "index" -- TODO remember to remove the ext when referencing it!
--   outDbIndex %> \_ -> aMmseqsSearchDb ePath qPath sPath outDbBase
--   out'   %> \_ -> aMmseqsConvertAlis qPath sPath outDbIndex out'
--   return (ExprPath out')
-- rMmseqsSearchDb _ e = fail $ "bad argument to rMmseqsSearch: " ++ show e

-- TODO how should this handle the .index and other files issue?
resolveMmseqsDb :: FilePath -> Action FilePath
resolveMmseqsDb path = do
  let loc = "modules.mmseqs.resolveMmseqsDb"
  need' loc [path] -- path is to a symlink to the main db file
  path' <- liftIO $ resolveSymlinks Nothing path
  -- let path''  = if ".index" `isSuffixOf` path'  then dropExtension path' else path'
  --     path''' = if "_h"     `isSuffixOf` path'' then init $ init path''  else path''
  return path'

-- TODO is the db being passed in place of the fa too?
-- TODO interesting! it works in stack repl but not stack exec
-- TODO search creates some db.* files but not the plain base file or .index! separate into different dir?
-- aMmseqsSearchDb :: FilePath -> FilePath -> FilePath -> FilePath -> Action ()
-- aMmseqsSearchDb ePath qDb sDb outDb = do

-- same as the Ali version below, but also runs aMmseqsConvertAlis to generate a table
-- TODO make that a separate function with ExprExpansion?
-- note that unline most search programs, for mmseqs we also convert the query fasta to a db first
aMmseqsSearchDbDb :: NewAction3
aMmseqsSearchDbDb (ExprPath outTab') ePath qDb sDb = do
  cfg  <- fmap fromJust getShakeExtra
  let loc = "modules.mmseqs.aMmseqsSearchDb"
      searchDbDir = tmpdir cfg </> "cache" </> "mmseqs" </> "search"
      outDbBase   = searchDbDir </> digest loc outTab' <.> "mmseqs2db"
      outDbIndex  = outDbBase <.> "index" -- TODO remember to remove the ext when referencing it!
  aMmseqsSearchDbDbAli (ExprPath outDbBase) ePath qDb sDb
  aMmseqsConvertAlis qDb sDb outDbIndex outTab'

aMmseqsSearchDbDbAli :: NewAction3
aMmseqsSearchDbDbAli (ExprPath outDb') ePath qDb sDb = do
  let loc = "modules.mmseqs.aMmseqsSearchDb"
  eStr <- readLit loc ePath
  qDb' <- fmap dropExtension $ resolveMmseqsDb qDb
  sDb' <- fmap dropExtension $ resolveMmseqsDb sDb
  let tmpDir = takeDirectory outDb' </> "tmp" -- TODO align this with sonicparanoid
  liftIO $ createDirectoryIfMissing True tmpDir
  runCmd $ CmdDesc
    { cmdParallel = False -- TODO true?
    , cmdFixEmpties = True
    , cmdOutPath = outDb'
    , cmdInPatterns = [qDb', sDb']
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [] -- TODO should there be some?
    , cmdOptions =[]
    , cmdBinary = "mmseqs-search.sh"
    , cmdArguments = [outDb', tmpDir, eStr, qDb', sDb']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outDb' ++ "*"]
    }

-- TODO remember to remove the .index extension when actually calling mmseqs
-- TODO convert this to a separate function with ExprExpansion?
aMmseqsConvertAlis :: FilePath -> FilePath -> FilePath -> FilePath -> Action ()
aMmseqsConvertAlis qDb sDb outDbIndex outTab = do
  -- let outDbBase = dropExtension outDbIndex
  qDb' <- fmap dropExtension $ resolveMmseqsDb qDb
  sDb' <- fmap dropExtension $ resolveMmseqsDb sDb
  oDb' <- fmap dropExtension $ resolveMmseqsDb outDbIndex
  -- TODO check this matches my existing blast hit tables, since mmseqs seems to have removed the format option?
  -- , "--format-output", "query target pident alnlen mismatch gapopen qstart qend tstart tend evalue bits"
  runCmd $ CmdDesc
    { cmdParallel = False -- TODO true?
    , cmdFixEmpties = True
    , cmdOutPath = outTab
    , cmdInPatterns = [qDb, sDb, oDb' <.> "index"]
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [] -- TODO should there be some?
    , cmdOptions =[]
    , cmdBinary = "mmseqs-convertalis.sh"
    , cmdArguments = [outTab, qDb', sDb', oDb']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [oDb' ++ "*"]
    }

-------------------
-- mmseqs_search --
-------------------

-- TODO rewrite as ExprExpansion
mmseqsSearch :: Function
mmseqsSearch = let name = "mmseqs_search" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeDesc  = name ++ " : num fa fa -> bht"
  -- , fTypeCheck = tMmseqsSearch name
  , fInputs = [Exactly num, Some fa "query (any fasta)", Some fa "subject (any fasta)"]
  , fOutput = Exactly bht
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMmseqsSearch
  }

-- tMmseqsSearch :: String -> TypeChecker
-- tMmseqsSearch n [_, y, z] | y == fna && z == fna = fail $ "Both " ++ n ++ " args can't be fna; translate one first."
-- tMmseqsSearch _ [x, y, z] | x == num && y `elem` [fna, faa] && z `elem` [fna, faa] = Right bht
-- tMmseqsSearch n types = fail $ n ++ " requires a number and two fasta files. Instead, got: " ++ show types

rMmseqsSearch :: RulesFn
rMmseqsSearch st (Fun rtn seed deps name [e, q, s])
  = rules st (Fun rtn seed deps name [e, q, sDb])
  where
    rules = fOldRules mmseqsSearchDb
    sDb = Fun mms Nothing (depsOf s) "mmseqs_createdb" [s] -- TODO also accept a fa.list here?
rMmseqsSearch _ _ = fail "bad argument to rMmseqsSearch"
