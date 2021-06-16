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
      path' <- (-<.> "lookup") <$> resolveSymlinks (Just [tmpdir cfg]) path
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
    , cmdArguments = dbPath : faPaths'
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

{- Even though all the MMseqs2 commands make some kind of database, the files
 - it will include are obscure to me. The search ones here have .0, .1 ... .n
 - suffixes and no .index or plain one with no extension. Easiest solution for
 - now seems to be depending on the .0 one and remembering to remove that
 - suffix later when needed.
 - TODO how should this handle the .index and other files issue?
 -}
resolveMmseqsDb :: FilePath -> Action FilePath
resolveMmseqsDb path = do
  let loc = "modules.mmseqs.resolveMmseqsDb"
  need' loc [path] -- path is to a symlink to the main db file
  liftIO $ resolveSymlinks Nothing path

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
  qDb' <- dropExtension <$> resolveMmseqsDb qDb
  sDb' <- dropExtension <$> resolveMmseqsDb sDb
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
  qDb' <- dropExtension <$> resolveMmseqsDb qDb
  sDb' <- dropExtension <$> resolveMmseqsDb sDb
  oDb' <- dropExtension <$> resolveMmseqsDb outDbIndex
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

-- | Simplest user-facing version that makes subject + query fastas into dbs first
mmseqsSearch :: Function
mmseqsSearch = newExprExpansion
  "mmseqs_search"
  [Exactly num, Some fa "any query fasta file", Some fa "any subject fasta file"] -- TODO ok if both same?
  (Exactly bht)
  mMmseqsSearch
  [Nondeterministic]

mMmseqsSearch :: ExprExpansion
mMmseqsSearch _ _ (Fun r ms ds _ [e, qFa, sFa]) = Fun r ms ds "mmseqs_search_db_db" [e, qDb, sDb]
  where
    qDb = Fun mms Nothing ds "mmseqs_createdb" [qFa]
    sDb = Fun mms Nothing ds "mmseqs_createdb" [sFa]
mMmseqsSearch _ _ e = error "ortholang.modules.mmseqs.mMmseqsSearch" $ "bad argument: " ++ show e
