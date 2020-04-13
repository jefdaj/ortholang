module OrthoLang.Modules.MMSeqs
  where

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
import OrthoLang.Core
import OrthoLang.Locks

import OrthoLang.Modules.Blast   (bht)
import OrthoLang.Modules.Singletons (withSingletonArg)
import OrthoLang.Modules.SeqIO   (fa, fna, faa)
import System.Directory          (createDirectoryIfMissing)
import System.Exit               (ExitCode(..))
import System.FilePath           ((</>), (<.>), (-<.>), takeDirectory, dropExtension)
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "MMSeqs"
  , mDesc = "Many-against-many sequence searching: ultra fast and sensitive search and clustering suite"
  , mTypes = [faa, fna, bht, mms]
  , mGroups = []
  , mFunctions =
      [ mmseqsCreateDbAll
      , mmseqsCreateDb
      , mmseqsSearchDb
      , mmseqsSearch
      ]
  }

-- note that this has an internal nucleic or amino acid type, but the program always handles either
mms :: Type
mms = Type
  { tExt  = "mms"
  , tDesc = "MMSeqs2 sequence database"
  , tShow = \_ ref path -> do
      path' <- fmap (-<.> "lookup") $ resolveSymlinks Nothing path
      Stdout out <- withReadLock ref path' $ cmd "wc" ["-l", path']
      let n = headOrDie "failed to read first word of MMSeqs2 db description" $ words out
      -- h5    <- fmap (take 5 . lines) $ withReadLock ref path $ readFileStrict' path'
      let desc = "MMSeqs2 database (" ++ n ++ " sequences)" -- TODO include a hash?
      return desc
  }

-------------------------
-- mmseqs_createdb_all --
-------------------------

mmseqsCreateDbAll :: Function
mmseqsCreateDbAll = let name = "mmseqs_createdb_all" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeDesc  = name ++ " : fa.list -> mms"
  -- , fTypeCheck = tMmseqsCreateDbAll name
  , fInputs = [ListSigs (Some fa "any fasta type")]
  , fOutput = Exactly mms
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMmseqsCreateDbAll
  }

-- (ListOf (Some fa "any fasta file")) (EncodedAs mmseqsdb (Some fa "any fasta file"))
-- shown as "fa.list -> fa.mmseqsdb, where fa is any fasta file"
-- tMmseqsCreateDbAll :: String -> TypeChecker
-- tMmseqsCreateDbAll _ [(ListOf x)] | x `elem` [fna, faa] = Right mms
-- tMmseqsCreateDbAll name types = fail $ name ++ " requires a list of fasta files, but got " ++ show types

{- Looks like mmseqs2 expects later commands to be smart about index naming? It
 - doesn't always create the plain no-suffix index file, but expects that
 - instead of a .index or whatever. We'll symlink to the .index file and remove
 - its suffix later for now.
 -}
rMmseqsCreateDbAll :: RulesFn
rMmseqsCreateDbAll scr e@(Fun _ _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr scr fas
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let out    = exprPath cfg dRef scr e
      out'   = debugRules "rMmseqsCreateDbAll" e $ fromPath cfg out
      createDbDir  = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "createdb" -- TODO be more or less specific?
      dbPath = createDbDir </> digest fas <.> "mmseqs2db" -- TODO take hash from somewhere else?
      index  = dbPath <.> "index" -- mmseqs2 always writes this one?
  out' %> \_ -> do
    unlessExists dbPath $ do -- TODO any reason it would exist already?
      faPaths <- readPaths fasPath
      let faPaths' = map (fromPath cfg) faPaths
      liftIO $ createDirectoryIfMissing True createDbDir
      -- TODO does mmseqs no longer always write a plain .mmseqs2db file? maybe we have to touch that ourselves?
      runCmd $ CmdDesc
        { cmdParallel = False -- TODO true?
        , cmdFixEmpties = True
        , cmdOutPath = out'
        , cmdInPatterns = [dbPath ++ "*"]
        , cmdExtraOutPaths = []
        , cmdSanitizePaths = [] -- TODO should there be some?
        , cmdOptions =[Cwd createDbDir] -- TODO remove?
        , cmdBinary = "mmseqs-createdb-all.sh"
        , cmdArguments = [dbPath] ++ faPaths'
        , cmdExitCode = ExitSuccess
        , cmdRmPatterns = [out', dbPath ++ "*"] -- TODO adjust the code to handle patterns!
        }
    symlink out $ toPath cfg index
  return (ExprPath out')
rMmseqsCreateDbAll _ e = fail $ "bad argument to rMmseqsCreateDbAll: " ++ show e

---------------------
-- mmseqs_createdb --
---------------------

mmseqsCreateDb :: Function
mmseqsCreateDb = let name = "mmseqs_createdb" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeDesc  = name ++ " : fa -> mms"
  -- , fTypeCheck = tMmseqsCreateDb name
  , fInputs = [Some fa "any fasta type"]
  , fOutput = Exactly mms
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMmseqsCreateDb
  }

-- (Some fa "any fasta file") (EncodedAs mmseqsdb (Some fa "any fasta file"))
-- shown as "fa -> fa.mmseqsdb, where fa is any fasta file"
-- tMmseqsCreateDb :: String -> TypeChecker
-- tMmseqsCreateDb _ [x] | x `elem` [fna, faa] = Right mms
-- tMmseqsCreateDb name types = fail $ name ++ " requires a fasta file, but got " ++ show types

rMmseqsCreateDb :: RulesFn
rMmseqsCreateDb s e = rMmseqsCreateDbAll s $ withSingletonArg e

----------------------
-- mmseqs_search_db --
----------------------

-- TODO for now this should also handle the convertalis step
-- TODO any reason to have a version that takes the query as a db? seems unnecessary

mmseqsSearchDb :: Function
mmseqsSearchDb = let name = "mmseqs_search_db" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeDesc  = name ++ " : num fa mms -> bht"
  -- , fTypeCheck = tMmseqsSearchDb name
  , fInputs = [Exactly num, Some fa "any fasta file", Exactly mms]
  , fOutput = Exactly bht -- TODO exact format right?
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMmseqsSearchDb
  }

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
rMmseqsSearchDb :: RulesFn
rMmseqsSearchDb scr e@(Fun _ salt _ _ [n, q, s]) = do
  (ExprPath ePath) <- rExpr scr n
  (ExprPath qPath) <- rExpr scr $ Fun mms salt (depsOf q) "mmseqs_createdb" [q]
  (ExprPath sPath) <- rExpr scr s -- note: the subject should already have been converted to a db
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let out    = exprPath cfg dRef scr e
      out'   = debugRules "rMmseqsSearch" e $ fromPath cfg out
      -- createDbDir  = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "search" </> digest e
      -- createDbDir  = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "createdb" -- TODO be more or less specific?
      searchDbDir  = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "search"
      outDbBase = searchDbDir </> digest out <.> "mmseqs2db"
      -- outDb0    = outDbBase <.> "0" -- TODO remember to remove the .0 when referencing it!
      outDbIndex = outDbBase <.> "index" -- TODO remember to remove the ext when referencing it!
  outDbIndex %> \_ -> aMmseqsSearchDb ePath qPath sPath outDbBase
  out'   %> \_ -> aMmseqConvertAlis qPath sPath outDbIndex out'
  return (ExprPath out')
rMmseqsSearchDb _ e = fail $ "bad argument to rMmseqsSearch: " ++ show e

-- TODO how should this handle the .index and other files issue?
resolveMmseqsDb :: FilePath -> Action FilePath
resolveMmseqsDb path = do
  need [path] -- path is to a symlink to the main db file
  path' <- liftIO $ resolveSymlinks Nothing path
  -- let path''  = if ".index" `isSuffixOf` path'  then dropExtension path' else path'
  --     path''' = if "_h"     `isSuffixOf` path'' then init $ init path''  else path''
  return path'

-- TODO is the db being passed in place of the fa too?
-- TODO interesting! it works in stack repl but not stack exec
-- TODO search creates some db.* files but not the plain base file or .index! separate into different dir?
aMmseqsSearchDb :: FilePath -> FilePath -> FilePath -> FilePath -> Action ()
aMmseqsSearchDb ePath qDb sDb outDb = do
  eStr <- readLit ePath
  qDb' <- fmap dropExtension $ resolveMmseqsDb qDb
  sDb' <- fmap dropExtension $ resolveMmseqsDb sDb
  let tmpDir = takeDirectory outDb </> "tmp" -- TODO align this with sonicparanoid
  liftIO $ createDirectoryIfMissing True tmpDir
  runCmd $ CmdDesc
    { cmdParallel = False -- TODO true?
    , cmdFixEmpties = True
    , cmdOutPath = outDb
    , cmdInPatterns = [qDb, sDb]
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [] -- TODO should there be some?
    , cmdOptions =[]
    , cmdBinary = "mmseqs-search.sh"
    , cmdArguments = [outDb, tmpDir, eStr, qDb', sDb']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outDb ++ "*"]
    }
  -- liftIO $ removeDirectoryRecursive tmpDir

-- TODO remember to remove the .index extension when actually calling mmseqs
aMmseqConvertAlis :: FilePath -> FilePath -> FilePath -> FilePath -> Action ()
aMmseqConvertAlis qDb sDb outDbIndex outTab = do
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
rMmseqsSearch st (Fun rtn salt deps name [e, q, s])
  = rules st (Fun rtn salt deps name [e, q, sDb])
  where
    rules = fOldRules mmseqsSearchDb
    sDb = Fun mms salt (depsOf s) "mmseqs_createdb" [s] -- TODO also accept a fa.list here?
rMmseqsSearch _ _ = fail "bad argument to rMmseqsSearch"
