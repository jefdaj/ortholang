module Detourrr.Modules.MMSeqs
  where

-- TODO keep intermediate files at least until we can line them up with sonicparanoid if needed
-- TODO default to the same settings sonicparanoid uses at first, both to be compatible and until you know better

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
import Detourrr.Core.Types

import Detourrr.Core.Actions       (readLit, readPaths, wrappedCmdWrite, symlink)
import Detourrr.Core.Compile.Basic (rExpr, debugRules)
import Detourrr.Core.Locks         (withReadLock)
import Detourrr.Core.Paths         (toRrrPath, fromRrrPath, exprPath)
import Detourrr.Core.Util          (digest, unlessExists, resolveSymlinks)
import Detourrr.Modules.Blast      (bht)
import Detourrr.Modules.BlastDB    (withSingleton) -- TODO move to core?
import Detourrr.Modules.SeqIO      (fna, faa)
import System.Directory            (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath             ((</>), (<.>), takeDirectory)

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "MMSeqs"
  , mDesc = "Many-against-many sequence searching: ultra fast and sensitive search and clustering suite"
  , mTypes = [faa, fna, bht, mms]
  , mFunctions =
      [ mmseqsCreateDbAll
      , mmseqsCreateDb
      , mmseqsSearchDb
      , mmseqsSearch
      ]
  }

-- note that this has an internal nucleic or amino acid type, but the program always handles either
mms :: RrrType
mms = RrrType
  { tExt  = "mms"
  , tDesc = "MMSeqs2 sequence database"
  , tShow = \_ ref path -> do
      path' <- fmap (<.> "lookup") $ resolveSymlinks Nothing path
      h5    <- fmap (take 5 . lines) $ withReadLock ref path $ readFile path'
      let desc = unlines $ ["MMSeqs2 sequence database " ++ path ++ ":"] ++ h5 ++ ["..."]
      return desc
  }

-------------------------
-- mmseqs_createdb_all --
-------------------------

mmseqsCreateDbAll :: RrrFunction
mmseqsCreateDbAll = let name = "mmseqs_createdb_all" in RrrFunction
  { fName      = name
  , fTypeDesc  = name ++ " : fa.list -> mms"
  , fTypeCheck = tMmseqsCreateDbAll name
  , fDesc      = Just "Create one MMSeqs2 sequence database from mutiple FASTA files."
  , fFixity    = Prefix
  , fRules     = rMmseqsCreateDbAll
  }

tMmseqsCreateDbAll :: String -> TypeChecker
tMmseqsCreateDbAll _ [(ListOf x)] | x `elem` [fna, faa] = Right mms
tMmseqsCreateDbAll name types = error $ name ++ " requires a list of fasta files, but got " ++ show types

rMmseqsCreateDbAll :: RulesFn
rMmseqsCreateDbAll s@(_, cfg, ref, _) e@(RrrFun _ _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr s fas
  let out    = exprPath s e
      out'   = debugRules cfg "rMmseqsCreateDbAll" e $ fromRrrPath cfg out
      dbDir  = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "createdb" -- TODO be more or less specific?
      dbPath = dbDir </> digest fas <.> "mmseqs2db" -- TODO take hash from somewhere else?
  out' %> \_ -> do
    unlessExists dbPath $ do -- TODO any reason it would exist already?
      faPaths <- readPaths cfg ref fasPath
      let faPaths' = map (fromRrrPath cfg) faPaths
      liftIO $ createDirectoryIfMissing True dbDir
      wrappedCmdWrite False True cfg ref out' [dbPath ++ "*"] [] [Cwd dbDir]
        "mmseqs" $ ["createdb"] ++ faPaths' ++ [dbPath]
    symlink cfg ref out $ toRrrPath cfg dbPath
  return (ExprPath out')
rMmseqsCreateDbAll _ e = error $ "bad argument to rMmseqsCreateDbAll: " ++ show e

---------------------
-- mmseqs_createdb --
---------------------

mmseqsCreateDb :: RrrFunction
mmseqsCreateDb = let name = "mmseqs_createdb" in RrrFunction
  { fName      = name
  , fTypeDesc  = name ++ " : fa -> mms"
  , fTypeCheck = tMmseqsCreateDb name
  , fDesc      = Just "Create an MMSeqs2 sequence database from a FASTA file."
  , fFixity    = Prefix
  , fRules     = rMmseqsCreateDb
  }

tMmseqsCreateDb :: String -> TypeChecker
tMmseqsCreateDb _ [x] | x `elem` [fna, faa] = Right mms
tMmseqsCreateDb name types = error $ name ++ " requires a fasta file, but got " ++ show types

rMmseqsCreateDb :: RulesFn
rMmseqsCreateDb s e = rMmseqsCreateDbAll s $ withSingleton e

----------------------
-- mmseqs_search_db --
----------------------

-- TODO for now this should also handle the convertalis step
-- TODO any reason to have a version that takes the query as a db? seems unnecessary

mmseqsSearchDb :: RrrFunction
mmseqsSearchDb = let name = "mmseqs_search_db" in RrrFunction
  { fName      = name
  , fTypeDesc  = name ++ " : num fa mms -> bht"
  , fTypeCheck = tMmseqsSearchDb name
  , fDesc      = Just "Search a target database for sequences matching the query FASTA, similar to BLAST."
  , fFixity    = Prefix
  , fRules     = rMmseqsSearchDb
  }

tMmseqsSearchDb :: String -> TypeChecker
-- TODO can this error be maintained despite the vague mms type? maybe not...
-- tMmseqsSearch n [x, y] | x == fna && y == fna = error $ "Both " ++ n ++ " args can't be fna; translate one first."
tMmseqsSearchDb _ [x, y, z] | x == num && y `elem` [fna, faa] && z == mms = Right bht
tMmseqsSearchDb n types = error $ n ++ " requires a number, fasta, and mmseqs2 db. Instead, got: " ++ show types

rMmseqsSearchDb :: RulesFn
rMmseqsSearchDb st@(_, cfg, ref, _) e@(RrrFun _ salt _ _ [n, q, s]) = do
  (ExprPath ePath) <- rExpr st n
  (ExprPath qPath) <- rExpr st $ RrrFun mms salt (depsOf q) "mmseqs_createdb" [q]
  (ExprPath sPath) <- rExpr st s -- note: the subject should already have been converted to a db
  let out    = exprPath st e
      out'   = debugRules cfg "rMmseqsSearch" e $ fromRrrPath cfg out
      -- dbDir  = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "search" </> digest e
      dbDir  = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "createdb" -- TODO be more or less specific?
      -- outDb' = dbDir </> "db" -- TODO error here?
      outDb' = dbDir </> digest out <.> "mmseqs2db" -- TODO does this hash match the ones from createdb_all?
  outDb' %> \_ -> aMmseqSearchDb    cfg ref ePath qPath sPath outDb'
  out'   %> \_ -> aMmseqConvertAlis cfg ref       qPath sPath outDb' out'
  return (ExprPath out')
rMmseqsSearchDb _ e = error $ "bad argument to rMmseqsSearch: " ++ show e

resolveMmseqsDb :: FilePath -> Action FilePath
resolveMmseqsDb path = do
  need [path] -- path is to a symlink to the main db file
  liftIO $ resolveSymlinks Nothing path

aMmseqSearchDb :: RrrConfig -> Locks -> FilePath -> FilePath -> FilePath -> FilePath -> Action ()
aMmseqSearchDb cfg ref ePath qDb sDb outDb = do
  qDb' <- resolveMmseqsDb qDb
  sDb' <- resolveMmseqsDb sDb
  eStr <- readLit cfg ref ePath
  let tmpDir = takeDirectory outDb </> "tmp" -- TODO align this with sonicparanoid
  liftIO $ createDirectoryIfMissing True tmpDir
  wrappedCmdWrite True True cfg ref outDb [ePath, qDb, sDb] [] []
    "mmseqs" ["search", "-e", eStr, qDb', sDb', outDb, tmpDir]
  liftIO $ removeDirectoryRecursive tmpDir

aMmseqConvertAlis :: RrrConfig -> Locks -> FilePath -> FilePath -> FilePath -> FilePath -> Action ()
aMmseqConvertAlis cfg ref qDb sDb outDb outTab = do
  qDb' <- resolveMmseqsDb qDb
  sDb' <- resolveMmseqsDb sDb
  oDb' <- resolveMmseqsDb outDb
  wrappedCmdWrite False True cfg ref outTab [qDb, sDb, outDb] [] [] "mmseqs"
    [ "convertalis"
    , qDb', sDb', oDb', outTab
    -- TODO check this matches my existing blast hit tables
    , "--format-output", "query target pident alnlen mismatch gapopen qstart qend tstart tend evalue bits"
    ]

-------------------
-- mmseqs_search --
-------------------

mmseqsSearch :: RrrFunction
mmseqsSearch = let name = "mmseqs_search" in RrrFunction
  { fName      = name
  , fTypeDesc  = name ++ " : num fa fa -> bht"
  , fTypeCheck = tMmseqsSearch name
  , fDesc      = Just "Find matching sequences in two fasta files, similar to BLAST."
  , fFixity    = Prefix
  , fRules     = rMmseqsSearch
  }

tMmseqsSearch :: String -> TypeChecker
tMmseqsSearch n [_, y, z] | y == fna && z == fna = error $ "Both " ++ n ++ " args can't be fna; translate one first."
tMmseqsSearch _ [x, y, z] | x == num && y `elem` [fna, faa] && z `elem` [fna, faa] = Right bht
tMmseqsSearch n types = error $ n ++ " requires a number and two fasta files. Instead, got: " ++ show types

rMmseqsSearch :: RulesFn
rMmseqsSearch st (RrrFun rtn salt deps name [e, q, s])
  = rules st (RrrFun rtn salt deps name [e, q, sDb])
  where
    rules = fRules mmseqsSearchDb
    sDb = RrrFun mms salt (depsOf s) "mmseqs_createdb" [s] 
rMmseqsSearch _ _ = error "bad argument to rMmseqsSearch"
