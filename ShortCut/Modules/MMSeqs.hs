module ShortCut.Modules.MMSeqs
  where

-- TODO should search take an evalue cutoff?
-- TODO separate types for na vs aa databases? or just let it handle them?
-- TODO both subject and query fastas need to be made into dbs before searching
-- TODO and the dbs need prefixes like blast ones
-- TODO mmseqs_search : fa mms -> mmr (this is mmseqs search with a singleton list)
-- TODO mmseqs_search_all : fa.list mms -> mmr (this is mmseqs search)
-- TODO mmseqs_search_profile?
-- TODO mmseqs_cluster
-- TODO mmseqs_linclust
-- TODO mmseqs_clusterupdate?
-- TODO mmseqs_taxonomy? maybe later
-- TODO rewrite the db printing once you figure out how to view results
-- TODO mmh type for mmseqs result/hits db?
-- TODO might need to provide a giant tmpdir when it goes on the server
-- TODO should we createindex before searching? not yet for small stuff
-- TODO caution that you can't use fna as both subject and query, just one or the other
--      (that can be done but requires a separate script with a few steps)

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions       (readLit, readPaths, wrappedCmdWrite, symlink)
import ShortCut.Core.Compile.Basic (rExpr, debugRules)
import ShortCut.Core.Locks         (withReadLock)
import ShortCut.Core.Paths         (toCutPath, fromCutPath, exprPath)
import ShortCut.Modules.SeqIO      (fna, faa)
import ShortCut.Modules.Blast      (bht)
import System.Directory            (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath             ((</>), (<.>), dropExtension, takeDirectory)
import ShortCut.Core.Util          (digest, unlessExists, resolveSymlinks)
import ShortCut.Modules.BlastDB    (withSingleton) -- TODO move to core?

cutModule :: CutModule
cutModule = CutModule
  { mName = "MMSeqs"
  , mDesc = "Many-against-many sequence searching: ultra fast and sensitive search and clustering suite."
  , mTypes = [faa, fna, bht, mms]
  , mFunctions =
      [ mmseqsCreateDbAll
      , mmseqsCreateDb
      , mmseqsSearchDb
      , mmseqsSearch
      ]
  }

-- note that this has an internal nucleic or amino acid type, but the program always handles either
mms :: CutType
mms = CutType
  { tExt  = "mms"
  , tDesc = "MMSeqs2 sequence database"
  , tShow = \_ ref path -> do
      h5 <- fmap (take 5 . lines) $ withReadLock ref path $ readFile path
      let desc = unlines $ ["MMSeqs2 sequence database " ++ path ++ ":"] ++ h5 ++ ["..."]
      return desc
  }

-------------------------
-- mmseqs_createdb_all --
-------------------------

mmseqsCreateDbAll :: CutFunction
mmseqsCreateDbAll = let name = "mmseqs_createdb_all" in CutFunction
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
rMmseqsCreateDbAll s@(_, cfg, ref) e@(CutFun _ _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr s fas
  let out      = exprPath s e
      out'     = debugRules cfg "rMmseqsCreateDbAll" e $ fromCutPath cfg out
      dbDir    = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "createdb" </> digest fas -- TODO be more or less specific?
      dbPrefix = dbDir </> "db" -- TODO any reason for a more descriptive name?
      dbPath   = dbPrefix <.> "lookup"
  out' %> \_ -> do
    unlessExists dbPath $ do -- TODO any reason it would exist already?
      faPaths <- readPaths cfg ref fasPath
      let faPaths' = map (fromCutPath cfg) faPaths
      liftIO $ createDirectoryIfMissing True dbDir
      wrappedCmdWrite False True cfg ref out' [dbPrefix ++ "*"] [] [Cwd dbDir]
        "mmseqs" $ ["createdb"] ++ faPaths' ++ [dbPrefix]
    symlink cfg ref out $ toCutPath cfg dbPath
  return (ExprPath out')
rMmseqsCreateDbAll _ e = error $ "bad argument to rMmseqsCreateDbAll: " ++ show e

---------------------
-- mmseqs_createdb --
---------------------

mmseqsCreateDb :: CutFunction
mmseqsCreateDb = let name = "mmseqs_createdb" in CutFunction
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

mmseqsSearchDb :: CutFunction
mmseqsSearchDb = let name = "mmseqs_search_db" in CutFunction
  { fName      = name
  , fTypeDesc  = name ++ " : fa mms -> bht"
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

-- TODO maybe the .lookup file isn't always there.. depend on the main db file instead?
rMmseqsSearchDb :: RulesFn
-- rMkBlastFromFa d@(_, _, _, dbType) st (CutFun rtn salt deps _ [e, q, s])
-- aMkBlastFromDb bCmd cfg ref [o, e, q, p] = do
rMmseqsSearchDb st@(_, cfg, ref) e@(CutFun _ salt _ _ [n, q, s]) = do
  -- TODO remove the .lookup from these to get prefixes
  -- TODO and make them real paths from cutpaths
  (ExprPath ePath) <- rExpr st n
  (ExprPath qPath) <- rExpr st $ CutFun mms salt (depsOf q) "mmseqs_createdb" [q]
  (ExprPath sPath) <- rExpr st s -- note: the subject should already have been converted to a db
  -- let qPath' = dropExtension qPath
  --     sPath' = dropExtension sPath
  let out    = exprPath st e
      out'   = debugRules cfg "rMmseqsSearch" e $ fromCutPath cfg out

      -- TODO rework this similarly to the createdb_all one that works well above
      outDbDir = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "search" </> digest e
      outDb' = outDbDir </> "db" -- TODO error here?

  outDb' %> \_ -> aMmseqSearchDb    cfg ref ePath qPath sPath outDb'
  out'   %> \_ -> aMmseqConvertAlis cfg ref       qPath sPath outDb' out'
  return (ExprPath out')
rMmseqsSearchDb _ e = error $ "bad argument to rMmseqsSearch: " ++ show e

-- TODO symlink to main db file instead, because that will always exist; print using .lookup later
resolveMmseqsDb :: FilePath -> Action FilePath
resolveMmseqsDb path = do
  need [path] -- path is to a symlink to the .lookup file
  liftIO $ fmap dropExtension $ resolveSymlinks Nothing path

aMmseqSearchDb :: CutConfig -> Locks -> FilePath -> FilePath -> FilePath -> FilePath -> Action ()
aMmseqSearchDb cfg ref ePath qDb sDb outDb = do
  qDb' <- resolveMmseqsDb qDb
  sDb' <- resolveMmseqsDb sDb
  eStr <- readLit cfg ref ePath
  let tmpDir = takeDirectory outDb </> "tmp"
  liftIO $ createDirectoryIfMissing True tmpDir
  wrappedCmdWrite True True cfg ref outDb [ePath, qDb, sDb] [] []
    "mmseqs" ["search", "-e", eStr, qDb', sDb', outDb, tmpDir]
  liftIO $ removeDirectoryRecursive tmpDir

aMmseqConvertAlis :: CutConfig -> Locks -> FilePath -> FilePath -> FilePath -> FilePath -> Action ()
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

mmseqsSearch :: CutFunction
mmseqsSearch = let name = "mmseqs_search" in CutFunction
  { fName      = name
  , fTypeDesc  = name ++ " : fa fa -> bht"
  , fTypeCheck = tMmseqsCreateDb name
  , fDesc      = Just "Find matching sequences in two fasta files, similar to BLAST."
  , fFixity    = Prefix
  , fRules     = undefined
  }


-- rMkBlastFromFa :: BlastDesc -> RulesFn
-- rMkBlastFromFa d@(_, _, _, dbType) st (CutFun rtn salt deps _ [e, q, s])
--   = rules st (CutFun rtn salt deps name1 [e, q, dbExpr])
--   where
--     rules = fRules $ mkBlastFromDb d
--     name1 = fName  $ mkBlastFromDb d
--     name2 = "makeblastdb" ++ if dbType == ndb then "_nucl" else "_prot"
--     dbExpr = CutFun dbType salt (depsOf s) name2 [s] 
-- rMkBlastFromFa _ _ _ = error "bad argument to rMkBlastFromFa"
