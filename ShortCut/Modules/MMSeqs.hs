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

import ShortCut.Core.Actions       (readPaths, wrappedCmdWrite, symlink)
import ShortCut.Core.Compile.Basic (rExpr, debugRules)
import ShortCut.Core.Locks         (withReadLock)
import ShortCut.Core.Paths         (toCutPath, fromCutPath, exprPath)
import ShortCut.Modules.SeqIO      (fna, faa)
import ShortCut.Modules.Blast      (bht)
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             ((</>), (<.>))
import ShortCut.Core.Util          (digest, unlessExists)
import ShortCut.Modules.BlastDB    (withSingleton) -- TODO move to core?

cutModule :: CutModule
cutModule = CutModule
  { mName = "MMSeqs"
  , mDesc = "Many-against-many sequence searching: ultra fast and sensitive search and clustering suite."
  , mTypes = [faa, fna, bht, mms]
  , mFunctions =
      [ mmseqsCreateDb
      , mmseqsCreateDbAll
      -- , mmseqsConvertalis
      -- , mmseqsSearch
      -- , mmseqsSearchDb
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
    unlessExists dbPath $ do
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

------------------------
-- mmseqs_convertalis --
------------------------

-- TODO write this as a separate function, then use it as part of the main search

----------------------
-- mmseqs_search_db --
----------------------

-- TODO for now this should also handle the convertalis step
-- TODO any reason to have a version that takes the query as a db? seems unnecessary

-- mmseqsSearchDb :: CutFunction
-- mmseqsSearchDb = let name = "mmseqs_search_db" in CutFunction
--   { fName      = name
--   , fTypeDesc  = name ++ " : fa mms -> bht"
--   , fTypeCheck = tMmseqsSearchDb name
--   , fDesc      = Just "Search a target database for sequences matching the query FASTA, similar to BLAST."
--   , fFixity    = Prefix
--   , fRules     = rMmseqsSearchDb
--   }

-- tMmseqsSearchDb :: String -> TypeChecker
-- -- TODO can this error be maintained despite the vague mms type? maybe not...
-- -- tMmseqsSearch n [x, y] | x == fna && y == fna = error $ "Both " ++ n ++ " args can't be fna; translate one first."
-- tMmseqsSearchDb _ [x, y] | x `elem` [fna, faa] && y == mms = Right bht
-- tMmseqsSearchDb n types = error $ n ++ " requires fasta and mmseqs2 db files, but got " ++ show types

-- rMmseqsSearchDb :: RulesFn
-- -- rMkBlastFromFa d@(_, _, _, dbType) st (CutFun rtn salt deps _ [e, q, s])
-- -- aMkBlastFromDb bCmd cfg ref [o, e, q, p] = do
-- rMmseqsSearchDb (_, cfg, ref) e@(CutFun _ _ _ _ [n, q, s]) = do
--   (ExprPath nPath) <- rExpr cfg n
--   (ExprPath qPath) <- rExpr cfg q -- TODO this one is converted to db automatically
--   (ExprPath sPath) <- rExpr cfg s -- TODO but this one is assumed to have been done already
--   qDb    <- undefined -- TODO wrap these in createdb calls and compile
--   sDb    <- undefined -- TODO wrap these in createdb calls and compile
-- 
--   let out   = exprPath s e
--       outDb = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "search" </> digest e
--       out'  = debugRules cfg "rMmseqsSearch" e $ fromCutPath cfg out
-- 
--   outDb %> \_ -> aMmseqsearch      cfg ref nPath qPath sPath outDbPath
--   out'  %> \_ -> aMmseqconvertalis cfg ref       qPath sPath outDbPath out'
-- 
--   return (ExprPath out')
--   -- where
--     -- nDb'  = fromCutPath cfg nDb
-- rMmseqsSearchDb _ e = error $ "bad argument to rMmseqsSearch: " ++ show e

-- note this only does the search itself; the next fn converts output to bht format
-- aMmseqsearchDb cfg ref qDb sDb outDb = undefined

-- aMmseqconvertalis cfg ref qDb sDb outDb outTab = do
--   wrappedCmdWrite False True cfg ref outTab [] [] []
--     "mmseqs" ["convertalis", qDb, sDb, outDb, outTab]

-------------------
-- mmseqs_search --
-------------------

-- mmseqsSearch :: CutFunction
-- mmseqsSearch = let name = "mmseqs_search" in CutFunction
--   { fName      = name
--   , fTypeDesc  = name ++ " : fa fa -> bht"
--   , fTypeCheck = tMmseqsCreateDb name
--   , fDesc      = Just "Find matching sequences in two fasta files, similar to BLAST."
--   , fFixity    = Prefix
--   , fRules     = rMmseqsSearch
--   }
-- 
-- 
-- rMkBlastFromFa :: BlastDesc -> RulesFn
-- rMkBlastFromFa d@(_, _, _, dbType) st (CutFun rtn salt deps _ [e, q, s])
--   = rules st (CutFun rtn salt deps name1 [e, q, dbExpr])
--   where
--     rules = fRules $ mkBlastFromDb d
--     name1 = fName  $ mkBlastFromDb d
--     name2 = "makeblastdb" ++ if dbType == ndb then "_nucl" else "_prot"
--     dbExpr = CutFun dbType salt (depsOf s) name2 [s] 
-- rMkBlastFromFa _ _ _ = error "bad argument to rMkBlastFromFa"


