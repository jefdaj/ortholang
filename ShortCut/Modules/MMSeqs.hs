module ShortCut.Modules.MMSeqs
  where

-- TODO should search take an evalue cutoff?
-- TODO separate types for na vs aa databases? or just let it handle them?
-- TODO both subject and query fastas need to be made into dbs before searching
-- TODO and the dbs need prefixes like blast ones
-- TODO mmseqs_createdb : fa -> mms
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
import System.Directory            (createDirectoryIfMissing)
import System.FilePath             ((</>), (<.>))
import ShortCut.Core.Util          (digest, unlessExists)
import ShortCut.Modules.BlastDB    (withSingleton) -- TODO move to core?

cutModule :: CutModule
cutModule = CutModule
  { mName = "MMSeqs"
  , mDesc = "Many-against-many sequence searching: ultra fast and sensitive search and clustering suite."
  , mTypes = [faa, fna, mms]
  , mFunctions =
      [ mmseqscreatedb
      , mmseqscreatedbAll
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

mmseqscreatedbAll :: CutFunction
mmseqscreatedbAll = let name = "mmseqs_createdb_all" in CutFunction
  { fName      = name
  , fTypeDesc  = name ++ " : fa.list -> mms"
  , fTypeCheck = tMmseqscreatedbAll name
  , fDesc      = Just "Create one MMSeqs2 sequence database from mutiple FASTA files."
  , fFixity    = Prefix
  , fRules     = rMmseqscreatedbAll
  }

tMmseqscreatedbAll :: String -> TypeChecker
tMmseqscreatedbAll _ [(ListOf x)] | x `elem` [fna, faa] = Right mms
tMmseqscreatedbAll name types = error $ name ++ " requires a list of fasta files, but got " ++ show types

rMmseqscreatedbAll :: RulesFn
rMmseqscreatedbAll s@(_, cfg, ref) e@(CutFun _ _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr s fas
  let out      = exprPath s e
      out'     = debugRules cfg "rMmseqscreatedbAll" e $ fromCutPath cfg out
      dbDir    = cfgTmpDir cfg </> "cache" </> "mmseqs" </> digest fas -- TODO be more or less specific?
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
rMmseqscreatedbAll _ e = error $ "bad argument to rMmseqscreatedbAll: " ++ show e

---------------------
-- mmseqs_createdb --
---------------------

mmseqscreatedb :: CutFunction
mmseqscreatedb = let name = "mmseqs_createdb" in CutFunction
  { fName      = name
  , fTypeDesc  = name ++ " : fa -> mms"
  , fTypeCheck = tMmseqscreatedb name
  , fDesc      = Just "Create an MMSeqs2 sequence database from a FASTA file."
  , fFixity    = Prefix
  , fRules     = rMmseqscreatedb
  }

tMmseqscreatedb :: String -> TypeChecker
tMmseqscreatedb _ [x] | x `elem` [fna, faa] = Right mms
tMmseqscreatedb name types = error $ name ++ " requires a fasta file, but got " ++ show types

rMmseqscreatedb :: RulesFn
rMmseqscreatedb s e = rMmseqscreatedbAll s $ withSingleton e
