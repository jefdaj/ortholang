module ShortCut.Modules.BlastDB where

import Development.Shake
import ShortCut.Core.Types

-- import Control.Monad           (when)
import ShortCut.Core.Config    (wrappedCmd)
import ShortCut.Core.Debug     (debugReadFile, debugWriteFile, debugReadLines,
                                debugWriteLines, debugAction, debugTrackWrite,
                                debugRules)
import ShortCut.Core.Compile.Rules     (rExpr, defaultTypeCheck, rMapLastTmp)
-- import ShortCut.Core.Compile.Paths     (exprPath, exprPathExplicit, cacheDir)
import ShortCut.Core.Compile.Paths2 (tmpToExpr, cacheDir2, exprHash)
import ShortCut.Core.Util      (stripWhiteSpace)
import ShortCut.Modules.SeqIO  (faa, fna)
import System.FilePath         (takeFileName, takeBaseName, (</>), (<.>),
                                makeRelative, takeDirectory)
import System.Directory        (createDirectoryIfMissing)
-- import System.FilePath.Glob    (compile, globDir1)
import Data.List               (isInfixOf)
import Data.Char               (toLower)
import Path (fromAbsFile, fromAbsDir)
-- import System.Exit             (ExitCode(..))

{- There are a few types of BLAST database files. For nucleic acids:
 - <prefix>.nhr, <prefix>.nin, <prefix>.nog, ...
 -
 - And for proteins:
 - <prefix>.phr, <prefix>.pin, <prefix>.pog, ...
 -
 - The BLAST programs just expect to be passed the prefix, which is fine for
 - most purposes but difficult in Shake; since it's not actually a file Shake
 - will complain that the Action failed to generate it. My solution for
 - now is to make a text file with the prefix pattern in it. The contents are
 - passed to BLAST functions.
 -
 - TODO does it work properly when the input fasta file changes and the database
 -      needs to be rebuilt?
 -}

cutModule :: CutModule
cutModule = CutModule
  { mName = "blastdb"
  , mFunctions =
    [ loadNuclDB
    , loadProtDB
    , loadNuclDBEach
    , loadProtDBEach
    -- , mkMakeblastdb ndb
    -- , mkMakeblastdb pdb
    , makeblastdbNucl
    , makeblastdbProt
    , mkMakeblastdbEach ndb
    , mkMakeblastdbEach pdb
    , blastdbget -- TODO mapped version so you can list -> git at once?
    , blastdblist
    -- , TODO write loadBlastDB
    ]
  }

ndb :: CutType
ndb = CutType
  { tExt  = "ndb"
  , tDesc = "BLAST nucleotide database"
  , tShow  = \f -> return $ "BLAST nucleotide database " ++ f
  }

-- TODO will people confuse this with PDB files for viewing molecules?
pdb :: CutType
pdb = CutType
  { tExt  = "pdb"
  , tDesc = "BLAST protein database"
  , tShow  = \f -> return $ "BLAST protein database " ++ f
  }

---------------------
-- load from files --
---------------------

{- These are a little different from normal, because rather than linking
 - directly to a database file (there isn't one!), they create separate text
 - files that you can read to get the proper prefix pattern.
 -}

mkLoadDB :: String -> CutType -> CutFunction
mkLoadDB name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fFixity    = Prefix
  , fRules  = rLoadDB
  }

mkLoadDBEach :: String -> CutType -> CutFunction
mkLoadDBEach name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf str] (ListOf rtn)
  , fFixity    = Prefix
  , fRules  = undefined -- TODO write this!
  }

rLoadDB :: RulesFn
rLoadDB st@(_,cfg) e@(CutFun _ _ _ _ [s]) = do
  (ExprPath sPath) <- rExpr st s
  let oPath = fromAbsFile $ tmpToExpr st e
  oPath %> \_ -> aLoadDB cfg oPath sPath 
  return (ExprPath oPath)
rLoadDB _ _ = error "bad argument to rLoadDB"

aLoadDB :: CutConfig -> FilePath -> FilePath -> Action ()
aLoadDB cfg oPath sPath = do
  pattern <- debugReadFile cfg sPath
  let pattern' = makeRelative (cfgTmpDir cfg) pattern
      oPath' = debugAction cfg "aLoadDB" oPath [oPath, sPath]
  debugWriteFile cfg oPath' pattern'

loadNuclDB :: CutFunction
loadNuclDB = mkLoadDB "load_nucl_db" ndb

loadProtDB :: CutFunction
loadProtDB = mkLoadDB "load_prot_db" pdb

loadNuclDBEach :: CutFunction
loadNuclDBEach = mkLoadDBEach "load_nucl_db_each" ndb

loadProtDBEach :: CutFunction
loadProtDBEach = mkLoadDBEach "load_prot_db_each" pdb

------------------------
-- download from NCBI --
------------------------

-- takes a filter string
blastdblist :: CutFunction
blastdblist = CutFunction
  { fName      = "blastdblist"
  , fTypeCheck = defaultTypeCheck [str] (ListOf str)
  , fFixity    = Prefix
  , fRules  = rBlastdblist
  }

filterNames :: String -> [String] -> [String]
filterNames s cs = filter matchFn cs
  where
    matchFn c = (map toLower s) `isInfixOf` (map toLower c)

rBlastdblist :: RulesFn
rBlastdblist s@(_,cfg) e@(CutFun _ _ _ _ [f]) = do
  (ExprPath fPath) <- rExpr s f
  let tmpDir = fromAbsDir  $ cacheDir2 cfg "blastdbget"
      oPath  = fromAbsFile $ tmpToExpr s e
      stdoutTmp = tmpDir </> "stdout" <.> "txt"
  oPath %> \_ -> aBlastdblist cfg oPath tmpDir stdoutTmp fPath
  return (ExprPath oPath)
rBlastdblist _ _ = error "bad argument to rBlastdblist"

aBlastdblist :: CutConfig -> String -> String -> FilePath -> String -> Action ()
aBlastdblist cfg oPath tmpDir stdoutTmp fPath = do
  wrappedCmd cfg [oPath] [Shell] "blastdbget" [tmpDir, ">", fPath]
  -- TODO should this strip newlines on its own? seems important
  filterStr <- debugReadFile  cfg fPath
  out       <- debugReadLines cfg stdoutTmp
  let names = if null filterStr || null out then []
              else filterNames (init filterStr) (tail out)
      oPath' = debugAction cfg "aBlastdblist" oPath [oPath, tmpDir, stdoutTmp, fPath]
  debugWriteLines cfg oPath' names

-- TODO do I need to adjust the timeout? try on the cluster first
blastdbget :: CutFunction
blastdbget = CutFunction
  { fName      = "blastdbget"
  , fTypeCheck = defaultTypeCheck [str] ndb -- TODO are there protein ones too?
  , fFixity    = Prefix
  , fRules  = rBlastdbget
  }

rBlastdbget :: RulesFn
rBlastdbget st@(_,cfg) e@(CutFun _ _ _ _ [name]) = do
  (ExprPath nPath) <- rExpr st name
  let tmpDir   = fromAbsDir  $ cacheDir2 cfg "blastdbget"
      dbPrefix = fromAbsFile $ tmpToExpr st e -- final prefix
  dbPrefix %> \_ -> aBlastdbget cfg dbPrefix tmpDir nPath
  return (ExprPath dbPrefix)
rBlastdbget _ _ = error "bad argument to rBlastdbget"

aBlastdbget :: CutConfig -> [Char] -> FilePath -> FilePath -> Action ()
aBlastdbget cfg dbPrefix tmpDir nPath = do
  need [nPath]
  dbName <- fmap stripWhiteSpace $ debugReadFile cfg nPath -- TODO need to strip?
  liftIO $ createDirectoryIfMissing True tmpDir -- TODO remove?
  let dbPrefix' = debugAction cfg "aBlastdbget" dbPrefix [dbPrefix, tmpDir, nPath]
  unit $ quietly $ wrappedCmd cfg [dbPrefix' ++ ".*"] [Cwd tmpDir]
    "blastdbget" ["-d", dbName, "."] -- TODO was taxdb needed for anything else?
  debugWriteFile cfg dbPrefix' $ (tmpDir </> dbName) ++ "\n"

--------------------------
-- make from FASTA file --
--------------------------

-- TODO put the database files themselves in the cache dir and only prefix in exprs?

-- TODO silence output?
-- TODO does this have an error where db path depends on the outer expression
--      in addition to actual inputs?
makeblastdbNucl :: CutFunction
makeblastdbNucl = CutFunction
  { fName      = "makeblastdb_nucl"
  , fTypeCheck = tMakeblastdb ndb
  , fFixity    = Prefix
  , fRules     = rMakeblastdb
  }

makeblastdbProt :: CutFunction
makeblastdbProt = CutFunction
  { fName      = "makeblastdb_prot"
  , fTypeCheck = tMakeblastdb pdb
  , fFixity    = Prefix
  , fRules     = rMakeblastdb
  }

-- TODO no! depends on an arg
tMakeblastdb :: CutType -> TypeChecker
tMakeblastdb dbType [x] | x `elem` [fna, faa] = Right dbType
tMakeblastdb _ _ = error "makeblastdb requires a fasta file" -- TODO typed error

-- TODO why does this get rebuilt one extra time, but *only* one?
-- TODO is rtn always the same as dbType?
-- TODO get the blast fn to need this!
-- <tmpdir>/cache/makeblastdb_<dbType>/<faHash>
rMakeblastdb :: RulesFn
rMakeblastdb s@(_, cfg) e@(CutFun rtn _ _ _ [fa]) = do
  (ExprPath faPath) <- rExpr s fa
  let out   = fromAbsFile $ tmpToExpr s e
      out'  = cfgTmpDir cfg </> out
      out'' = debugRules cfg "rMakeblastdb" e out'
      cDir  = fromAbsDir $ cacheDir2 cfg $ "makeblastdb" ++ dbType
      dbType   = if rtn == ndb then "_nucl" else "_prot"
      dbPrefix = cDir </> exprHash s fa
      -- cache = ".." </> ".." </> out -- TODO sytematize this
  out'' %> \_ -> aMakeblastdb rtn cfg (CacheDir cDir)
                      [ExprPath out'', ExprPath dbPrefix, ExprPath faPath]
  -- TODO what's up with the linking? just write the prefix to the outfile!
  return (ExprPath out'')
rMakeblastdb _ _ = error "bad argument to makeblastdb"

aMakeblastdb :: CutType -> ActionFn
aMakeblastdb dbType cfg (CacheDir cDir) [ExprPath out, ExprPath dbPrefix, ExprPath faPath] = do
  -- TODO tmpToExpr handles this now?
  let relDb = makeRelative (cfgTmpDir cfg) dbPrefix
      dbType' = if dbType == ndb then "nucl" else "prot"
  liftIO $ putStrLn $ "dbPrefix: " ++ dbPrefix
  liftIO $ putStrLn $ "dbType': " ++ dbType'
  need [faPath]
  let out' = debugAction cfg "aMakeblastdb" out
                         [extOf dbType, out, dbPrefix, faPath]
      dbDir = takeDirectory dbPrefix
  liftIO $ createDirectoryIfMissing True cDir
  quietly $ wrappedCmd cfg [dbPrefix, dbPrefix ++ ".*"] [Cwd dbDir] "makeblastdb"
    [ "-in"    , faPath
    , "-out"   , dbPrefix
    , "-title" , takeFileName dbPrefix -- TODO does this make sense?
    , "-dbtype", dbType'
    ]
  -- TODO put back if you can figure out how with the new wrappedCmd
  -- when (cfgDebug cfg) (liftIO $ putStrLn $ out)
  files <- fmap (map (cfgTmpDir cfg </>))
         $ getDirectoryFiles cDir [takeBaseName dbPrefix ++ ".*"]
  debugTrackWrite cfg files
  -- liftIO $ putStrLn $ "files: " ++ show files
  debugWriteLines cfg out' [relDb]
aMakeblastdb _ _ _ paths = error $ "bad argument to aMakeblastdb: " ++ show paths

--------------------------------
-- make many from FASTA files --
--------------------------------

mkMakeblastdbEach :: CutType -> CutFunction
mkMakeblastdbEach dbType = CutFunction
  { fName      = "makeblastdb" ++ (if dbType == ndb then "_nucl" else "_prot") ++ "_each"
  , fTypeCheck = tMakeblastdbEach dbType
  , fFixity    = Prefix
  , fRules  = rMakeblastdbEach dbType
  }

-- TODO no! depends on an arg
tMakeblastdbEach :: CutType -> TypeChecker
tMakeblastdbEach dbType [ListOf x] | x `elem` [fna, faa] = Right (ListOf dbType)
tMakeblastdbEach _ _ = error "makeblastdb_each requires a list of fasta files" -- TODO typed error

rMakeblastdbEach :: CutType -> RulesFn
rMakeblastdbEach dbType = rMapLastTmp (aMakeblastdb dbType) "makeblastdb" dbType
