module ShortCut.Modules.BlastDB where

import Development.Shake
import ShortCut.Core.Types

-- import Control.Monad           (when)
import ShortCut.Core.Config    (wrappedCmd)
import ShortCut.Core.Debug     (debugAction, debugTrackWrite, debugRules)
import ShortCut.Core.Compile.Basic     (rExpr, defaultTypeCheck)
import ShortCut.Core.Compile.Map     (rMapLastTmp)
import ShortCut.Core.Paths (exprPath, cacheDir, fromCutPath, readLit, writeLit,
                            readLits, writePath, writeLits, toCutPath, CutPath)
import ShortCut.Core.Util      (stripWhiteSpace, digest)
import ShortCut.Modules.SeqIO  (faa, fna)
import System.FilePath         (takeFileName, takeBaseName, (</>), (<.>),
                                makeRelative, takeDirectory)
import System.Directory        (createDirectoryIfMissing)
-- import System.FilePath.Glob    (compile, globDir1)
import Data.List               (isInfixOf)
import Data.Char               (toLower)
-- import Path (fromCutPath cfg, fromCutPath cfg)
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
  let sPath' = toCutPath cfg sPath
  oPath' %> \_ -> aLoadDB cfg oPath sPath'
  return (ExprPath oPath')
  where
    oPath  = exprPath st e
    oPath' = fromCutPath cfg oPath
rLoadDB _ _ = error "bad argument to rLoadDB"

aLoadDB :: CutConfig -> CutPath -> CutPath -> Action ()
aLoadDB cfg oPath sPath = do
  pattern <- readLit cfg sPath'
  let pattern' = makeRelative (cfgTmpDir cfg) pattern -- TODO is this right??
  writeLit cfg oPath'' pattern'
  where
    oPath'  = fromCutPath cfg oPath
    sPath'  = fromCutPath cfg sPath
    oPath'' = debugAction cfg "aLoadDB" oPath' [oPath', sPath']

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
  oPath' %> \_ -> aBlastdblist cfg oPath tmpDir sTmp' fPath'
  return (ExprPath oPath')
  where
    oPath     = exprPath s e
    stdoutTmp = tmpDir' </> "stdout" <.> "txt"
    tmpDir    = cacheDir cfg "blastdbget"
    tmpDir'   = fromCutPath cfg tmpDir
    oPath'    = fromCutPath cfg oPath
    sTmp'     = toCutPath   cfg stdoutTmp
    fPath'    = toCutPath   cfg stdoutTmp
rBlastdblist _ _ = error "bad argument to rBlastdblist"

aBlastdblist :: CutConfig -> CutPath -> CutPath -> CutPath -> CutPath -> Action ()
aBlastdblist cfg oPath tmpDir stdoutTmp fPath = do
  wrappedCmd cfg [oPath'] [Shell] "blastdbget" [tmpDir', ">", fPath']
  -- TODO should this strip newlines on its own? seems important
  filterStr <- readLit  cfg fPath'
  out       <- readLits cfg stdoutTmp'
  let names = if null filterStr || null out
                then []
                else filterNames (init filterStr) (tail out)
  writeLits cfg oPath' names
  where
    fPath'     = fromCutPath cfg fPath
    oPath'     = fromCutPath cfg oPath
    tmpDir'    = fromCutPath cfg tmpDir
    stdoutTmp' = fromCutPath cfg stdoutTmp
    oPath'' = debugAction cfg "aBlastdblist" oPath' [oPath', tmpDir', stdoutTmp', fPath']

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
  let tmpDir    = cacheDir cfg "blastdbget"
      dbPrefix  = exprPath st e -- final prefix
      dbPrefix' = fromCutPath cfg dbPrefix
      nPath'    = toCutPath cfg nPath
  dbPrefix' %> \_ -> aBlastdbget cfg dbPrefix tmpDir nPath'
  return (ExprPath dbPrefix')
rBlastdbget _ _ = error "bad argument to rBlastdbget"

aBlastdbget :: CutConfig -> CutPath -> CutPath -> CutPath -> Action ()
aBlastdbget cfg dbPrefix tmpDir nPath = do
  need [nPath']
  dbName <- fmap stripWhiteSpace $ readLit cfg nPath' -- TODO need to strip?
  liftIO $ createDirectoryIfMissing True tmp' -- TODO remove?
  unit $ quietly $ wrappedCmd cfg [dbPrefix' ++ ".*"] [Cwd tmp']
    "blastdbget" ["-d", dbName, "."] -- TODO was taxdb needed for anything else?
  -- TODO switch to writePath
  writeLit cfg dbPrefix' $ tmp' </> dbName
  where
    tmp'       = fromCutPath cfg tmpDir
    nPath'     = fromCutPath cfg nPath
    dbPrefix'  = fromCutPath cfg dbPrefix
    dbPrefix'' = debugAction cfg "aBlastdbget" dbPrefix' [dbPrefix', tmp', nPath']

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
  let out       = exprPath s e
      out'      = debugRules cfg "rMakeblastdb" e $ fromCutPath cfg out
      cDir      = cacheDir cfg $ "makeblastdb" ++ dbType
      dbType    = if rtn == ndb then "_nucl" else "_prot"
      dbPrefix  = (fromCutPath cfg cDir) </> digest (exprPath s fa)
      dbPrefix' = toCutPath cfg dbPrefix
      faPath'   = toCutPath cfg faPath
  out' %> \_ -> aMakeblastdb rtn cfg cDir [out, dbPrefix', faPath']
  -- TODO what's up with the linking? just write the prefix to the outfile!
  return (ExprPath out')
rMakeblastdb _ _ = error "bad argument to makeblastdb"

aMakeblastdb :: CutType -> CutConfig -> CutPath -> [CutPath] -> Action ()
aMakeblastdb dbType cfg cDir [out, dbPrefix, faPath] = do
  -- TODO exprPath handles this now?
  -- let relDb = makeRelative (cfgTmpDir cfg) dbPrefix
  let dbType' = if dbType == ndb then "nucl" else "prot"
  liftIO $ putStrLn $ "dbPrefix': " ++ dbPrefix'
  liftIO $ putStrLn $ "dbType': " ++ dbType'
  need [faPath']
  -- TODO is cDir' actually used?
  -- liftIO $ createDirectoryIfMissing True cDir'
  liftIO $ createDirectoryIfMissing True dbDir
  quietly $ wrappedCmd cfg [dbPrefix', dbPrefix' ++ ".*"] [Cwd dbDir] "makeblastdb"
    [ "-in"    , faPath'
    , "-out"   , dbPrefix'
    , "-title" , takeFileName dbPrefix' -- TODO does this make sense?
    , "-dbtype", dbType'
    ]
  -- TODO put back if you can figure out how with the new wrappedCmd
  -- when (cfgDebug cfg) (liftIO $ putStrLn $ out)
  files <- fmap (map (cfgTmpDir cfg </>))
         $ getDirectoryFiles cDir' [takeBaseName dbPrefix' ++ ".*"]
  debugTrackWrite cfg files
  -- liftIO $ putStrLn $ "files: " ++ show files
  writePath cfg out'' dbPrefix
  where
    out'      = fromCutPath cfg out
    dbPrefix' = fromCutPath cfg dbPrefix
    cDir'     = takeDirectory $ fromCutPath cfg cDir
    dbDir     = takeDirectory dbPrefix'
    faPath'   = fromCutPath cfg faPath
    out'' = debugAction cfg "aMakeblastdb" out' [extOf dbType, out', dbPrefix', faPath']
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
rMakeblastdbEach dbType = rMapLastTmp (aMakeblastdb dbType) "makeblastdb"
