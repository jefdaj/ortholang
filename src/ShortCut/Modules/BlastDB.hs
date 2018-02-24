{-# LANGUAGE ScopedTypeVariables #-}

module ShortCut.Modules.BlastDB where

-- TODO should makeblastdb be just one fn? no, make everything else stricter later!

import Development.Shake
import ShortCut.Core.Types

-- import ShortCut.Core.Debug (debug)

import Control.Monad               (when)
import ShortCut.Core.Actions       (wrappedCmd, wrappedCmdWrite, wrappedCmdExit, wrappedCmdError,
                                    debugTrackWrite, readLit, writeLit, readLits,
                                    writeLits, writePath)
import ShortCut.Core.Debug         (debugAction, debugRules)
import ShortCut.Core.Compile.Basic (rExpr, defaultTypeCheck)
import ShortCut.Core.Compile.Each  (rEachTmp)
import ShortCut.Core.Paths         (exprPath, cacheDir, fromCutPath,
                                    toCutPath, CutPath)
import ShortCut.Core.Util          (stripWhiteSpace, resolveSymlinks)
import ShortCut.Modules.SeqIO      (faa, fna)
import System.FilePath             (takeFileName, takeBaseName, (</>), (<.>),
                                    makeRelative, takeDirectory)
import System.Directory            (createDirectoryIfMissing)
import Data.List                   (isInfixOf)
import Data.Char                   (toLower)
-- import System.Exit                 (ExitCode(..))

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

-- takes a filter string (leave empty for all results)
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

-- we use two different ones here because it matches the rEach behavior of using just fn name
blastdbgetCache :: CutConfig -> CutPath
blastdbgetCache cfg = cacheDir cfg "blastdbget"

-- we use two different ones here because it matches the rEach behavior of using just fn name
makeblastdbCache :: CutConfig -> CutPath
makeblastdbCache cfg = cacheDir cfg "makeblastdb"

rBlastdblist :: RulesFn
rBlastdblist s@(_,cfg) e@(CutFun _ _ _ _ [f]) = do
  (ExprPath fPath) <- rExpr s f
  let fPath' = toCutPath   cfg fPath
  oPath' %> \_ -> aBlastdblist cfg oPath lTmp' fPath'
  return (ExprPath oPath')
  where
    oPath   = exprPath s e
    tmpDir  = blastdbgetCache cfg
    tmpDir' = fromCutPath cfg tmpDir
    listTmp = tmpDir' </> "dblist" <.> "txt"
    oPath'  = fromCutPath cfg oPath
    lTmp'   = toCutPath   cfg listTmp
rBlastdblist _ _ = error "bad argument to rBlastdblist"

aBlastdblist :: CutConfig -> CutPath -> CutPath -> CutPath -> Action ()
aBlastdblist cfg oPath listTmp fPath = do
  done <- doesFileExist listTmp'
  when (not done) $ do
    liftIO $ createDirectoryIfMissing True tmpDir
    -- This one is tricky because it exits 1 on success
    -- TODO still use a lockfile and wrack writes!
    code <- wrappedCmdExit cfg listTmp' [] [Cwd tmpDir, Shell] -- TODO remove stderr?
      "blastdbget" [tmpDir, ">", listTmp']
    case code of
      0 -> debugTrackWrite cfg [listTmp'] -- never happens :(
      1 -> debugTrackWrite cfg [listTmp']
      n -> wrappedCmdError "blastdbget" n [listTmp'] -- TODO also the lockfile?
  filterStr <- readLit  cfg fPath'
  out       <- readLits cfg listTmp'
  let names  = if null out then [] else tail out
      names' = if null filterStr then names else filterNames filterStr names
  writeLits cfg oPath'' names'
  where
    fPath'   = fromCutPath cfg fPath
    oPath'   = fromCutPath cfg oPath
    listTmp' = fromCutPath cfg listTmp
    tmpDir   = takeDirectory $ listTmp'
    oPath''  = debugAction cfg "aBlastdblist" oPath' [oPath', tmpDir, listTmp', fPath']

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
  let tmpDir    = blastdbgetCache cfg
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
  -- TODO was taxdb needed for anything else?
  -- TODO does this need to lock on a separate file from dbPrefix''?
  -- TODO does it need to be given a dbPrefix'' + "*" pattern to delete on errors?
  -- TODO wrappedCmdWrite and delete the file on errors?
  _ <- wrappedCmd cfg dbPrefix'' [] [Cwd tmp'] "blastdbget" ["-d", dbName, "."]
  -- TODO switch to writePath
  writeLit cfg dbPrefix'' $ tmp' </> dbName
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

tMakeblastdb :: CutType -> TypeChecker
tMakeblastdb dbType [faType]
  | dbType == pdb && faType   ==    faa       = Right pdb
  | dbType == ndb && faType `elem` [faa, fna] = Right dbType
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
      cDir      = makeblastdbCache cfg
      -- dbType    = if rtn == ndb then "_nucl" else "_prot"
      -- dbPrefix  = (fromCutPath cfg cDir) </> digest (exprPath s fa)
      -- dbPrefix' = toCutPath cfg dbPrefix
      faPath'   = toCutPath cfg faPath
  -- out' %> \_ -> aMakeblastdb rtn cfg cDir [out, dbPrefix', faPath']
  out' %> \_ -> aMakeblastdb rtn cfg cDir [out, faPath']
  -- TODO what's up with the linking? just write the prefix to the outfile!
  return (ExprPath out')
rMakeblastdb _ _ = error "bad argument to makeblastdb"

listPrefixFiles :: FilePattern -> Action [FilePath]
listPrefixFiles prefix = do
  let pDir  = takeDirectory prefix
      pName = takeFileName  prefix
  e1 <- doesDirectoryExist pDir
  if e1
    then getDirectoryFiles pDir [pName] >>= return . map (pDir </>)
    else return []

-- TODO why is cDir just the top-level cache without its last dir component?
aMakeblastdb :: CutType -> CutConfig -> CutPath -> [CutPath] -> Action ()
aMakeblastdb dbType cfg cDir [out, faPath] = do
  -- TODO exprPath handles this now?
  -- let relDb = makeRelative (cfgTmpDir cfg) dbPrefix
  let dbType' = if dbType == ndb then "nucl" else "prot"

  -- TODO probably put this back??
  -- debug cfg ("needing: " ++ faPath') (need [faPath'])
  need [faPath']

  {- The idea was to hash content here, but it took a long time.
   - So now it gets hashed only once, in another thread, by a load_* function,
   - and from then on we pick the hash out of the filename.
   -
   - TODO oh no it goes one link too far still
   -}
  faHash <- fmap takeBaseName $ liftIO $ resolveSymlinks cfg True faPath'
  -- liftIO $ putStrLn $ "aMakeblastdb faHash: " ++ faHash
  let dbPrefix  = cDir' </> faHash </> faHash <.> extOf dbType
      dbPrefix' = toCutPath cfg dbPrefix
      out'' = debugAction cfg "aMakeblastdb" out'
                [extOf dbType, out', dbPrefix, faPath']
  liftIO $ createDirectoryIfMissing True cDir'

  -- TODO somehow out' still has the TMPDIR prefix in it! wtf?
  --      ah the cfgTmpDir itself is set to $TMPDIR! real wtf
  -- liftIO $ putStrLn $ "aMakeblastdb out': "      ++ out'
  -- liftIO $ putStrLn $ "aMakeblastdb cDir: "     ++ show cDir
  -- liftIO $ putStrLn $ "aMakeblastdb cDir': "     ++ cDir'
  -- liftIO $ putStrLn $ "aMakeblastdb dbPrefix': " ++ show dbPrefix'
  -- liftIO $ putStrLn $ "aMakeblastdb dbPrefix: "  ++ dbPrefix
  -- liftIO $ putStrLn $ "aMakeblastdb dbType': "   ++ dbType'
  -- liftIO $ putStrLn $ "aMakeblastdb cfg: "   ++ show cfg

  let ptn = dbPrefix ++ ".*" -- TODO is this failing because of the TMPDIR?
  -- before <- getDirectoryFiles (takeDirectory ptn) [takeFileName ptn]
  -- TODO is this picking up the .lock file too? maybe that causes the loop?
  before <- listPrefixFiles ptn
  -- liftIO $ putStrLn $ "before: " ++ show before

  -- TODO why can't shake detect that they exist already? am I not using trackWrite?
  when (not $ null before) (return ())

  -- TODO is there a need for wrapping or cleanup on errors here?
  -- TODO check files before too and skip if they exist? annoying but still...
  -- quietly $ wrappedCmd cfg [dbPrefix, dbPrefix ++ ".*"] [Cwd cDir']
  -- quietly $ wrappedCmd cfg dbPrefix [dbPrefix, ptn] [Cwd cDir'] "makeblastdb"
  -- TODO probably don't need the _lock then? bet it's a separate issue causing the loop
  -- liftIO $ putStrLn $ "this is ptn: " ++ ptn
  -- liftIO $ putStrLn $ "it matched these files: " ++ show before
  -- liftIO $ putStrLn $ "this will be dbPrefix: " ++ dbPrefix
  --
  -- TODO make wrappedCmd list files from a pattern and delete them as needed
  -- liftIO $ putStrLn $ "aMakeblastdb faPath': " ++ faPath'
  quietly $ wrappedCmdWrite cfg dbPrefix [faPath'] [ptn] [Cwd cDir'] "makeblastdb"
    [ "-in"    , faPath'
    , "-out"   , dbPrefix
    , "-title" , takeFileName dbPrefix -- TODO does this make sense?
    , "-dbtype", dbType'
    ]

  -- TODO why does after never work? maybe the command hasn't finished when called?
  -- when (cfgDebug cfg) (liftIO $ putStrLn $ out)
  -- after <- getDirectoryFiles (takeDirectory ptn) [takeFileName ptn]
  after <- listPrefixFiles ptn
  debugTrackWrite cfg after
  -- liftIO $ putStrLn $ "after: " ++ show after
  -- when (null after) (fail $ "makeblastdb failed to create " ++ ptn)
  writePath cfg out'' dbPrefix'
  where
    out'    = fromCutPath cfg out
    cDir'   = fromCutPath cfg cDir
    faPath' = fromCutPath cfg faPath
aMakeblastdb _ _ _ paths = error $ "bad argument to aMakeblastdb: " ++ show paths

--------------------------------
-- make many from FASTA files --
--------------------------------

mkMakeblastdbEach :: CutType -> CutFunction
mkMakeblastdbEach dbType = CutFunction
  { fName      = singleName ++ "_each"
  , fTypeCheck = tMakeblastdbEach dbType
  , fFixity    = Prefix
  , fRules  = rMakeblastdbEach dbType
  }
  where
    singleName = "makeblastdb" ++ if dbType == ndb then "_nucl" else "_prot"

-- TODO no! depends on an arg
tMakeblastdbEach :: CutType -> TypeChecker
tMakeblastdbEach dbType [ListOf x] | x `elem` [fna, faa] = Right (ListOf dbType)
tMakeblastdbEach _ _ = error "makeblastdb_each requires a list of fasta files" -- TODO typed error

rMakeblastdbEach :: CutType -> RulesFn
rMakeblastdbEach dbType = rEachTmp (aMakeblastdb dbType) "makeblastdb"
