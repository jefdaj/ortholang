{-# LANGUAGE ScopedTypeVariables #-}

module Detourrr.Modules.BlastDB where

-- TODO should makeblastdb be just one fn? no, make everything else stricter later!
-- TODO need to remove tmpfiles in /tmp on quit to save space?

import Development.Shake
import Detourrr.Core.Types

import Data.Maybe                  (isJust)
import Control.Monad               (when, forM)
import Detourrr.Core.Actions       (wrappedCmdWrite, wrappedCmdExit,
                                    debugTrackWrite, readLit, readPaths, writeLit, readLits,
                                    writeLits, writePath, debugA, debugL, debugIO, debugNeed,
                                    cachedLinesPath, debugL, writeStrings, readStrings, writePaths)
import Detourrr.Core.Compile.Basic (rExpr, defaultTypeCheck, debugRules)
import Detourrr.Core.Paths         (exprPath, cacheDir, fromRrrPath,
                                    toRrrPath, RrrPath)
import Detourrr.Core.Util          (stripWhiteSpace, resolveSymlinks)
import Detourrr.Modules.SeqIO      (faa, fna)
import System.FilePath             (takeFileName, takeBaseName, (</>), (<.>),
                                    makeRelative, takeDirectory)
import Data.List                   (isInfixOf)
import Data.Char                   (toLower)
import System.Directory           (createDirectoryIfMissing)
import Detourrr.Core.Compile.Map2 (singleton)
import Detourrr.Core.Paths (fromGeneric)
import Detourrr.Core.Compile.Map (rMap)
import Detourrr.Core.Locks (withReadLock)
import System.Process
import Data.String.Utils (split)
import Data.List (isPrefixOf)

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

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "BlastDB"
  , mDesc = "Create, load, and download BLAST databases"
  , mTypes = [ndb, pdb]
  , mFunctions =

    [ loadNuclDB
    , loadProtDB
    , loadNuclDBEach
    , loadProtDBEach
    -- , mkMakeblastdb ndb
    -- , mkMakeblastdb pdb

    -- these are actually the most basic ones, because that way we can have
    -- only one main action function that takes a quoted list of paths, rather
    -- than that + a regular non-quoted one
    , makeblastdbNuclAll -- makeblastdb_nucl_all : fa.list  -> ndb
    , makeblastdbProtAll -- makeblastdb_prot_all : faa.list -> pdb

    -- these are implemented using the _all versions above and singleton lists
    -- you can make a nucl db from either, but a protein db only from faa.. backward?
    -- TODO replace most of the singleton lists in test rrrs with these
    , makeblastdbNucl    -- makeblastdb_nucl     : fa       -> ndb
    , makeblastdbProt    -- makeblastdb_prot     : faa      -> pdb

    -- these are used in the _rbh machinery
    -- they're a little weird because they are implemented using the non _all
    -- versions, so they internally make their args into lists of singleton lists
    , mkMakeblastdbEach ndb -- makeblastdb_nucl_each : fa.list  -> ndb.list
    , mkMakeblastdbEach pdb -- makeblastdb_prot_each : faa.list -> pdb.list

    , blastdbget -- TODO mapped version so you can list -> git at once?
    , blastdblist
    -- , TODO write loadBlastDB

    -- TODO hide this from users?
    , singletons
    ]
  }

ndb :: RrrType
ndb = RrrType
  { tExt  = "ndb"
  , tDesc = "BLAST nucleotide database"
  , tShow  = showBlastDb
  }

-- TODO will people confuse this with PDB files for viewing molecules?
pdb :: RrrType
pdb = RrrType
  { tExt  = "pdb"
  , tDesc = "BLAST protein database"
  , tShow  = showBlastDb
  }

---------------------
-- load from files --
---------------------

{- These are a little different from normal, because rather than linking
 - directly to a database file (there isn't one!), they create separate text
 - files that you can read to get the proper prefix pattern.
 -}

mkLoadDB :: String -> RrrType -> RrrFunction
mkLoadDB name rtn = RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name [str] rtn
  , fFixity    = Prefix
  , fRules  = rLoadDB
  }

mkLoadDBEach :: String -> RrrType -> RrrFunction
mkLoadDBEach name rtn = RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf str] (ListOf rtn)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [ListOf str] (ListOf rtn)
  , fFixity    = Prefix
  , fRules  = undefined -- TODO write this!
  }

rLoadDB :: RulesFn
rLoadDB st@(_, cfg, ref, ids) e@(RrrFun _ _ _ _ [s]) = do
  (ExprPath sPath) <- rExpr st s
  let sPath' = toRrrPath cfg sPath
  oPath' %> \_ -> aLoadDB cfg ref ids oPath sPath'
  return (ExprPath oPath')
  where
    oPath  = exprPath st e
    oPath' = fromRrrPath cfg oPath
rLoadDB _ _ = error "bad argument to rLoadDB"

aLoadDB :: RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> Action ()
aLoadDB cfg ref _ oPath sPath = do
  pattern <- readLit cfg ref sPath'
  let pattern' = makeRelative (cfgTmpDir cfg) pattern -- TODO is this right??
  writeLit cfg ref oPath'' pattern'
  where
    oPath'  = fromRrrPath cfg oPath
    sPath'  = fromRrrPath cfg sPath
    oPath'' = debugA cfg "aLoadDB" oPath' [oPath', sPath']

loadNuclDB :: RrrFunction
loadNuclDB = mkLoadDB "load_nucl_db" ndb

loadProtDB :: RrrFunction
loadProtDB = mkLoadDB "load_prot_db" pdb

loadNuclDBEach :: RrrFunction
loadNuclDBEach = mkLoadDBEach "load_nucl_db_each" ndb

loadProtDBEach :: RrrFunction
loadProtDBEach = mkLoadDBEach "load_prot_db_each" pdb

------------------------
-- download from NCBI --
------------------------

-- takes a filter string (leave empty for all results)
blastdblist :: RrrFunction
blastdblist = let name = "blastdblist" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] (ListOf str)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [str] (ListOf str)
  , fFixity    = Prefix
  , fRules     = rBlastdblist
  }

filterNames :: String -> [String] -> [String]
filterNames s cs = filter matchFn cs
  where
    matchFn c = (map toLower s) `isInfixOf` (map toLower c)

-- we use two different ones here because it matches the rMap behavior of using just fn name
blastdbgetCache :: RrrConfig -> RrrPath
blastdbgetCache cfg = cacheDir cfg "blastdbget"

-- we use two different ones here because it matches the rMap behavior of using just fn name
makeblastdbCache :: RrrConfig -> RrrPath
makeblastdbCache cfg = cacheDir cfg "makeblastdb"

rBlastdblist :: RulesFn
rBlastdblist s@(_, cfg, ref, ids) e@(RrrFun _ _ _ _ [f]) = do
  (ExprPath fPath) <- rExpr s f
  let fPath' = toRrrPath   cfg fPath
  listTmp %> \_ -> aBlastdblist   cfg ref ids lTmp'
  oPath'  %> \_ -> aBlastdbfilter cfg ref ids oPath lTmp' fPath'
  return (ExprPath oPath')
  where
    oPath   = exprPath s e
    tmpDir  = blastdbgetCache cfg
    tmpDir' = fromRrrPath cfg tmpDir
    listTmp = tmpDir' </> "dblist" <.> "txt"
    oPath'  = fromRrrPath cfg oPath
    lTmp'   = toRrrPath   cfg listTmp
rBlastdblist _ _ = error "bad argument to rBlastdblist"

aBlastdblist :: RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> Action ()
aBlastdblist cfg ref _ listTmp = do
  liftIO $ createDirectoryIfMissing True tmpDir
  _ <- wrappedCmdExit False True cfg ref (Just oPath) [] [Cwd tmpDir, Shell] -- TODO remove stderr?
    "blastdbget" [tmpDir, ">", listTmp'] [1]
  return ()
  where
    listTmp' = fromRrrPath cfg listTmp
    tmpDir   = takeDirectory $ listTmp'
    oPath    = debugA cfg "aBlastdblist" listTmp' [listTmp']

aBlastdbfilter :: RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> RrrPath -> Action ()
aBlastdbfilter cfg ref _ oPath listTmp fPath = do
  filterStr <- readLit  cfg ref fPath'
  out       <- readLits cfg ref listTmp'
  let names  = if null out then [] else tail out
      names' = if null filterStr then names else filterNames filterStr names
  debugL cfg $ "aBlastdbfilter names': " ++ show names'
  writeLits cfg ref oPath'' names'
  where
    fPath'   = fromRrrPath cfg fPath
    oPath'   = fromRrrPath cfg oPath
    listTmp' = fromRrrPath cfg listTmp
    oPath''  = debugA cfg "aBlastdbfilter" oPath' [oPath', listTmp', fPath']

blastdbget :: RrrFunction
blastdbget = let name = "blastdbget" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] ndb -- TODO are there protein ones too?
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [str] ndb -- TODO are there protein ones too?
  , fFixity    = Prefix
  , fRules  = rBlastdbget
  }

rBlastdbget :: RulesFn
rBlastdbget st@(_, cfg, ref, ids) e@(RrrFun _ _ _ _ [name]) = do
  (ExprPath nPath) <- rExpr st name
  let tmpDir    = blastdbgetCache cfg
      dbPrefix  = exprPath st e -- final prefix
      dbPrefix' = fromRrrPath cfg dbPrefix
      nPath'    = toRrrPath cfg nPath
  dbPrefix' %> \_ -> aBlastdbget cfg ref ids dbPrefix tmpDir nPath'
  return (ExprPath dbPrefix')
rBlastdbget _ _ = error "bad argument to rBlastdbget"

aBlastdbget :: RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> RrrPath -> RrrPath -> Action ()
aBlastdbget cfg ref _ dbPrefix tmpDir nPath = do
  debugNeed cfg "aBlastdbget" [nPath']
  dbName <- fmap stripWhiteSpace $ readLit cfg ref nPath' -- TODO need to strip?
  let dbPath = tmp' </> dbName
  liftIO $ createDirectoryIfMissing True tmp'
  -- TODO was taxdb needed for anything else?
  debugL cfg $ "aBlastdbget dbPrefix'': " ++ dbPrefix''
  debugL cfg $ "aBlastdbget dbPath: " ++ dbPath
  _ <- wrappedCmdWrite False True cfg ref dbPrefix'' [] [] [Cwd tmp']
         "blastdbget" ["-d", dbName, "."]
  writeLit cfg ref dbPrefix'' dbPath -- note this writes the path itself!
  where
    tmp'       = fromRrrPath cfg tmpDir
    nPath'     = fromRrrPath cfg nPath
    dbPrefix'  = fromRrrPath cfg dbPrefix
    dbPrefix'' = debugA cfg "aBlastdbget" dbPrefix' [dbPrefix', tmp', nPath']

--------------------------------------------
-- make one db from a list of FASTA files --
--------------------------------------------

-- TODO put the database files themselves in the cache dir and only prefix in exprs?

-- TODO silence output?
-- TODO does this have an error where db path depends on the outer expression
--      in addition to actual inputs?
makeblastdbNuclAll :: RrrFunction
makeblastdbNuclAll = RrrFunction
  { fName      = name
  , fTypeCheck = tMakeblastdbAll name ndb
  , fDesc = Nothing, fTypeDesc  = name ++ " : fa.list -> ndb"
  , fFixity    = Prefix
  , fRules     = rMakeblastdbAll
  }
  where
    name = "makeblastdb_nucl_all"

makeblastdbProtAll :: RrrFunction
makeblastdbProtAll = RrrFunction
  { fName      = name
  , fTypeCheck = tMakeblastdbAll name pdb
  , fDesc = Nothing, fTypeDesc  = name ++ " : faa.list -> pdb"
  , fFixity    = Prefix
  , fRules     = rMakeblastdbAll
  }
  where
    name = "makeblastdb_prot_all"

-- TODO allow fna.list -> pdb.list using translate?
tMakeblastdbAll :: String -> RrrType -> TypeChecker
tMakeblastdbAll _ dbType [ListOf faType]
  | dbType == pdb && faType   ==    faa       = Right pdb
  | dbType == ndb && faType `elem` [faa, fna] = Right dbType
tMakeblastdbAll name _ types = error $ name ++ " requires a list of fasta files, but got "
                                            ++ show types

-- TODO why does this get rebuilt one extra time, but *only* one?
-- TODO is rtn always the same as dbType?
-- TODO get the blast fn to need this!
-- <tmpdir>/cache/makeblastdb_<dbType>/<faHash>
rMakeblastdbAll :: RulesFn
rMakeblastdbAll s@(_, cfg, ref, ids) e@(RrrFun rtn _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr s fas
  let out       = exprPath s e
      out'      = debugRules cfg "rMakeblastdbAll" e $ fromRrrPath cfg out
      cDir      = makeblastdbCache cfg
      fasPath'   = toRrrPath cfg fasPath

  -- TODO need new shake first:
  -- out' %> \_ -> actionRetry 3 $ aMakeblastdbAll rtn cfg ref cDir [out, fasPath']

  out' %> \_ -> aMakeblastdbAll rtn cfg ref ids cDir [out, fasPath']
  -- TODO what's up with the linking? just write the prefix to the outfile!
  return (ExprPath out')
rMakeblastdbAll _ e = error $ "bad argument to rMakeblastdbAll: " ++ show e

listPrefixFiles :: FilePattern -> Action [FilePath]
listPrefixFiles prefix = liftIO (getDirectoryFilesIO pDir [pName]) >>= return . map (pDir </>)
  where
    pDir  = takeDirectory prefix
    pName = takeFileName  prefix

-- TODO why does this randomly fail by producing only two files?
-- TODO why is cDir just the top-level cache without its last dir component?
aMakeblastdbAll :: RrrType -> RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> [RrrPath] -> Action ()
aMakeblastdbAll dbType cfg ref _ cDir [out, fasPath] = do
  -- TODO exprPath handles this now?
  -- let relDb = makeRelative (cfgTmpDir cfg) dbOut
  let dbType' = if dbType == ndb then "nucl" else "prot"
  debugNeed cfg "aMakeblastdbAll" [fasPath']

  -- The idea was to hash content here, but it took a long time.
  -- So now it gets hashed only once, in another thread, by a load_* function,
  -- and from then on we pick the hash out of the filename.
  fasHash <- fmap takeBaseName $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) fasPath'

  let dbDir  = cDir' </> fasHash
      dbOut  = dbDir </> fasHash <.> extOf dbType
      dbOut' = toRrrPath cfg dbOut
      out''  = debugA cfg "aMakeblastdbAll" out' [extOf dbType, out', dbOut, fasPath']
      dbPtn  = cDir' </> fasHash </> "*" -- TODO does this actually help?

  -- Quoting is tricky here because makeblastdb expects multiple -in fastas to
  -- be passed as one quoted arg, but we also have to take into account Shake's
  -- built-in quoting and a possible wrapper script, which may in turn be
  -- running SLURM commands. These settings work on my system in all cases, but
  -- quoteInner may also be needed if you have spaces in your paths.... best
  -- solution is just to avoid that for now?
  --
  -- TODO would quoting JUST inner paths be right? And Shake does the outer ones?
  faPaths <- readPaths cfg ref fasPath'
  let noQuoting  = unwords $ map (fromRrrPath cfg) faPaths
      quoteOuter = "\"" ++ noQuoting ++ "\""
      fixedPaths = if isJust (cfgWrapper cfg) then quoteOuter else noQuoting
      -- quoteInner = "\"" ++ unwords
      --              (map (\p -> "'" ++ fromRrrPath cfg p ++ "'") faPaths)
      --              ++ "\""

  debugL cfg $ "aMakeblastdbAll out': "       ++ out'
  debugL cfg $ "aMakeblastdbAll cDir: "       ++ show cDir
  debugL cfg $ "aMakeblastdbAll cDir': "      ++ cDir'
  debugL cfg $ "aMakeblastdbAll dbOut': "     ++ show dbOut'
  debugL cfg $ "aMakeblastdbAll dbType': "    ++ dbType'
  debugL cfg $ "aMakeblastdbAll cfg: "        ++ show cfg
  debugL cfg $ "aMakeblastdbAll fixedPaths: " ++ show fixedPaths

  liftIO $ createDirectoryIfMissing True dbDir
  before <- listPrefixFiles dbPtn
  when (length before < 3) $ do
    debugL cfg $ "this is dbPtn: " ++ dbPtn
    debugL cfg $ "this will be dbOut: " ++ dbOut
    wrappedCmdWrite False True cfg ref out' [dbPtn] [] [Cwd cDir'] "makeblastdb"
      [ "-in"    , fixedPaths
      , "-out"   , dbOut
      , "-title" , takeFileName dbOut
      , "-dbtype", dbType'
      ]
    after <- listPrefixFiles dbPtn
    debugL cfg $ "these actual db files were created: " ++ show after
    when (length after < 3) (error "makeblastdb failed (< 3 db files created)")
    debugTrackWrite cfg after
    debugL cfg $ "dbOut was also created: " ++ dbOut
  -- TODO why should this work when outside the when block but not inside?? something about retries?
  writePath cfg ref out'' dbOut'
  where
    out'     = fromRrrPath cfg out
    cDir'    = fromRrrPath cfg cDir
    fasPath' = fromRrrPath cfg fasPath
aMakeblastdbAll _ _ _ _ _ paths = error $ "bad argument to aMakeblastdbAll: " ++ show paths

----------------------------------------
-- make a db from a single FASTA file --
----------------------------------------

-- these are oddly implemented in terms of the _all ones above,
-- because that turned out to be easier

makeblastdbNucl :: RrrFunction
makeblastdbNucl = RrrFunction
  { fName      = "makeblastdb_nucl"
  , fTypeCheck = tMakeblastdb ndb
  , fDesc = Nothing, fTypeDesc  = "makeblastdb_nucl : fa -> ndb"
  , fFixity    = Prefix
  , fRules     = rMakeblastdb
  }

makeblastdbProt :: RrrFunction
makeblastdbProt = RrrFunction
  { fName      = "makeblastdb_prot"
  , fTypeCheck = tMakeblastdb pdb
  , fDesc = Nothing, fTypeDesc  = "makeblastdb_prot : faa -> pdb"
  , fFixity    = Prefix
  , fRules     = rMakeblastdb
  }

tMakeblastdb :: RrrType -> TypeChecker
tMakeblastdb dbType [faType]
  | dbType == pdb && faType   ==    faa       = Right pdb
  | dbType == ndb && faType `elem` [faa, fna] = Right dbType
tMakeblastdb _ _ = error "makeblastdb requires a fasta file" -- TODO typed error

rMakeblastdb :: RulesFn
rMakeblastdb s e = rMakeblastdbAll s $ withSingleton e

-- TODO is this map1of1?
withSingleton :: RrrExpr -> RrrExpr
withSingleton (RrrFun rtn salt deps name [s])
  =           (RrrFun rtn salt deps name [singleton s])
withSingleton e = error $ "bad argument to withSingleton: " ++ show e

-----------------------------------------------
-- make list of dbs from list of FASTA files --
-----------------------------------------------

mkMakeblastdbEach :: RrrType -> RrrFunction
mkMakeblastdbEach dbType = RrrFunction
  { fName      = name
  , fTypeCheck = tMakeblastdbEach dbType
  , fDesc = Nothing, fTypeDesc  = desc
  , fFixity    = Prefix
  , fRules     = rMakeblastdbEach
  }
  where
    desc = name ++ " : " ++ ext ++ ".list -> " ++ extOf dbType ++ ".list"
    name = "makeblastdb" ++ (if dbType == ndb then "_nucl" else "_prot") ++ "_each"
    ext  = if dbType == ndb then "fa" else "faa"

-- TODO no! depends on an arg
tMakeblastdbEach :: RrrType -> TypeChecker
tMakeblastdbEach dbType [ListOf x] | x `elem` [fna, faa] = Right (ListOf dbType)
tMakeblastdbEach _ _ = error "expected a list of fasta files" -- TODO typed error

-- rFun1 :: Action1 -> RulesFn
-- rFun1 act1 st@(_, cfg, ref) expr@(RrrFun _ _ _ _ [a1]) = do

-- map1of1 :: RrrType -> RrrType -> Action1 -> Action1
-- map1of1 inType outType act1 cfg locks out a1 = do

-- rMap :: Int -> (RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()) -> RulesFn
-- rMap index actFn = rVecMain index Nothing actFn'

-- TODO this fails either either with map or vectorize, so problem might be unrelated?
rMakeblastdbEach :: RulesFn
rMakeblastdbEach st@(_, cfg, _, _) (RrrFun (ListOf dbType) salt deps name [e]) =
  -- rFun1 (map1of1 faType dbType act1) st expr'
  (rMap 1 act1) st expr'
  where
    -- faType = typeOf e
    tmpDir = makeblastdbCache cfg 
    -- act1 c r o a1 = aMakeblastdbAll dbType c r tmpDir [o, a1]
    act1 c r i = aMakeblastdbAll dbType c r i tmpDir -- TODO should be i right? not ids?
    expr' = RrrFun (ListOf dbType) salt deps name [withSingletons e]
    -- expr'' = trace ("expr':" ++ show expr') expr'
rMakeblastdbEach _ e = error $ "bad argument to rMakeblastdbEach" ++ show e

----------------
-- singletons --
----------------

-- TODO move this to its own module? remove it when possible?

withSingletons :: RrrExpr -> RrrExpr
withSingletons e = RrrFun (ListOf $ typeOf e) (saltOf e) (depsOf e) "singletons" [e]

-- Only used for the makeblastdb_*_each functions so far
-- TODO hide from users?
singletons :: RrrFunction
singletons = RrrFunction
  { fName      = name
  , fFixity    = Prefix
  , fDesc = Nothing, fTypeDesc  = name ++ " : X.list -> X.list.list"
  , fTypeCheck = tSingletons
  , fRules     = rSingletons
  }
  where
    name = "singletons"

tSingletons :: [RrrType] -> Either String RrrType
tSingletons [ListOf x] = Right $ ListOf $ ListOf x
tSingletons _ = Left "tSingletons expected a list"

rSingletons :: RulesFn
rSingletons st@(_, cfg, ref, ids) expr@(RrrFun rtn _ _ _ [listExpr]) = do
  (ExprPath listPath') <- rExpr st listExpr
  let outPath  = exprPath st expr
      outPath' = fromRrrPath cfg outPath
      listPath = toRrrPath cfg listPath'
      (ListOf (ListOf t)) = rtn
  outPath' %> \_ -> aSingletons t cfg ref ids outPath listPath
  return $ ExprPath outPath'
rSingletons _ _ = error "bad argument to rSingletons"

aSingletons :: RrrType -> Action1
aSingletons elemType cfg ref _ outPath listPath = do
  let listPath' = fromRrrPath cfg listPath
      outPath'  = fromRrrPath cfg outPath
  debugL cfg $ "aSingletons listpath': " ++ listPath'
  debugL cfg $ "aSingletons outpath': " ++ outPath'
  elems <- readStrings elemType cfg ref listPath'
  debugL cfg $ "aSingletons elems: " ++ show elems
  singletonPaths <- forM elems $ \e -> do
    let singletonPath' = cachedLinesPath cfg [e] -- TODO nondeterministic?
        singletonPath  = toRrrPath cfg singletonPath'
    debugL cfg $ "aSingletons singletonPath': " ++ singletonPath'
    writeStrings elemType cfg ref singletonPath' [e]
    return singletonPath
  writePaths cfg ref outPath' singletonPaths -- TODO nondeterministic?

------------------
-- show db info --
------------------

-- TODO remove the Volumes... lines too?
showBlastDb :: RrrConfig -> Locks -> FilePath -> IO String
showBlastDb cfg ref path = do
  path' <- fmap (fromGeneric cfg . stripWhiteSpace) $ readFile path
  let dbDir  = takeDirectory path'
      dbBase = takeFileName  path'
      args = ["-info", "-db", dbBase]
  debugIO cfg $ "showBlastDb dbDir: '" ++ dbDir ++ "'"
  debugIO cfg $ "showBlastDb args: " ++ show args
  out <- withReadLock ref path' $
           readCreateProcess (proc "blastdbcmd" args)
             { cwd = Just dbDir, env = Just [("BLASTDB", dbDir)] } ""
  let out1 = lines out
      out2 = concatMap (split "\t") out1
      out3 = filter (not . null) out2
      out4 = filter (\l -> not $ "Date" `isPrefixOf` l) out3
      out5 = unlines out4
  return out5