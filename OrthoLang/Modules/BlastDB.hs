{-# LANGUAGE ScopedTypeVariables #-}

module OrthoLang.Modules.BlastDB where

-- TODO aha! errors are partially because i've been assuming blastdbget returns ndb when really it can be pdb?
--      should be fixable by determining the type from the .ni* files or whatever

-- TODO should makeblastdb be just one fn? no, make everything else stricter later!
-- TODO need to remove tmpfiles in /tmp on quit to save space?

import Development.Shake

import OrthoLang.Core
import OrthoLang.Locks
import OrthoLang.Modules.SeqIO      (faa, fna)
import OrthoLang.Modules.Singletons (withSingletons, withSingletonArg)

import Control.Monad           (when, forM)
import Data.Char               (toLower)
import Data.List               (isInfixOf)
import Data.List               (isPrefixOf)
import Data.Maybe              (isJust, fromJust)
import Data.String.Utils       (split)
import System.Directory        (createDirectoryIfMissing)
import System.Exit             (ExitCode(..))
import System.FilePath         (takeFileName, takeBaseName, (</>), (<.>), makeRelative, takeDirectory)
import System.Process          (readCreateProcess, proc)

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

debugA' :: String -> String -> Action ()
debugA' name = debugA ("modules.blastdb." ++ name)

debugR' :: (Pretty a, Show b) => Config -> String -> a -> b -> b
debugR' cfg name = debugRules ("modules.blastdb." ++ name)

olModule :: Module
olModule = Module
  { mName = "BlastDB"
  , mDesc = "Create, load, and download BLAST databases"
  , mTypes = [fna, faa, ndb, pdb]
  , mGroups = []
  , mEncodings = [blastdb]
  , mFunctions =

    [ loadFnaDb
    , loadFaaDb
    , loadFnaDbEach
    , loadFaaDbEach
    -- , mkMakeblastdb ndb
    -- , mkMakeblastdb pdb

    -- these are actually the most basic ones, because that way we can have
    -- only one main action function that takes a quoted list of paths, rather
    -- than that + a regular non-quoted one
    , makeblastdbFnaAll -- makeblastdb_fna_all : fa.list  -> ndb
    , makeblastdbFaaAll -- makeblastdb_faa_all : faa.list -> pdb

    -- these are implemented using the _all versions above and singleton lists
    -- you can make a nucl db from either, but a protein db only from faa.. backward?
    -- TODO replace most of the singleton lists in test cuts with these
    , makeblastdbFna    -- makeblastdb_fna     : fa       -> ndb
    , makeblastdbFaa    -- makeblastdb_faa     : faa      -> pdb

    -- these are used in the _rbh machinery
    -- they're a little weird because they are implemented using the non _all
    -- versions, so they internally make their args into lists of singleton lists
    , mkMakeblastdbEach fna -- makeblastdb_fna_each : fa.list  -> ndb.list
    , mkMakeblastdbEach faa -- makeblastdb_faa_each : faa.list -> pdb.list

    , blastdbgetFna -- TODO mapped version so you can list -> git at once?
    , blastdbgetFaa -- TODO mapped version so you can list -> git at once?
    , blastdblist
    -- , TODO write loadBlastDB
    ]
  }

-- TODO add a blastdb type group? seems natural but i'm not sure you ever need to mix them

blastdb :: Encoding
blastdb = Encoding
  { enExt = "blastdb"
  , enDesc = "NCBI BLAST+ sequence database"
  , enShow = undefined
  }

-- shorthand
ndb = EncodedAs blastdb fna
pdb = EncodedAs blastdb faa

-- TODO remove?
-- ndb :: Type
-- ndb = Type
--   { tExt  = "ndb"
--   , tDesc = "BLAST nucleotide database"
--   , tShow  = showBlastDb
--   }

-- TODO remove?
-- TODO will people confuse this with PDB files for viewing molecules?
-- pdb :: Type
-- pdb = Type
--   { tExt  = "pdb"
--   , tDesc = "BLAST protein database"
--   , tShow  = showBlastDb
--   }

---------------------
-- load from files --
---------------------

{- These are a little different from normal, because rather than linking
 - directly to a database file (there isn't one!), they create separate text
 - files that you can read to get the proper prefix pattern.
 -}

mkLoadDB :: String -> Type -> Function
mkLoadDB name faType = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [str] rtn
  -- , fTypeDesc  = mkTypeDesc name [str] rtn
  , fInputs = [Exactly str]
  , fOutput =  Exactly (EncodedAs blastdb faType)
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rLoadDB
  }

mkLoadDBEach :: String -> Type -> Function
mkLoadDBEach name faType = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [ListOf str] (ListOf rtn)
  -- , fTypeDesc  = mkTypeDesc name  [ListOf str] (ListOf rtn)
  , fInputs = [Exactly (ListOf str)]
  , fOutput =  Exactly (ListOf (EncodedAs blastdb faType))
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = undefined -- TODO write this!
  }

rLoadDB :: RulesFn
rLoadDB scr e@(Fun _ _ _ _ [s]) = do
  (ExprPath sPath) <- rExpr scr s
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let oPath  = exprPath cfg dRef scr e
      oPath' = fromPath cfg oPath
  let sPath' = toPath cfg sPath
  oPath' %> \_ -> aLoadDB oPath sPath'
  return (ExprPath oPath')
rLoadDB _ _ = fail "bad argument to rLoadDB"

aLoadDB :: Path -> Path -> Action ()
aLoadDB oPath sPath = do
  cfg <- fmap fromJust getShakeExtra
  let oPath'  = fromPath cfg oPath
      sPath'  = fromPath cfg sPath
      loc = "modules.blastdb.aLoadDB"
      oPath'' = traceA loc oPath' [oPath', sPath']
  pattern <- readLit loc sPath'
  let pattern' = makeRelative (cfgTmpDir cfg) pattern -- TODO is this right??
  writeLit loc oPath'' pattern'

loadFnaDb :: Function
loadFnaDb = mkLoadDB "load_fna_db" fna

loadFaaDb :: Function
loadFaaDb = mkLoadDB "load_faa_db" faa

loadFnaDbEach :: Function
loadFnaDbEach = mkLoadDBEach "load_fna_db_each" fna

loadFaaDbEach :: Function
loadFaaDbEach = mkLoadDBEach "load_faa_db_each" faa

------------------------
-- download from NCBI --
------------------------

-- takes a filter string (leave empty for all results)
blastdblist :: Function
blastdblist = let name = "blastdblist" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [str] (ListOf str)
  -- , fTypeDesc  = mkTypeDesc name  [str] (ListOf str)
  , fInputs = [Exactly str]
  , fOutput =  Exactly (ListOf str)
  , fTags = [ReadsURL]
  , fNewRules = NewNotImplemented, fOldRules = rBlastdblist
  }

filterNames :: String -> [String] -> [String]
filterNames s cs = filter matchFn cs
  where
    matchFn c = (map toLower s) `isInfixOf` (map toLower c)

-- we use two different ones here because it matches the rMap behavior of using just fn name
blastdbgetCache :: Config -> Path
blastdbgetCache cfg = cacheDir cfg "blastdbget"

-- we use two different ones here because it matches the rMap behavior of using just fn name
makeblastdbCache :: Config -> Path
makeblastdbCache cfg = cacheDir cfg "makeblastdb"

rBlastdblist :: RulesFn
rBlastdblist scr e@(Fun _ _ _ _ [f]) = do
  (ExprPath fPath) <- rExpr scr f
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let fPath' = toPath cfg fPath
      oPath   = exprPath cfg dRef scr e
      tmpDir  = blastdbgetCache cfg
      tmpDir' = fromPath cfg tmpDir
      listTmp = tmpDir' </> "dblist" <.> "txt"
      oPath'  = fromPath cfg oPath
      lTmp'   = toPath   cfg listTmp
  listTmp %> \_ -> aBlastdblist lTmp'
  oPath'  %> \_ -> aFilterList oPath lTmp' fPath'
  return (ExprPath oPath')
rBlastdblist _ _ = fail "bad argument to rBlastdblist"

aBlastdblist :: Path -> Action ()
aBlastdblist listTmp = do
  cfg <- fmap fromJust getShakeExtra
  let listTmp' = fromPath cfg listTmp
      tmpDir   = takeDirectory $ listTmp'
      oPath    = traceA "aBlastdblist" listTmp' [listTmp']
  liftIO $ createDirectoryIfMissing True tmpDir
  withWriteLock' tmpDir $ do
    runCmd $ CmdDesc
      { cmdParallel = False
      , cmdFixEmpties = True
      , cmdOutPath = oPath
      , cmdInPatterns = []
      , cmdExtraOutPaths = []
      , cmdSanitizePaths = []
      , cmdOptions =[Cwd tmpDir] -- TODO remove?
      , cmdBinary = "blastdblist.sh"
      , cmdArguments = [tmpDir, listTmp']
      , cmdRmPatterns = [] -- TODO remove tmpdir on fail? seems wasteful
      , cmdExitCode = ExitFailure 1
      }

-- TODO generalize so it works with busco_list_lineages too?
-- TODO move to a "Filter" module once that gets started
aFilterList :: Path -> Path -> Path -> Action ()
aFilterList oPath listTmp fPath = do
  cfg <- fmap fromJust getShakeExtra
  let fPath'   = fromPath cfg fPath
      oPath'   = fromPath cfg oPath
      listTmp' = fromPath cfg listTmp
      loc = "modules.blastdb.aFilterList"
      oPath''  = traceA loc oPath' [oPath', listTmp', fPath']
  filterStr <- readLit  loc fPath'
  out       <- readLits loc listTmp'
  let names  = if null out then [] else tail out
      names' = if null filterStr then names else filterNames filterStr names
  debugA' loc $ "names': " ++ show names'
  writeLits loc oPath'' names'

mkBlastdbget :: String -> Type -> Function
mkBlastdbget name faType = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [str] dbType -- TODO are there protein ones too?
  -- , fTypeDesc  = mkTypeDesc name  [str] dbType -- TODO are there protein ones too?
  , fInputs = [Exactly str]
  , fOutput =  Exactly (EncodedAs blastdb faType)
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rBlastdbget
  }

-- TODO rename with fna
blastdbgetFna :: Function
blastdbgetFna = mkBlastdbget "blastdbget_fna" fna

-- TODO rename with faa
blastdbgetFaa :: Function
blastdbgetFaa = mkBlastdbget "blastdbget_faa" faa

rBlastdbget :: RulesFn
rBlastdbget scr e@(Fun _ _ _ _ [name]) = do
  (ExprPath nPath) <- rExpr scr name
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let tmpDir    = blastdbgetCache cfg
      dbPrefix  = exprPath cfg dRef scr e -- final prefix
      dbPrefix' = fromPath cfg dbPrefix
      nPath'    = toPath cfg nPath
  dbPrefix' %> \_ -> aBlastdbget dbPrefix tmpDir nPath'
  return (ExprPath dbPrefix')
rBlastdbget _ _ = fail "bad argument to rBlastdbget"

aBlastdbget :: Path -> Path -> Path -> Action ()
aBlastdbget dbPrefix tmpDir nPath = do
  cfg <- fmap fromJust getShakeExtra
  let tmp'       = fromPath cfg tmpDir
      nPath'     = fromPath cfg nPath
      dbPrefix'  = fromPath cfg dbPrefix
      loc = "ortholang.modules.blastdb.aBlastdbget"
      dbPrefix'' = traceA loc dbPrefix' [dbPrefix', tmp', nPath']
  need' loc [nPath']
  dbName <- fmap stripWhiteSpace $ readLit loc nPath' -- TODO need to strip?
  let dbPath = tmp' </> dbName
  liftIO $ createDirectoryIfMissing True tmp'
  -- TODO was taxdb needed for anything else?
  debugA' "aBlastdbget" $ "dbPrefix'': " ++ dbPrefix''
  debugA' "aBlastdbget" $ "dbPath: " ++ dbPath
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = dbPrefix''
    , cmdInPatterns = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[Cwd tmp'] -- TODO remove?
    , cmdBinary = "blastdbget.sh"
    , cmdArguments = [tmp', dbName]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [] -- TODO remove tmpdir on fail? seems wasteful
    }
  writeLit loc dbPrefix'' dbPath -- note this writes the path itself!

--------------------------------------------
-- make one db from a list of FASTA files --
--------------------------------------------

-- TODO put the database files themselves in the cache dir and only prefix in exprs?

-- TODO silence output?
-- TODO does this have an error where db path depends on the outer expression
--      in addition to actual inputs?
makeblastdbFnaAll :: Function
makeblastdbFnaAll = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = tMakeblastdbAll name ndb
  -- , fTypeDesc  = name ++ " : fa.list -> ndb"
  , fInputs = [Exactly (ListOf fna)] -- TODO can this also take faas?
  , fOutput =  Exactly ndb
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMakeblastdbAll
  }
  where
    name = "makeblastdb_fna_all"

makeblastdbFaaAll :: Function
makeblastdbFaaAll = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = tMakeblastdbAll name pdb
  -- , fTypeDesc  = name ++ " : faa.list -> pdb"
  , fInputs = [Exactly (ListOf faa)]
  , fOutput = Exactly pdb
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMakeblastdbAll
  }
  where
    name = "makeblastdb_faa_all"

-- (ListOf (Some fa "a fasta file")) (Encoded blastdb (Some fa "a fasta file"))
-- shown as "fa.list -> fa.blastdb, where fa is a fasta file"
-- tMakeblastdbAll :: String -> Type -> TypeChecker
-- tMakeblastdbAll _ dbType [ListOf faType]
--   | dbType == pdb && faType   ==    faa       = Right pdb
--   | dbType == ndb && faType `elem` [faa, fna] = Right dbType
-- tMakeblastdbAll name _ types = error $ name ++ " requires a list of fasta files, but got "
--                                             ++ show types

-- TODO why does this get rebuilt one extra time, but *only* one?
-- TODO is rtn always the same as dbType?
-- TODO get the blast fn to need this!
-- <tmpdir>/cache/makeblastdb_<dbType>/<faHash>
rMakeblastdbAll :: RulesFn
rMakeblastdbAll scr e@(Fun rtn _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr scr fas
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let out       = exprPath cfg dRef scr e
      out'      = debugR' cfg "rMakeblastdbAll" e $ fromPath cfg out
      cDir      = makeblastdbCache cfg
      fasPath'   = toPath cfg fasPath

  -- TODO need new shake first:
  -- out' %> \_ -> actionRetry 3 $ aMakeblastdbAll rtn cfg ref cDir [out, fasPath']

  out' %> \_ -> aMakeblastdbAll rtn cDir [out, fasPath']
  -- TODO what's up with the linking? just write the prefix to the outfile!
  return (ExprPath out')
rMakeblastdbAll _ e = error $ "bad argument to rMakeblastdbAll: " ++ show e

listPrefixFiles :: FilePattern -> IO [FilePath]
listPrefixFiles prefix = getDirectoryFilesIO pDir [pName] >>= return . map (pDir </>)
  where
    pDir  = takeDirectory prefix
    pName = takeFileName  prefix

-- TODO why does this randomly fail by producing only two files?
-- TODO why is cDir just the top-level cache without its last dir component?
aMakeblastdbAll :: Type -> Path -> [Path] -> Action ()
aMakeblastdbAll dbType cDir [out, fasPath] = do
  -- TODO exprPath handles this now?
  -- let relDb = makeRel_ ative (cfgTmpDir cfg) dbOut
  cfg <- fmap fromJust getShakeExtra
  let out'     = fromPath cfg out
      cDir'    = fromPath cfg cDir
      fasPath' = fromPath cfg fasPath
  let dbType' = if dbType == ndb then "nucl" else "prot"
  need' "ortholang.modules.blastdb.aMakeblastdbAll" [fasPath']

  -- The idea was to hash content here, but it took a long time.
  -- So now it gets hashed only once, in another thread, by a load_* function,
  -- and from then on we pick the hash out of the filename.
  fasHash <- fmap takeBaseName $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) fasPath'

  let dbDir  = cDir' </> fasHash
      dbOut  = dbDir </> "result"
      dbOut' = toPath cfg dbOut
      loc = "modules.blastdb.aMakeblastdbAll"
      out''  = traceA loc out' [tExtOf dbType, out', dbOut, fasPath']
      dbPtn  = dbDir </> "*" -- TODO does this actually help?
      dbg = debugA' loc

  -- Quoting is tricky here because makeblastdb expects multiple -in fastas to
  -- be passed as one quoted arg, but we also have to take into account Shake's
  -- built-in quoting and a possible wrapper script, which may in turn be
  -- running SLURM commands. These settings work on my system in all cases, but
  -- quoteInner may also be needed if you have spaces in your paths.... best
  -- solution is just to avoid that for now?
  --
  -- TODO would quoting JUST inner paths be right? And Shake does the outer ones?
  faPaths <- readPaths loc fasPath'
  let noQuoting  = unwords $ map (fromPath cfg) faPaths
      quoteOuter = "\"" ++ noQuoting ++ "\""
      fixedPaths = if isJust (cfgWrapper cfg) then quoteOuter else noQuoting
      -- quoteInner = "\"" ++ unwords
      --              (map (\p -> "\"" ++ fromPath cfg p ++ "\"") faPaths)
      --              ++ "\""

  dbg $ "out': "       ++ out'
  dbg $ "cDir: "       ++ show cDir
  dbg $ "cDir': "      ++ cDir'
  dbg $ "dbOut': "     ++ show dbOut'
  dbg $ "dbType': "    ++ dbType'
  dbg $ "cfg: "        ++ show cfg
  dbg $ "fixedPaths: " ++ show fixedPaths
  dbg $ "dbPtn: "      ++ dbPtn
  dbg $ "dbOut: "      ++ dbOut

  -- liftIO $ createDirectoryIfMissing True dbDir
  -- before <- liftIO $ listPrefixFiles dbPtn
  -- when (length before < 5) $ do
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = dbOut
    , cmdInPatterns = [dbPtn]
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[]
    , cmdBinary = "makeblastdb.sh"
    , cmdArguments = [dbOut, fixedPaths, dbType']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [dbDir]
    }

  -- check that all the right files were created
  after <- liftIO $ listPrefixFiles dbPtn
  -- liftIO $ putStrLn "running makeblastdb"
  dbg $ "after: " ++ show after
  trackWrite' after

  -- usually there's an index file too, but not always
  -- TODO put these back? they sometimes fail when it splits into .00.pin etc.
  -- let expected = if dbType == ndb
  --                  then [".nhr", ".nin", ".nsq"]
  --                  else [".phr", ".pin", ".psq"]
  --     success = all (\e -> e `elem` (map takeExtension after)) expected
  -- dbg $ "these actual db files were created: " ++ show after
  -- unless success $ error $ "makeblastdb failed to create some database files: " ++ show after
    
  -- dbg $ "dbOut was also created: " ++ dbOut
  -- TODO why should this work when outside the when block but not inside?? something about retries?
  writePath loc out'' dbOut'
aMakeblastdbAll _ _ paths = error $ "bad argument to aMakeblastdbAll: " ++ show paths

----------------------------------------
-- make a db from a single FASTA file --
----------------------------------------

-- these are oddly implemented in terms of the _all ones above,
-- because that turned out to be easier

makeblastdbFna :: Function
makeblastdbFna = Function
  { fOpChar = Nothing, fName = "makeblastdb_fna"
  -- , fTypeCheck = tMakeblastdb ndb
  -- , fTypeDesc  = "makeblastdb_fna : fa -> ndb"
  , fInputs = [Exactly fna] -- TODO can't do it from faa right?
  , fOutput =  Exactly ndb
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMakeblastdb
  }

makeblastdbFaa :: Function
makeblastdbFaa = Function
  { fOpChar = Nothing, fName = "makeblastdb_faa"
  -- , fTypeCheck = tMakeblastdb pdb
  -- , fTypeDesc  = "makeblastdb_faa : faa -> pdb"
  , fInputs = [Exactly faa] -- TODO can't do it from faa right?
  , fOutput =  Exactly pdb
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMakeblastdb
  }

-- (Some fa "a fasta file") (Encoded blastdb (Some fa "a fasta file"))
-- shown as "fa -> fa.blastdb, where fa is a fasta file"
-- tMakeblastdb :: Type -> TypeChecker
-- tMakeblastdb dbType [faType]
--   | dbType == pdb && faType   ==    faa       = Right pdb
--   | dbType == ndb && faType `elem` [faa, fna] = Right dbType
-- tMakeblastdb _ _ = error "makeblastdb requires a fasta file" -- TODO typed error

rMakeblastdb :: RulesFn
rMakeblastdb s e = rMakeblastdbAll s $ withSingletonArg e

-----------------------------------------------
-- make list of dbs from list of FASTA files --
-----------------------------------------------

-- TODO convert to just one function that makes either kind of db? or let ppl choose the type
mkMakeblastdbEach :: Type -> Function
mkMakeblastdbEach faType = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = tMakeblastdbEach dbType
  -- , fTypeDesc  = desc
  , fInputs = [Exactly (ListOf faType)]
  , fOutput =  Exactly (ListOf (EncodedAs blastdb faType))
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMakeblastdbEach
  }
  where
    name = "makeblastdb_" ++ tExtOf faType ++ "_each" -- TODO change scripts to match!
    -- desc = name ++ " : " ++ ext ++ ".list -> " ++ tExtOf dbType ++ ".list"
    -- ext  = if dbType == ndb then "fa" else "faa"

-- (ListOf (Some fa "a fasta file")) (ListOf (Encoded blastdb (Some fa "a fasta file")))
-- shown as "fa.list -> fa.blastdb.list, where: fa is a fasta file"
-- tMakeblastdbEach :: Type -> TypeChecker
-- tMakeblastdbEach dbType [ListOf x] | x `elem` [fna, faa] = Right (ListOf dbType)
-- tMakeblastdbEach _ _ = error "expected a list of fasta files" -- TODO typed error

-- map1of1 :: Type -> Type -> Action1 -> Action1
-- map1of1 inType outType act1 cfg locks out a1 = do

-- rMap :: Int -> ([Path] -> Action ()) -> RulesFn
-- rMap index actFn = rMapMain index Nothing actFn'

-- TODO this fails either either with map or vectorize, so problem might be unrelated?
rMakeblastdbEach :: RulesFn
rMakeblastdbEach scr (Fun (ListOf dbType) salt deps name [e]) = do
  cfg <- fmap fromJust getShakeExtraRules
  let tmpDir = makeblastdbCache cfg 
      act1 = aMakeblastdbAll dbType tmpDir -- TODO should be i right? not ids?
      expr' = Fun (ListOf dbType) salt deps name [withSingletons e]
  (rMap 1 act1) scr expr'
rMakeblastdbEach _ e = error $ "bad argument to rMakeblastdbEach" ++ show e

------------------
-- show db info --
------------------

-- TODO remove the Volumes... lines too?
showBlastDb :: Config -> LocksRef -> FilePath -> IO String
showBlastDb cfg ref path = do
  path' <- fmap (fromGeneric cfg . stripWhiteSpace) $ readFileStrict ref path
  let dbDir  = takeDirectory path'
      -- dbBase = takeBaseName  dbDir
  debug "modules.blastdb.showBlastDb" $ "showBlastDb dbDir: \"" ++ dbDir ++ "\""
  out <- withReadLock ref path' $
           readCreateProcess (proc "blastdbcmd.sh" [dbDir]) ""
  let out1 = lines out
      out2 = concatMap (split "\t") out1
      out3 = filter (not . null) out2
      out4 = filter (\l -> not $ "Date" `isPrefixOf` l) out3
      out5 = unlines out4
  return out5
