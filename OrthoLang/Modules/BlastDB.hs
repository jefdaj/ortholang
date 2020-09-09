{-# LANGUAGE ScopedTypeVariables #-}

module OrthoLang.Modules.BlastDB where

-- TODO shen showing db, make paths generic. or just remove the volumes lines completely?

-- TODO aha! errors are partially because i've been assuming blastdbget returns ndb when really it can be pdb?
--      should be fixable by determining the type from the .ni* files or whatever

-- TODO should makeblastdb be just one fn? no, make everything else stricter later!
-- TODO need to remove tmpfiles in /tmp on quit to save space?

import Development.Shake

import OrthoLang.Types
import OrthoLang.Locks
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO      (faa, fna, fa)
import OrthoLang.Modules.Singletons (withSingleton, singletons)

-- import Control.Monad           (when, forM)
import Data.Char               (toLower)
import Data.List               (isInfixOf)
import Data.List               (isPrefixOf)
import Data.Maybe              (isJust, fromJust)
import Data.String.Utils       (split)
import System.Directory        (createDirectoryIfMissing)
import System.Exit             (ExitCode(..))
import System.FilePath         (takeBaseName, (</>), (<.>), makeRelative, takeDirectory)
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
debugR' _ name = debugRules ("modules.blastdb." ++ name)

olModule :: Module
olModule = Module
  { mName = "BlastDB"
  , mDesc = "Create, load, and download BLAST databases"
  , mTypes = [fna, faa, ndb, pdb]
  , mGroups = []
  , mEncodings = [blastdb]
  , mRules = return ()
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
    , makeblastdbNuclAll -- makeblastdb_fna_all : fa.list  -> ndb
    , makeblastdbProtAll -- makeblastdb_faa_all : faa.list -> pdb

    -- these are implemented using the _all versions above and singleton lists
    -- you can make a nucl db from either, but a protein db only from faa.. backward?
    -- TODO replace most of the singleton lists in test cuts with these
    , makeblastdbNucl    -- makeblastdb_fna     : fa       -> ndb
    , makeblastdbProt    -- makeblastdb_faa     : faa      -> pdb

    -- these are used in the _rbh machinery
    -- they're a little weird because they are implemented using the non _all
    -- versions, so they internally make their args into lists of singleton lists
    , mkMakeblastdbNuclEach -- makeblastdb_fna_each : fa.list  -> ndb.list
    , mkMakeblastdbProtEach -- makeblastdb_faa_each : faa.list -> pdb.list
    , mkMakeblastdbNuclAllEach
    , mkMakeblastdbProtAllEach

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
  , enShow = showBlastDb
  }

-- shorthand
ndb :: Type
ndb = EncodedAs blastdb fna

pdb :: Type
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
mkLoadDB name faType = newFnA1
  name
  (Exactly str)
  (Exactly $ EncodedAs blastdb faType)
  aLoadDB
  []

mkLoadDBEach :: String -> Type -> Function
mkLoadDBEach name faType = newFnA1
  (name ++ "_each")
  (Exactly $ ListOf str)
  (Exactly $ ListOf $ EncodedAs blastdb faType)
  (newMap1of1 name)
  []

aLoadDB :: NewAction1
aLoadDB (ExprPath oPath') sPath' = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.blastdb.aLoadDB"
      oPath'' = traceA loc oPath' [oPath', sPath']
  pattern <- readLit loc sPath'
  let pattern' = makeRelative (tmpdir cfg) pattern -- TODO is this right??
  writeLit loc oPath'' pattern'

loadFnaDb :: Function
loadFnaDb = mkLoadDB "load_fna_db" fna

loadFaaDb :: Function
loadFaaDb = mkLoadDB "load_faa_db" faa

loadFnaDbEach :: Function
loadFnaDbEach = mkLoadDBEach "load_fna_db" fna

loadFaaDbEach :: Function
loadFaaDbEach = mkLoadDBEach "load_faa_db" faa

------------------------
-- download from NCBI --
------------------------

-- takes a filter string (leave empty for all results)
blastdblist :: Function
blastdblist = let name = "blastdblist" in Function
  { fOpChar = Nothing, fName = name
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
  let loc = "modules.blastdb.rBlastdblist"
      fPath' = toPath loc cfg fPath
      oPath   = exprPath cfg dRef scr e
      tmpDir  = blastdbgetCache cfg
      tmpDir' = fromPath loc cfg tmpDir
      listTmp = tmpDir' </> "dblist" <.> "txt"
      oPath'  = fromPath loc cfg oPath
      lTmp'   = toPath loc cfg listTmp
  listTmp %> \_ -> aBlastdblist lTmp'
  oPath'  %> \_ -> aFilterList oPath lTmp' fPath'
  return (ExprPath oPath')
rBlastdblist _ _ = fail "bad argument to rBlastdblist"

aBlastdblist :: Path -> Action ()
aBlastdblist listTmp = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.blastdb.aBlastdblist"
      listTmp' = fromPath loc cfg listTmp
      tmpDir   = takeDirectory $ listTmp'
      oPath    = traceA loc listTmp' [listTmp']
  liftIO $ createDirectoryIfMissing True tmpDir
  withWriteLock' tmpDir $ do
    runCmd $ CmdDesc
      { cmdParallel = False
      , cmdFixEmpties = True
      , cmdOutPath = oPath
      , cmdInPatterns = []
      , cmdNoNeedDirs = []
      , cmdExtraOutPaths = []
      , cmdSanitizePaths = []
      , cmdOptions =[Cwd tmpDir] -- TODO remove?
      , cmdBinary = "blastdblist.sh"
      , cmdArguments = [tmpDir, listTmp']
      , cmdRmPatterns = [] -- TODO remove tmpdir on fail? seems wasteful
      , cmdExitCode = ExitSuccess
      }

-- TODO generalize so it works with busco_list_lineages too?
-- TODO move to a "Filter" module once that gets started
aFilterList :: Path -> Path -> Path -> Action ()
aFilterList oPath listTmp fPath = do
  cfg <- fmap fromJust getShakeExtra
  let fPath'   = fromPath loc cfg fPath
      oPath'   = fromPath loc cfg oPath
      listTmp' = fromPath loc cfg listTmp
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
  let loc = "modules.blastdb.rBlastdbget"
      tmpDir    = blastdbgetCache cfg
      dbPrefix  = exprPath cfg dRef scr e -- final prefix
      dbPrefix' = fromPath loc cfg dbPrefix
      nPath'    = toPath loc cfg nPath
  dbPrefix' %> \_ -> aBlastdbget dbPrefix tmpDir nPath'
  return (ExprPath dbPrefix')
rBlastdbget _ _ = fail "bad argument to rBlastdbget"

aBlastdbget :: Path -> Path -> Path -> Action ()
aBlastdbget dbPrefix tmpDir nPath = do
  cfg <- fmap fromJust getShakeExtra
  let tmp'       = fromPath loc cfg tmpDir
      nPath'     = fromPath loc cfg nPath
      dbPrefix'  = fromPath loc cfg dbPrefix
      loc = "ortholang.modules.blastdb.aBlastdbget"
      dbPrefix'' = traceA loc dbPrefix' [dbPrefix', tmp', nPath']
  -- need' loc [nPath']
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
    , cmdNoNeedDirs = []
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
-- makeblastdbNuclAll :: Function
-- makeblastdbNuclAll = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly (ListOf fna)] -- TODO can this also take faas?
--   , fOutput =  Exactly ndb
--   , fTags = [] -- TODO is it deterministic though? double-check
--   , fNewRules = NewNotImplemented, fOldRules = rMakeblastdbAll
--   }
--   where
--     name = "makeblastdb_fna_all"

-- makeblastdbProtAll :: Function
-- makeblastdbProtAll = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly (ListOf faa)]
--   , fOutput = Exactly pdb
--   , fTags = [] -- TODO is it deterministic though? double-check
--   , fNewRules = NewNotImplemented, fOldRules = rMakeblastdbAll
--   }
--   where
--     name = "makeblastdb_faa_all"

makeblastdbNuclAll :: Function
makeblastdbNuclAll = newFnA1
  "makeblastdb_nucl_all"
  -- (Exactly (ListOf fna))
  (ListSigs $ Some fa "a fasta file")
  (Exactly ndb)
  (aMakeblastdbAll2 "nucl")
  []

makeblastdbProtAll :: Function
makeblastdbProtAll = newFnA1
  "makeblastdb_prot_all"
  (Exactly (ListOf faa))
  (Exactly pdb)
  (aMakeblastdbAll2 "prot")
  []

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
-- rMakeblastdbAll :: RulesFn
-- rMakeblastdbAll scr e@(Fun rtn _ _ _ [fas]) = do
--   (ExprPath fasPath) <- rExpr scr fas
--   cfg  <- fmap fromJust getShakeExtraRules
--   dRef <- fmap fromJust getShakeExtraRules
--   let loc = "modules.blastdb.rMakeblastdbAll"
--       out       = exprPath cfg dRef scr e
--       out'      = debugR' cfg loc e $ fromPath loc cfg out
--       cDir      = makeblastdbCache cfg
--       fasPath'  = toPath loc cfg fasPath
-- 
--   -- TODO need new shake first:
--   -- out' %> \_ -> actionRetry 3 $ aMakeblastdbAll rtn cfg ref cDir [out, fasPath']
-- 
--   out' %> \_ -> aMakeblastdbAll rtn cDir [out, fasPath']
--   -- TODO what's up with the linking? just write the prefix to the outfile!
--   return (ExprPath out')
-- rMakeblastdbAll _ e = error $ "bad argument to rMakeblastdbAll: " ++ show e

-- listPrefixFiles :: FilePattern -> IO [FilePath]
-- listPrefixFiles prefix = getDirectoryFilesIO pDir [pName] >>= return . map (pDir </>)
--   where
--     pDir  = takeDirectory prefix
--     pName = takeFileName  prefix

aMakeblastdbAll2 :: String -> NewAction1
aMakeblastdbAll2 dbType (ExprPath out') fasPath' = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.blastdb.aMakeblastdbAll2"
      out     = toPath loc cfg out'
      fasPath = toPath loc cfg fasPath'
      cDir    = makeblastdbCache cfg
  aMakeblastdbAll dbType cDir [out, fasPath]

-- TODO why does this randomly fail by producing only two files?
-- TODO why is cDir just the top-level cache without its last dir component?
aMakeblastdbAll :: String -> Path -> [Path] -> Action ()
aMakeblastdbAll dbType cDir [out, fasPath] = do
  -- TODO exprPath handles this now?
  -- let relDb = makeRel_ ative (tmpdir cfg) dbOut
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.blastdb.aMakeblastdbAll"
      out'     = fromPath loc cfg out
      cDir'    = fromPath loc cfg cDir
      fasPath' = fromPath loc cfg fasPath
  -- let dbType' = if dbType == ndb then "nucl" else "prot"
  need' loc [fasPath']

  -- The idea was to hash content here, but it took a long time.
  -- So now it gets hashed only once, in another thread, by a load_* function,
  -- and from then on we pick the hash out of the filename.
  fasHash <- fmap takeBaseName $ liftIO $ resolveSymlinks (Just [tmpdir cfg]) fasPath'

  let dbDir  = cDir' </> fasHash
      dbOut  = dbDir </> "result"
      dbOut' = toPath loc cfg dbOut
      out''  = traceA loc out' [dbType, out', dbOut, fasPath']
      -- dbPtn  = dbDir </> "*" -- TODO does this actually help?
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
  let faPaths' = map (fromPath loc cfg) faPaths
  need' loc faPaths'
  let noQuoting  = unwords $ map (fromPath loc cfg) faPaths
      quoteOuter = "\"" ++ noQuoting ++ "\""
      fixedPaths = if isJust (wrapper cfg) then quoteOuter else noQuoting
      -- quoteInner = "\"" ++ unwords
      --              (map (\p -> "\"" ++ fromPath loc cfg p ++ "\"") faPaths)
      --              ++ "\""

  dbg $ "out': "       ++ out'
  dbg $ "cDir: "       ++ show cDir
  dbg $ "cDir': "      ++ cDir'
  dbg $ "dbOut': "     ++ show dbOut'
  dbg $ "dbType: "    ++ dbType
  dbg $ "cfg: "        ++ show cfg
  dbg $ "fixedPaths: " ++ show fixedPaths
  -- dbg $ "dbPtn: "      ++ dbPtn
  dbg $ "dbOut: "      ++ dbOut

  liftIO $ createDirectoryIfMissing True dbDir
  -- before <- liftIO $ listPrefixFiles dbPtn
  -- when (length before < 5) $ do
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = dbOut
    , cmdInPatterns = []
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdBinary = "makeblastdb.sh"
    , cmdArguments = [dbOut, fixedPaths, dbType]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [dbDir]
    }

  -- check that all the right files were created
  -- after <- liftIO $ listPrefixFiles dbPtn
  -- liftIO $ putStrLn "running makeblastdb"
  -- dbg $ "after: " ++ show after
  -- trackWrite' after

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
  produces [dbOut]
  -- trackWrite' [dbOut]
  writePath loc out'' dbOut'
aMakeblastdbAll _ _ paths = error $ "bad argument to aMakeblastdbAll: " ++ show paths

----------------------------------------
-- make a db from a single FASTA file --
----------------------------------------

-- these are oddly implemented in terms of the _all ones above,
-- because that turned out to be easier

-- makeblastdbNucl :: Function
-- makeblastdbNucl = Function
--   { fOpChar = Nothing, fName = "makeblastdb_fna"
--   , fInputs = [Exactly fna] -- TODO can't do it from faa right?
--   , fOutput =  Exactly ndb
--   , fTags = [] -- TODO is it deterministic though? double-check
--   , fNewRules = NewNotImplemented, fOldRules = rMakeblastdb
--   }

makeblastdbNucl :: Function
makeblastdbNucl = newExprExpansion
  "makeblastdb_nucl"
  [Some fa "a fasta file"]
  (Exactly ndb)
  mMakeblastdb
  [] -- TODO is it though?

-- makeblastdbProt :: Function
-- makeblastdbProt = Function
--   { fOpChar = Nothing, fName = "makeblastdb_faa"
--   , fInputs = [Exactly faa] -- TODO can't do it from faa right?
--   , fOutput =  Exactly pdb
--   , fTags = [] -- TODO is it deterministic though? double-check
--   , fNewRules = NewNotImplemented, fOldRules = rMakeblastdb
--   }

makeblastdbProt :: Function
makeblastdbProt = newExprExpansion
  "makeblastdb_prot"
  [Exactly faa]
  (Exactly pdb)
  mMakeblastdb
  [] -- TODO is it though?

-- (Some fa "a fasta file") (Encoded blastdb (Some fa "a fasta file"))
-- shown as "fa -> fa.blastdb, where fa is a fasta file"
-- tMakeblastdb :: Type -> TypeChecker
-- tMakeblastdb dbType [faType]
--   | dbType == pdb && faType   ==    faa       = Right pdb
--   | dbType == ndb && faType `elem` [faa, fna] = Right dbType
-- tMakeblastdb _ _ = error "makeblastdb requires a fasta file" -- TODO typed error

-- rMakeblastdb :: RulesFn
-- rMakeblastdb s e = rMakeblastdbAll s $ withSingletonArg e

mMakeblastdb :: ExprExpansion
mMakeblastdb _ _ (Fun r _ ds n [s]) = Fun r Nothing ds (n ++ "_all") [withSingleton s]
mMakeblastdb _ _ e = error "ortholang.modules.blastdb.mMakeblastdb" $ "bad arg: " ++ show e

-----------------------------------------------
-- make list of dbs from list of FASTA files --
-----------------------------------------------

-- TODO convert to just one function that makes either kind of db? or let ppl choose the type
-- mkMakeblastdbEach :: Type -> Function
-- mkMakeblastdbEach faType = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly (ListOf faType)]
--   , fOutput =  Exactly (ListOf (EncodedAs blastdb faType))
--   , fTags = [] -- TODO is it deterministic though? double-check
--   , fNewRules = NewNotImplemented, fOldRules = rMakeblastdbEach
--   }
--   where
--     name = "makeblastdb_" ++ ext faType ++ "_each" -- TODO change scripts to match!
--     -- d = name ++ " : " ++ ext ++ ".list -> " ++ ext dbType ++ ".list"
--     -- ext  = if dbType == ndb then "fa" else "faa"

-- TODO this fails either either with map or vectorize, so problem might be unrelated?
-- rMakeblastdbEach :: RulesFn
-- rMakeblastdbEach scr (Fun (ListOf dbType) seed deps name [e]) = do
--   cfg <- fmap fromJust getShakeExtraRules
--   let tmpDir = makeblastdbCache cfg 
--       act1 = aMakeblastdbAll dbType tmpDir -- TODO should be i right? not ids?
--       expr' = Fun (ListOf dbType) seed deps name [withSingletons e]
--   (rMap 1 act1) scr expr'
-- rMakeblastdbEach _ e = error $ "bad argument to rMakeblastdbEach" ++ show e

-- mkMakeblastdbEach :: Type -> Function
-- mkMakeblastdbEach faType = newExprExpansion
--   ("makeblastdb_" ++ ext faType ++ "_each")
--   [Exactly (ListOf faType)]
--   (Exactly (ListOf (EncodedAs blastdb faType)))
--   mMakeblastdbEach
--   [] -- TODO is it though?
--
-- mMakeblastdbEach :: ExprExpansion
-- mMakeblastdbEach = undefined -- TODO oh, have to solve mapping first :/

mkMakeblastdbNuclAllEach :: Function
mkMakeblastdbNuclAllEach = hidden $ newFnA1
  "makeblastdb_nucl_all_each"
  (ListSigs $ Some fa "a fasta file")
  (Exactly $ ListOf $ EncodedAs blastdb fna)
  (newMap1of1 "makeblastdb_nucl_all")
  [Hidden]

mkMakeblastdbProtAllEach :: Function
mkMakeblastdbProtAllEach = hidden $ newFnA1
  "makeblastdb_prot_all_each"
  (Exactly $ ListOf $ ListOf faa)
  (Exactly $ ListOf $ EncodedAs blastdb faa)
  (newMap1of1 "makeblastdb_prot_all")
  [Hidden]

mkMakeblastdbNuclEach :: Function
mkMakeblastdbNuclEach = compose1
  "makeblastdb_nucl_each"
  []
  singletons
  mkMakeblastdbNuclAllEach

mkMakeblastdbProtEach :: Function
mkMakeblastdbProtEach = compose1
  "makeblastdb_prot_each"
  []
  singletons
  mkMakeblastdbProtAllEach

------------------
-- show db info --
------------------

-- TODO remove the Volumes... lines too?
showBlastDb :: Config -> LocksRef -> FilePath -> IO String
showBlastDb cfg ref path = do
  let loc = "modules.blastdb.showBlastDb"
  -- TODO wait, wouldn't generic paths be better to leave in?
  path' <- fmap (fromGeneric loc cfg . stripWhiteSpace) $ readFileStrict ref path
  let dbDir = takeDirectory path'
      -- dbBase = takeBaseName  dbDir
  debug loc $ "dbDir: \"" ++ dbDir ++ "\""
  -- note we pass the dir here, since the filename is always result
  out <- withReadLock ref path' $
           readCreateProcess (proc "blastdbcmd.sh" [dbDir]) ""
  let out1 = lines out
      out2 = concatMap (split "\t") out1
      out3 = filter (not . null) out2
      out4 = filter (\l -> not $ "Date" `isPrefixOf` l) out3
      out5 = unlines out4
  return out5
