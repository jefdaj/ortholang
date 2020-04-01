{-# LANGUAGE ScopedTypeVariables #-}

module OrthoLang.Modules.BlastDB where

-- TODO aha! errors are partially because i've been assuming blastdbget returns ndb when really it can be pdb?
--      should be fixable by determining the type from the .ni* files or whatever

-- TODO should makeblastdb be just one fn? no, make everything else stricter later!
-- TODO need to remove tmpfiles in /tmp on quit to save space?

import Development.Shake
import OrthoLang.Core
import OrthoLang.Locks

import Control.Monad           (when, forM)
import Data.Char               (toLower)
import Data.List               (isInfixOf)
import Data.List               (isPrefixOf)
import Data.Maybe              (isJust)
import Data.String.Utils       (split)
import OrthoLang.Modules.SeqIO (faa, fna)
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
debugR' cfg name = debugRules cfg ("modules.blastdb." ++ name)

orthoLangModule :: Module
orthoLangModule = Module
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
    -- TODO replace most of the singleton lists in test cuts with these
    , makeblastdbNucl    -- makeblastdb_nucl     : fa       -> ndb
    , makeblastdbProt    -- makeblastdb_prot     : faa      -> pdb

    -- these are used in the _rbh machinery
    -- they're a little weird because they are implemented using the non _all
    -- versions, so they internally make their args into lists of singleton lists
    , mkMakeblastdbEach ndb -- makeblastdb_nucl_each : fa.list  -> ndb.list
    , mkMakeblastdbEach pdb -- makeblastdb_prot_each : faa.list -> pdb.list

    , blastdbgetNucl -- TODO mapped version so you can list -> git at once?
    , blastdbgetProt -- TODO mapped version so you can list -> git at once?
    , blastdblist
    -- , TODO write loadBlastDB

    -- TODO hide this from users?
    , singletons
    ]
  }

-- TODO add a blastdb type group? seems natural but i'm not sure you ever need to mix them

ndb :: Type
ndb = Type
  { tExt  = "ndb"
  , tDesc = "BLAST nucleotide database"
  , tShow  = showBlastDb
  }

-- TODO will people confuse this with PDB files for viewing molecules?
pdb :: Type
pdb = Type
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

mkLoadDB :: String -> Type -> Function
mkLoadDB name rtn = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str] rtn
  , fTypeDesc  = mkTypeDesc name [str] rtn
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rLoadDB
  }

mkLoadDBEach :: String -> Type -> Function
mkLoadDBEach name rtn = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [ListOf str] (ListOf rtn)
  , fTypeDesc  = mkTypeDesc name  [ListOf str] (ListOf rtn)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = undefined -- TODO write this!
  }

rLoadDB :: RulesFn
rLoadDB st@(scr,cfg, ref, ids, dRef) e@(Fun _ _ _ _ [s]) = do
  (ExprPath sPath) <- rExpr st s
  let sPath' = toPath cfg sPath
  oPath' %> \_ -> aLoadDB cfg ref ids oPath sPath'
  return (ExprPath oPath')
  where
    oPath  = exprPath cfg dRef scr e
    oPath' = fromPath cfg oPath
rLoadDB _ _ = fail "bad argument to rLoadDB"

aLoadDB :: Path -> Path -> Action ()
aLoadDB cfg ref _ oPath sPath = do
  pattern <- readLit cfg ref sPath'
  let pattern' = makeRelative (cfgTmpDir cfg) pattern -- TODO is this right??
  writeLit cfg ref oPath'' pattern'
  where
    oPath'  = fromPath cfg oPath
    sPath'  = fromPath cfg sPath
    oPath'' = traceA "aLoadDB" oPath' [oPath', sPath']

loadNuclDB :: Function
loadNuclDB = mkLoadDB "load_nucl_db" ndb

loadProtDB :: Function
loadProtDB = mkLoadDB "load_prot_db" pdb

loadNuclDBEach :: Function
loadNuclDBEach = mkLoadDBEach "load_nucl_db_each" ndb

loadProtDBEach :: Function
loadProtDBEach = mkLoadDBEach "load_prot_db_each" pdb

------------------------
-- download from NCBI --
------------------------

-- takes a filter string (leave empty for all results)
blastdblist :: Function
blastdblist = let name = "blastdblist" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [str] (ListOf str)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rBlastdblist
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
rBlastdblist s@(scr,cfg, ref, ids, dRef) e@(Fun _ _ _ _ [f]) = do
  (ExprPath fPath) <- rExpr s f
  let fPath' = toPath   cfg fPath
  listTmp %> \_ -> aBlastdblist   cfg ref ids lTmp'
  oPath'  %> \_ -> aFilterList cfg ref ids oPath lTmp' fPath'
  return (ExprPath oPath')
  where
    oPath   = exprPath cfg dRef scr e
    tmpDir  = blastdbgetCache cfg
    tmpDir' = fromPath cfg tmpDir
    listTmp = tmpDir' </> "dblist" <.> "txt"
    oPath'  = fromPath cfg oPath
    lTmp'   = toPath   cfg listTmp
rBlastdblist _ _ = fail "bad argument to rBlastdblist"

aBlastdblist :: Path -> Action ()
aBlastdblist cfg ref _ listTmp = do
  liftIO $ createDirectoryIfMissing True tmpDir
  withWriteLock' tmpDir $ do
    runCmd cfg ref $ CmdDesc
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
  where
    listTmp' = fromPath cfg listTmp
    tmpDir   = takeDirectory $ listTmp'
    oPath    = traceA "aBlastdblist" listTmp' [listTmp']

-- TODO generalize so it works with busco_list_lineages too?
-- TODO move to a "Filter" module once that gets started
aFilterList :: Path -> Path -> Path -> Action ()
aFilterList cfg ref _ oPath listTmp fPath = do
  filterStr <- readLit  cfg ref fPath'
  out       <- readLits listTmp'
  let names  = if null out then [] else tail out
      names' = if null filterStr then names else filterNames filterStr names
  debugA' "aFilterList" $ "names': " ++ show names'
  writeLits cfg ref oPath'' names'
  where
    fPath'   = fromPath cfg fPath
    oPath'   = fromPath cfg oPath
    listTmp' = fromPath cfg listTmp
    oPath''  = traceA "aFilterList" oPath' [oPath', listTmp', fPath']

mkBlastdbget :: String -> Type -> Function
mkBlastdbget name dbType = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str] dbType -- TODO are there protein ones too?
  , fTypeDesc  = mkTypeDesc name  [str] dbType -- TODO are there protein ones too?
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rBlastdbget
  }

blastdbgetNucl :: Function
blastdbgetNucl = mkBlastdbget "blastdbget_nucl" ndb

blastdbgetProt :: Function
blastdbgetProt = mkBlastdbget "blastdbget_prot" pdb

rBlastdbget :: RulesFn
rBlastdbget st@(scr,cfg, ref, ids, dRef) e@(Fun _ _ _ _ [name]) = do
  (ExprPath nPath) <- rExpr st name
  let tmpDir    = blastdbgetCache cfg
      dbPrefix  = exprPath cfg dRef scr e -- final prefix
      dbPrefix' = fromPath cfg dbPrefix
      nPath'    = toPath cfg nPath
  dbPrefix' %> \_ -> aBlastdbget cfg ref ids dbPrefix tmpDir nPath'
  return (ExprPath dbPrefix')
rBlastdbget _ _ = fail "bad argument to rBlastdbget"

aBlastdbget :: Path -> Path -> Path -> Action ()
aBlastdbget cfg ref _ dbPrefix tmpDir nPath = do
  need' "ortholang.modules.blastdb.aBlastdbget" [nPath']
  dbName <- fmap stripWhiteSpace $ readLit cfg ref nPath' -- TODO need to strip?
  let dbPath = tmp' </> dbName
  liftIO $ createDirectoryIfMissing True tmp'
  -- TODO was taxdb needed for anything else?
  debugA' "aBlastdbget" $ "dbPrefix'': " ++ dbPrefix''
  debugA' "aBlastdbget" $ "dbPath: " ++ dbPath
  runCmd cfg ref $ CmdDesc
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
  writeLit cfg ref dbPrefix'' dbPath -- note this writes the path itself!
  where
    tmp'       = fromPath cfg tmpDir
    nPath'     = fromPath cfg nPath
    dbPrefix'  = fromPath cfg dbPrefix
    dbPrefix'' = traceA "aBlastdbget" dbPrefix' [dbPrefix', tmp', nPath']

--------------------------------------------
-- make one db from a list of FASTA files --
--------------------------------------------

-- TODO put the database files themselves in the cache dir and only prefix in exprs?

-- TODO silence output?
-- TODO does this have an error where db path depends on the outer expression
--      in addition to actual inputs?
makeblastdbNuclAll :: Function
makeblastdbNuclAll = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = tMakeblastdbAll name ndb
  , fTypeDesc  = name ++ " : fa.list -> ndb"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMakeblastdbAll
  }
  where
    name = "makeblastdb_nucl_all"

makeblastdbProtAll :: Function
makeblastdbProtAll = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = tMakeblastdbAll name pdb
  , fTypeDesc  = name ++ " : faa.list -> pdb"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMakeblastdbAll
  }
  where
    name = "makeblastdb_prot_all"

-- TODO allow fna.list -> pdb.list using translate?
tMakeblastdbAll :: String -> Type -> TypeChecker
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
rMakeblastdbAll s@(scr,cfg, ref, ids, dRef) e@(Fun rtn _ _ _ [fas]) = do
  (ExprPath fasPath) <- rExpr s fas
  let out       = exprPath cfg dRef scr e
      out'      = debugR' cfg "rMakeblastdbAll" e $ fromPath cfg out
      cDir      = makeblastdbCache cfg
      fasPath'   = toPath cfg fasPath

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
aMakeblastdbAll :: Type -> Path -> [Path] -> Action ()
aMakeblastdbAll dbType cfg ref _ cDir [out, fasPath] = do
  -- TODO exprPath handles this now?
  -- let relDb = makeRelative (cfgTmpDir cfg) dbOut
  let dbType' = if dbType == ndb then "nucl" else "prot"
  need' "ortholang.modules.blastdb.aMakeblastdbAll" [fasPath']

  -- The idea was to hash content here, but it took a long time.
  -- So now it gets hashed only once, in another thread, by a load_* function,
  -- and from then on we pick the hash out of the filename.
  fasHash <- fmap takeBaseName $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) fasPath'

  let dbDir  = cDir' </> fasHash
      dbOut  = dbDir </> fasHash <.> extOf dbType
      dbOut' = toPath cfg dbOut
      out''  = traceA "aMakeblastdbAll" out' [extOf dbType, out', dbOut, fasPath']
      dbPtn  = cDir' </> fasHash </> "*" -- TODO does this actually help?

  -- Quoting is tricky here because makeblastdb expects multiple -in fastas to
  -- be passed as one quoted arg, but we also have to take into account Shake's
  -- built-in quoting and a possible wrapper script, which may in turn be
  -- running SLURM commands. These settings work on my system in all cases, but
  -- quoteInner may also be needed if you have spaces in your paths.... best
  -- solution is just to avoid that for now?
  --
  -- TODO would quoting JUST inner paths be right? And Shake does the outer ones?
  faPaths <- readPaths fasPath'
  let noQuoting  = unwords $ map (fromPath cfg) faPaths
      quoteOuter = "\"" ++ noQuoting ++ "\""
      fixedPaths = if isJust (cfgWrapper cfg) then quoteOuter else noQuoting
      -- quoteInner = "\"" ++ unwords
      --              (map (\p -> "\"" ++ fromPath cfg p ++ "\"") faPaths)
      --              ++ "\""

  let dbg = debugA' "aMakeblastdbAll"
  dbg $ "out': "       ++ out'
  dbg $ "cDir: "       ++ show cDir
  dbg $ "cDir': "      ++ cDir'
  dbg $ "dbOut': "     ++ show dbOut'
  dbg $ "dbType': "    ++ dbType'
  dbg $ "cfg: "        ++ show cfg
  dbg $ "fixedPaths: " ++ show fixedPaths

  liftIO $ createDirectoryIfMissing True dbDir
  before <- listPrefixFiles dbPtn
  when (length before < 5) $ do
    dbg $ "this is dbPtn: " ++ dbPtn
    dbg $ "this will be dbOut: " ++ dbOut
    runCmd cfg ref $ CmdDesc
      { cmdParallel = False
      , cmdFixEmpties = True
      , cmdOutPath = out'
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
    after <- listPrefixFiles dbPtn
    -- liftIO $ putStrLn "running makeblastdb"
    trackWrite' after

    -- usually there's an index file too, but not always
    -- TODO put these back? they sometimes fail when it splits into .00.pin etc.
    -- let expected = if dbType == ndb
    --                  then [".nhr", ".nin", ".nsq"]
    --                  else [".phr", ".pin", ".psq"]
    --     success = all (\e -> e `elem` (map takeExtension after)) expected
    -- dbg $ "these actual db files were created: " ++ show after
    -- unless success $ error $ "makeblastdb failed to create some database files: " ++ show after
    
    dbg $ "dbOut was also created: " ++ dbOut
  -- TODO why should this work when outside the when block but not inside?? something about retries?
  writePath cfg ref out'' dbOut'
  where
    out'     = fromPath cfg out
    cDir'    = fromPath cfg cDir
    fasPath' = fromPath cfg fasPath
aMakeblastdbAll _ _ _ _ _ paths = error $ "bad argument to aMakeblastdbAll: " ++ show paths

----------------------------------------
-- make a db from a single FASTA file --
----------------------------------------

-- these are oddly implemented in terms of the _all ones above,
-- because that turned out to be easier

makeblastdbNucl :: Function
makeblastdbNucl = Function
  { fOpChar = Nothing, fName = "makeblastdb_nucl"
  , fTypeCheck = tMakeblastdb ndb
  , fTypeDesc  = "makeblastdb_nucl : fa -> ndb"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMakeblastdb
  }

makeblastdbProt :: Function
makeblastdbProt = Function
  { fOpChar = Nothing, fName = "makeblastdb_prot"
  , fTypeCheck = tMakeblastdb pdb
  , fTypeDesc  = "makeblastdb_prot : faa -> pdb"
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMakeblastdb
  }

tMakeblastdb :: Type -> TypeChecker
tMakeblastdb dbType [faType]
  | dbType == pdb && faType   ==    faa       = Right pdb
  | dbType == ndb && faType `elem` [faa, fna] = Right dbType
tMakeblastdb _ _ = error "makeblastdb requires a fasta file" -- TODO typed error

rMakeblastdb :: RulesFn
rMakeblastdb s e = rMakeblastdbAll s $ withSingleton e

-- TODO is this map1of1?
withSingleton :: Expr -> Expr
withSingleton (Fun rtn salt deps name [s])
  =           (Fun rtn salt deps name [singleton s])
withSingleton e = error $ "bad argument to withSingleton: " ++ show e

-----------------------------------------------
-- make list of dbs from list of FASTA files --
-----------------------------------------------

mkMakeblastdbEach :: Type -> Function
mkMakeblastdbEach dbType = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = tMakeblastdbEach dbType
  , fTypeDesc  = desc
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMakeblastdbEach
  }
  where
    desc = name ++ " : " ++ ext ++ ".list -> " ++ extOf dbType ++ ".list"
    name = "makeblastdb" ++ (if dbType == ndb then "_nucl" else "_prot") ++ "_each"
    ext  = if dbType == ndb then "fa" else "faa"

-- TODO no! depends on an arg
tMakeblastdbEach :: Type -> TypeChecker
tMakeblastdbEach dbType [ListOf x] | x `elem` [fna, faa] = Right (ListOf dbType)
tMakeblastdbEach _ _ = error "expected a list of fasta files" -- TODO typed error

-- rFun1 :: Action1 -> RulesFn
-- rFun1 act1 st@(scr,cfg, ref) expr@(Fun _ _ _ _ [a1]) = do

-- map1of1 :: Type -> Type -> Action1 -> Action1
-- map1of1 inType outType act1 cfg locks out a1 = do

-- rMap :: Int -> ([Path] -> Action ()) -> RulesFn
-- rMap index actFn = rMapMain index Nothing actFn'

-- TODO this fails either either with map or vectorize, so problem might be unrelated?
rMakeblastdbEach :: RulesFn
rMakeblastdbEach st@(_, cfg, _, _, _) (Fun (ListOf dbType) salt deps name [e]) =
  -- rFun1 (map1of1 faType dbType act1) st expr'
  (rMap 1 act1) st expr'
  where
    -- faType = typeOf e
    tmpDir = makeblastdbCache cfg 
    -- act1 c r o a1 = aMakeblastdbAll dbType c r tmpDir [o, a1]
    act1 c r i = aMakeblastdbAll dbType c r i tmpDir -- TODO should be i right? not ids?
    expr' = Fun (ListOf dbType) salt deps name [withSingletons e]
    -- expr'' = trace ("expr':" ++ show expr') expr'
rMakeblastdbEach _ e = error $ "bad argument to rMakeblastdbEach" ++ show e

----------------
-- singletons --
----------------

-- TODO move this to its own module? remove it when possible?

withSingletons :: Expr -> Expr
withSingletons e = Fun (ListOf $ typeOf e) (saltOf e) (depsOf e) "singletons" [e]

-- Only used for the makeblastdb_*_each functions so far
-- TODO hide from users?
singletons :: Function
singletons = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeDesc  = name ++ " : X.list -> X.list.list"
  , fTypeCheck = tSingletons
  , fNewRules = Nothing, fOldRules = rSingletons
  }
  where
    name = "singletons"

tSingletons :: [Type] -> Either String Type
tSingletons [ListOf x] = Right $ ListOf $ ListOf x
tSingletons _ = Left "tSingletons expected a list"

rSingletons :: RulesFn
rSingletons st@(scr,cfg, ref, ids, dRef) expr@(Fun rtn _ _ _ [listExpr]) = do
  (ExprPath listPath') <- rExpr st listExpr
  let outPath  = exprPath cfg dRef scr expr
      outPath' = fromPath cfg outPath
      listPath = toPath cfg listPath'
      (ListOf (ListOf t)) = rtn
  outPath' %> \_ -> aSingletons t cfg ref ids outPath listPath
  return $ ExprPath outPath'
rSingletons _ _ = fail "bad argument to rSingletons"

aSingletons :: Type -> Action1
aSingletons elemType cfg ref _ outPath listPath = do
  let listPath' = fromPath cfg listPath
      outPath'  = fromPath cfg outPath
      dbg = debugA' "aSingletons"
  dbg $ "listpath': " ++ listPath'
  dbg $ "outpath': " ++ outPath'
  elems <- readStrings elemType cfg ref listPath'
  dbg $ "elems: " ++ show elems
  singletonPaths <- forM elems $ \e -> do
    let singletonPath' = cachedLinesPath cfg [e] -- TODO nondeterministic?
        singletonPath  = toPath cfg singletonPath'
    dbg $ "singletonPath': " ++ singletonPath'
    writeStrings elemType cfg ref singletonPath' [e]
    return singletonPath
  writePaths cfg ref outPath' singletonPaths -- TODO nondeterministic?

------------------
-- show db info --
------------------

-- TODO remove the Volumes... lines too?
showBlastDb :: Config -> LocksRef -> FilePath -> IO String
showBlastDb cfg ref path = do
  path' <- fmap (fromGeneric cfg . stripWhiteSpace) $ readFileStrict ref path
  let dbDir  = takeDirectory path'
      dbBase = takeFileName  path'
  debug "modules.blastdb.showBlastDb" $ "showBlastDb dbDir: \"" ++ dbDir ++ "\""
  out <- withReadLock path' $
           readCreateProcess (proc "blastdbcmd.sh" [dbDir, dbBase]) ""
  let out1 = lines out
      out2 = concatMap (split "\t") out1
      out3 = filter (not . null) out2
      out4 = filter (\l -> not $ "Date" `isPrefixOf` l) out3
      out5 = unlines out4
  return out5
