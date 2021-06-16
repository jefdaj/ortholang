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
import OrthoLang.Util (stripWhiteSpace)
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO      (faa, fna, fa)
import OrthoLang.Modules.Singletons (withSingleton, singletons)

-- import Control.Monad           (when, forM)
import Data.Char               (toLower)
-- import Data.List               (isInfixOf)
import Data.List               (isPrefixOf)
import Data.Maybe              (isJust, fromJust)
import Data.String.Utils       (split)
import System.Directory        (createDirectoryIfMissing)
import System.Exit             (ExitCode(..))
import System.FilePath         (takeBaseName, (</>), makeRelative, takeDirectory)
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
  , mRules = []
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
  let loc     = "modules.blastdb.aLoadDB"
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

--------------------------------------------
-- make one db from a list of FASTA files --
--------------------------------------------

-- TODO put the database files themselves in the cache dir and only prefix in exprs?

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

-- we use two different ones here because it matches the rMap behavior of using just fn name
makeblastdbCache :: Config -> Path
makeblastdbCache cfg = cacheDir cfg "makeblastdb"

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

makeblastdbNucl :: Function
makeblastdbNucl = newExprExpansion
  "makeblastdb_nucl"
  [Some fa "a fasta file"]
  (Exactly ndb)
  mMakeblastdb
  [] -- TODO is it though?

makeblastdbProt :: Function
makeblastdbProt = newExprExpansion
  "makeblastdb_prot"
  [Exactly faa]
  (Exactly pdb)
  mMakeblastdb
  [] -- TODO is it though?

mMakeblastdb :: ExprExpansion
mMakeblastdb _ _ (Fun r _ ds n [s]) = Fun r Nothing ds (n ++ "_all") [withSingleton s]
mMakeblastdb _ _ e = error "ortholang.modules.blastdb.mMakeblastdb" $ "bad arg: " ++ show e

-----------------------------------------------
-- make list of dbs from list of FASTA files --
-----------------------------------------------

-- TODO convert to just one function that makes either kind of db? or let ppl choose the type

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

showBlastDb :: Config -> LocksRef -> FilePath -> IO String
showBlastDb cfg ref path = do
  let loc = "modules.blastdb.showBlastDb"
  path' <- fmap (fromGeneric loc cfg . stripWhiteSpace) $ readFileStrict ref path
  let dbDir = takeDirectory path'
      dbBase = takeBaseName path'
  debug loc $ "dbDir: \"" ++ dbDir ++ "\""
  debug loc $ "dbBase: \"" ++ dbBase ++ "\""
  out <- withReadLock ref path' $
           readCreateProcess (proc "blastdbcmd.sh" [dbDir, dbBase]) ""
  let out1 = lines out
      out2 = concatMap (split "\t") out1
      out3 = filter (not . null) out2
      out4 = filter (\l -> not $ "Date" `isPrefixOf` l) out3
      out5 = stripWhiteSpace $ toGeneric cfg $ unlines out4
  return out5
