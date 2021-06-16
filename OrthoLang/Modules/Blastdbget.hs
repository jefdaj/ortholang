{-# LANGUAGE ScopedTypeVariables #-}

module OrthoLang.Modules.Blastdbget where

import Development.Shake

import OrthoLang.Types
import OrthoLang.Locks
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO      (faa, fna)

import OrthoLang.Modules.BlastDB (blastdb, ndb, pdb)

import Data.Maybe              (fromJust)
import System.Directory        (createDirectoryIfMissing)
import System.Exit             (ExitCode(..))
import System.FilePath         ((</>))
import Data.Time               (Day)

---------------
-- debugging --
---------------

debugA' :: String -> String -> Action ()
debugA' name = debugA ("modules.blastdbget." ++ name)

debugR' :: (Pretty a, Show b) => Config -> String -> a -> b -> b
debugR' _ name = debugRules ("modules.blastdbget." ++ name)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "Blastdbget"
  , mDesc = "Download BLAST databases from NCBI"
  , mTypes = [fna, faa, ndb, pdb]
  , mGroups = []
  , mEncodings = [blastdb]
  , mRules = [rBlastdblist]
  , mFunctions =
    [ blastdbList, blastdbListDate
    , blastdbGetFna -- TODO mapped version so you can list -> git at once?
    , blastdbGetFaa -- TODO mapped version so you can list -> git at once?
    , blastdbGetFnaDate -- TODO mapped version so you can list -> git at once?
    , blastdbGetFaaDate -- TODO mapped version so you can list -> git at once?
    ]
  }

-- we use two different ones here because it matches the rMap behavior of using just fn name
blastdbgetCache :: Config -> Path
blastdbgetCache cfg = cacheDir cfg "blastdbget"

-- | This downloads dblist.txt for use in the blastdblist* fns
rBlastdblist :: Rules ()
rBlastdblist = do
  (cfg :: Config) <- fmap fromJust getShakeExtraRules
  (day :: Day   ) <- fmap fromJust getShakeExtraRules
  let loc  = "ortholang.modules.blastdbget.rBlastdblist"
      tmp' = fromPath loc cfg $ blastdbgetCache cfg
      dbl' = tmp' </> show day </> "dblist.txt"
  dbl' %> \_ -> do
    liftIO $ createDirectoryIfMissing True tmp'
    withWriteLock' tmp' $ do
      runCmd $ CmdDesc
        { cmdParallel = False
        , cmdFixEmpties = True
        , cmdOutPath = dbl'
        , cmdInPatterns = []
        , cmdNoNeedDirs = []
        , cmdExtraOutPaths = []
        , cmdSanitizePaths = []
        , cmdOptions =[Cwd tmp'] -- TODO remove?
        , cmdBinary = "blastdblist.sh"
        , cmdArguments = [tmp', dbl']
        , cmdRmPatterns = [] -- TODO remove dblist.txt on fail?
        , cmdExitCode = ExitSuccess
        }

-----------------
-- blastdblist --
-----------------

-- | User-facing version that takes a date expression (date in yyyy-mm-dd format or "today" or "cached")
blastdbList :: Function
blastdbList = newFnA1
  "blastdb_list"
  (Exactly str)
  (Exactly $ ListOf str)
  (newDate1of1 "blastdb_list")
  [ReadsURL]

----------------------
-- blastdblist_date --
----------------------

-- | Hidden version that assumes a properly formatted date from newDate1of1 above
blastdbListDate :: Function
blastdbListDate = newFnA1
  "blastdb_list_date"
  (Exactly str)
  (Exactly $ ListOf str)
  aBlastdbListDate
  [Hidden, ReadsURL]

aBlastdbListDate :: NewAction1
aBlastdbListDate (ExprPath out') datePath' = do
  let loc = "ortholang.modules.blastdbget.aBlastdbListDate"
  cfg <- fmap fromJust getShakeExtra
  day <- readLit loc datePath'
  -- TODO utility fn for finding this?
  let dbl' = fromPath loc cfg (blastdbgetCache cfg) </> day </> "dblist.txt"
  -- need' loc [dbl'] -- TODO remove?
  let dbl = toPath loc cfg dbl'
      out = toPath loc cfg out'
  symlink out dbl

------------------
-- blastdbget_* --
------------------

-- | User-facing versions that take a date expression
mkBlastdbGet :: Type -> Function
mkBlastdbGet faType =
  let name' = "blastdb_get_" ++ ext faType
  in newFnA2
       name'
       (Exactly str, Exactly str)
       (Exactly $ EncodedAs blastdb faType)
       (newDate1of2 name')
       [ReadsURL]

blastdbGetFna :: Function
blastdbGetFna = mkBlastdbGet fna

blastdbGetFaa :: Function
blastdbGetFaa = mkBlastdbGet faa

-----------------------
-- blastdbget_*_date --
-----------------------

-- | Hidden versions that take a proper date string sanitized by newDate1of2 above
mkBlastdbGetDate :: Type -> Function
mkBlastdbGetDate faType =
  let name' = "blastdb_get_" ++ ext faType ++ "_date"
  in newFnA2
       name'
       (Exactly str, Exactly str)
       (Exactly $ EncodedAs blastdb faType)
       aBlastdbGetDate
       [ReadsURL, Hidden]

blastdbGetFnaDate :: Function
blastdbGetFnaDate = mkBlastdbGetDate fna

blastdbGetFaaDate :: Function
blastdbGetFaaDate = mkBlastdbGetDate faa

aBlastdbGetDate :: NewAction2
aBlastdbGetDate (ExprPath outPath') datePath' namePath' = do
  let loc = "ortholang.modules.blastdb.aBlastdbget"
  cfg <- fmap fromJust getShakeExtra
  date <- readLit loc datePath'
  let tmpDir'    = fromPath loc cfg (blastdbgetCache cfg) </> date
      outPath'' = traceA loc outPath' [outPath', tmpDir', namePath']
  dbName <- stripWhiteSpace <$> readLit loc namePath' -- TODO need to strip?
  liftIO $ createDirectoryIfMissing True tmpDir'
  debugA' "aBlastdbget" $ "outPath'': " ++ outPath''
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = outPath''
    , cmdInPatterns = []
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[Cwd tmpDir'] -- TODO remove?
    , cmdBinary = "blastdbget.sh"
    , cmdArguments = [tmpDir', dbName]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [] -- TODO remove anything on failure?
    }
  let dbPath = toPath loc cfg $ tmpDir' </> dbName
  debugA' "aBlastdbget" $ "dbPath: " ++ show dbPath
  writePath loc outPath'' dbPath
