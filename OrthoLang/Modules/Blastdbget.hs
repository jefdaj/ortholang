{-# LANGUAGE ScopedTypeVariables #-}

-- TODO should you also cache the taxdb at the same time as dblist.txt? seems reasonable
--      or maybe at the same time as doing the first main blastdbget

-- TODO rename blastdblist -> blastdbget_list?

module OrthoLang.Modules.Blastdbget where

import Development.Shake

import OrthoLang.Types
import OrthoLang.Locks
import OrthoLang.Interpreter
import OrthoLang.Modules.SeqIO      (faa, fna)

import OrthoLang.Modules.BlastDB (blastdb, ndb, pdb)

import Data.Char               (toLower)
import Data.List               (isInfixOf)
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
    [ blastdblist, blastdblistDate
    -- TODO rewrite: , blastdbgetFna -- TODO mapped version so you can list -> git at once?
    -- TODO rewrite: , blastdbgetFaa -- TODO mapped version so you can list -> git at once?
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

{- This is a two-part thing where we only want to download the full list once
 - per day at the most, but instantly filter it by different strings. At the
 - same time, we also want to break out a _date version that caches the
 - downloaded lists by date. So the functions we need are:
 -
 - blastdblist : str str -> str.list (the filtering one written with newDate1of2)
 - blasdtdblist_date : str str -> str.list (the caching one written with newFnA2)
 - aBlastdblist (the unnamed Rules for doing the actual list download)
 -}

-- takes a filter string (leave empty for all results)
-- blastdblist :: Function
-- blastdblist = let name = "blastdblist" in Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly str]
--   , fOutput =  Exactly (ListOf str)
--   , fTags = [ReadsURL]
--   , fNewRules = NewNotImplemented, fOldRules = rBlastdblist
--   }

-- | User-facing version that takes a date expression and filter string
--   TODO rename this and make blastdblist be a user-facing expansion like filter_list (this) "filter str"?
blastdblist :: Function
blastdblist = newFnA1
  "blastdblist"
  (Exactly str)
  (Exactly $ ListOf str)
  (newDate1of1 "blastdblist")
  [ReadsURL]

----------------------
-- blastdblist_date --
----------------------

-- | Hidden version that assumes a properly formatted date from newDate1of1 above
blastdblistDate :: Function
blastdblistDate = newFnA1
  "blastdblist_date"
  (Exactly str)
  (Exactly $ ListOf str)
  aBlastdblistDate -- TODO does this need to be expanded again?
  [Hidden, ReadsURL]

aBlastdblistDate :: NewAction1
aBlastdblistDate (ExprPath out') datePath' = do
  let loc = "ortholang.modules.blastdbget.aBlastdblistDate"
  cfg <- fmap fromJust getShakeExtra
  day <- readLit loc datePath'
  -- TODO utility fn for finding this?
  let dbl' = fromPath loc cfg (blastdbgetCache cfg) </> day </> "dblist.txt"
  need' loc [dbl'] -- TODO remove?
  let dbl = toPath loc cfg dbl'
      out = toPath loc cfg out'
  symlink out dbl

------------------
-- blastdbget_* --
------------------

-- mkBlastdbget :: String -> Type -> Function
-- mkBlastdbget name faType = Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = [Exactly str]
--   , fOutput =  Exactly (EncodedAs blastdb faType)
--   , fTags = []
--   , fNewRules = NewNotImplemented, fOldRules = rBlastdbget
--   }

mkBlastdbget :: String -> Type -> Function
mkBlastdbget name faType = newFnA1 -- TODO add date arg
  name
  (Exactly str)
  (Exactly $ EncodedAs blastdb faType)
  undefined -- TODO write this: aBlastdbget
  [ReadsURL]

-- TODO rename with fna
blastdbgetFna :: Function
blastdbgetFna = mkBlastdbget "blastdbget_fna" fna

-- TODO rename with faa
blastdbgetFaa :: Function
blastdbgetFaa = mkBlastdbget "blastdbget_faa" faa

-- rBlastdbget :: RulesFn
-- rBlastdbget scr e@(Fun _ _ _ _ [name]) = do
--   (ExprPath nPath) <- rExpr scr name
--   cfg  <- fmap fromJust getShakeExtraRules
--   dRef <- fmap fromJust getShakeExtraRules
--   let loc = "modules.blastdb.rBlastdbget"
--       tmpDir    = blastdbgetCache cfg
--       dbPrefix  = exprPath cfg dRef scr e -- final prefix
--       dbPrefix' = fromPath loc cfg dbPrefix
--       nPath'    = toPath loc cfg nPath
--   dbPrefix' %> \_ -> aBlastdbget dbPrefix tmpDir nPath'
--   return (ExprPath dbPrefix')
-- rBlastdbget _ _ = fail "bad argument to rBlastdbget"

aBlastdbget :: NewAction2
aBlastdbget (ExprPath dbPrefix') tmp' nPath' = do
  -- cfg <- fmap fromJust getShakeExtra
  -- let tmp'       = fromPath loc cfg tmpDir
      -- nPath'     = fromPath loc cfg nPath
      -- dbPrefix'  = fromPath loc cfg dbPrefix
  let loc = "ortholang.modules.blastdb.aBlastdbget"
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
