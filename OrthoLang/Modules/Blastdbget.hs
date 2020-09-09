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
import System.FilePath         ((</>), (<.>), takeDirectory)

debugA' :: String -> String -> Action ()
debugA' name = debugA ("modules.blastdbget." ++ name)

debugR' :: (Pretty a, Show b) => Config -> String -> a -> b -> b
debugR' _ name = debugRules ("modules.blastdbget." ++ name)

olModule :: Module
olModule = Module
  { mName = "Blastdbget"
  , mDesc = "Download BLAST databases from NCBI"
  , mTypes = [fna, faa, ndb, pdb]
  , mGroups = []
  , mEncodings = [blastdb]
  , mFunctions =
    [ blastdblist
    , blastdbgetFna -- TODO mapped version so you can list -> git at once?
    , blastdbgetFaa -- TODO mapped version so you can list -> git at once?
    ]
  }

-- we use two different ones here because it matches the rMap behavior of using just fn name
blastdbgetCache :: Config -> Path
blastdbgetCache cfg = cacheDir cfg "blastdbget"

-----------------
-- blastdblist --
-----------------

{- This is a two-part thing where we only want to download the full list once
 - per day at the most, but instantly filter it by different strings.
 -}

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
