module OrthoLang.Modules.OrthoFinder
  where

-- TODO write a function to extract_seqs from multiple fastas at once, useful here + elsewhere?
-- TODO can all "extract" functions be renamed with "list"?
-- TODO try DIAMOND, MMseqs2

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Locks

import Control.Monad.IO.Class  (liftIO)
import OrthoLang.Modules.SeqIO (faa)
import System.Directory        (createDirectoryIfMissing)
import System.Exit             (ExitCode(..))
import System.FilePath         ((</>), takeFileName, replaceBaseName)
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "OrthoFinder"
  , mDesc = "Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree"
  , mTypes = [faa, ofr]
  , mGroups = []
  , mEncodings = []
  , mFunctions =
      [ orthofinder
      ]
  }

ofr :: Type
ofr = Type
  { tExt  = "ofr"
  , tDesc = "OrthoFinder results"
  , tShow = \_ ref path -> do
      txt <- readFileStrict ref path
      return $ unlines $ take 17 $ lines txt -- TODO why doesn't this limit lines?
  }

-----------------
-- orthofinder --
-----------------

orthofinder :: Function
orthofinder = let name = "orthofinder" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeDesc  = mkTypeDesc  name [ListOf faa] ofr
  -- , fTypeCheck = defaultTypeCheck name [ListOf faa] ofr
  , fInputs = [Exactly (ListOf faa)]
  , fOutput = Exactly ofr
  , fTags = [Stochastic]
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple aOrthofinder
  }

-- TODO do blast separately and link to outputs from the WorkingDirectory dir, and check if same results
-- TODO what's diamond blast? do i need to add it?
aOrthofinder :: [Path] -> Action ()
aOrthofinder [out, faListPath] = do
  cfg <- fmap fromJust getShakeExtra
  let out'        = fromPath loc cfg out
      faListPath' = fromPath loc cfg faListPath
      loc = "modules.orthofinder.aOrthofinder"
      out''       = traceA loc out' [out', faListPath']
      tmpDir = tmpdir cfg </> "cache" </> "orthofinder" </> digest faListPath
      statsPath = toPath loc cfg $ tmpDir
                    </> "OrthoFinder" </> "Results_"
                    </> "Comparative_Genomics_Statistics" </> "Statistics_Overall.tsv"
      statsPath' = fromPath loc cfg statsPath
  liftIO $ createDirectoryIfMissing True tmpDir
  -- withWriteLock' (tmpDir </> "lock") $ do
  faPaths <- readPaths loc faListPath'
  let faPaths' = map (fromPath loc cfg) faPaths
  need' "ortholang.modules.orthofinder.aOrthofinder" faPaths'
  let faLinks = map (\p -> toPath loc cfg $ tmpDir </> (takeFileName $ fromPath loc cfg p)) faPaths
  -- orthofinder is sensitive to which files and dirs have been created before it runs
  -- so we need to lock the tmpDir to prevent it creating something like Results__1
  -- and we can't mark statsPath' as an extra outpath
  -- TODO patch orthofinder not to adjust and then do this the standard way
  withWriteLock' tmpDir $ do -- this is important to prevent multiple threads trying at once
    mapM_ (\(p, l) -> symlink l p) $ zip faPaths faLinks
    runCmd $ CmdDesc
      { cmdBinary = "orthofinder.sh"
      , cmdArguments = [replaceBaseName out'' "out", tmpDir, "diamond", "-n", digest faListPath]
      , cmdFixEmpties = False
      , cmdParallel = False -- TODO fix this? it fails because of withResource somehow
      , cmdOptions = []
      , cmdInPatterns = faPaths'
      , cmdOutPath = replaceBaseName out'' "out"
      , cmdExtraOutPaths = [replaceBaseName out'' "err"] -- TODO statsPath'? seems to break it
      , cmdSanitizePaths = [] -- TODO use this?
      , cmdExitCode = ExitSuccess
      , cmdRmPatterns = [out'', tmpDir]
      }
    -- liftIO $ putStrLn $ "out: " ++ show out
    -- liftIO $ putStrLn $ "statsPath: " ++ show statsPath
    -- liftIO $ putStrLn $ "statsPath: " ++ show statsPath
  trackWrite' [statsPath']
  symlink out statsPath

aOrthofinder args = error $ "bad argument to aOrthofinder: " ++ show args
