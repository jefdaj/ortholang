module OrthoLang.Test
  ( TestTree
  , mkTests
  , runTests
  )
  where

-- TODO learn how to set multiple test patterns here, then plumb them through Config testpatterns
--      wait, no need for the plumbing! repl and tests are separate. remove testpatterns from Config

import OrthoLang.Debug
import OrthoLang.Types

import OrthoLang.Test.Scripts (mkTestGroup)
import Paths_OrthoLang       (getDataFileName)
import System.Directory      (getTemporaryDirectory, createDirectoryIfMissing, setCurrentDirectory)
import System.Environment    (setEnv, withArgs)
import System.FilePath.Posix ((</>))
import System.IO.Temp        (withTempDirectory)
import System.Process        (readCreateProcessWithExitCode, shell)
import Test.Tasty            (TestTree, defaultMain)
import System.Console.Docopt (Arguments, getArg, longOption)
import Data.Foldable (forM_)
-- import Data.List.Utils       (replace)

-- we just need <each of these>.mkTests
import qualified OrthoLang.Test.Versions as V
import qualified OrthoLang.Test.Parse    as P
import qualified OrthoLang.Test.Repl     as R
import qualified OrthoLang.Test.Scripts  as S
import qualified OrthoLang.Test.Help     as H

{-|
This is weird because all tests are always created;
filtering is done according to the TASTY_PATTERN environment var.
Gotcha: can't print the test pattern in place of "all tests"
because then they all match, ruining the filter.
-}
mkTests :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
mkTests cfg ref ids dRef = mkTestGroup cfg ref ids dRef "all tests" tests
  where
    tests  = [V.mkTests, H.mkTests, P.mkTests, R.mkTests, S.mkTests]

mkTestConfig :: Config -> FilePath -> Config
mkTestConfig cfg dir =
  -- let dir' = replace ":" "_" dir -- ':' messes with BLASTDB paths, and probably others
  -- in cfg
  cfg
    { script  = Nothing
    , tmpdir  = dir
    , workdir = dir
    , interactive = False -- except for the Repl tests
    , logfile = dir </> "log.txt"
    , report  = Just $ dir </> "report.html" -- TODO nothing?
    , history = Nothing -- TODO any reason to add it?
    , termcolumns = Just 100 -- TODO wasn't this set somewhere already?
    , shellaccess = True -- TODO test turning it off
    , progressbar = False -- TODO any way to test the bar?
    , showhidden = False -- TODO repl test turning it on
    , showtypes = False -- TODO repl test turning it on
    , autosave = False
    , outfile = Nothing -- TODO use this for test scripts?
    , shared = Nothing -- except for the tmpfile sharing tests
    -- TODO change debugregex?
    -- TODO change wrapper?
    -- TODO change outfile?
    }

dbg :: String -> IO ()
dbg = debug "test.runTests"

runTests :: Arguments -> Config -> LocksRef -> IDsRef -> DigestsRef -> IO ()
runTests args cfg ref ids dRef = withArgs [] $ do
  tmpRootDir <- getTemporaryDirectory -- can't share /tmp on the Berkeley cluster!
  createDirectoryIfMissing True tmpRootDir
  withTempDirectory tmpRootDir "ortholang" $ \tmpSubDir -> do
    dbg $ "created tmpdir " ++ tmpSubDir
    wd <- getDataFileName ""
    setCurrentDirectory wd -- TODO issue with this in the stack tests?
    dbg $ "working dir is " ++ wd
    -- TODO check exit code?
    -- each test can be multithreaded, but running more than one at a time
    -- confuses the stdout/stderr capturing
    setEnv "TASTY_NUM_THREADS" "1"
    setEnv "TASTY_TIMEOUT" "2m" -- TODO configure this?
    -- TODO can you also just export this before running ortholang?
    forM_
      (getArg args $ longOption "test") (setEnv "TASTY_PATTERN")
    let exSrc = wd </> "examples"
        exDst = tmpSubDir </> "examples"
    (_,_,_) <- readCreateProcessWithExitCode
      (shell $ unwords ["ln -s", exSrc, exDst]) ""
    dbg $ "created examples dir " ++ exDst
    tests <- mkTests (mkTestConfig cfg tmpSubDir) ref ids dRef
    defaultMain tests
