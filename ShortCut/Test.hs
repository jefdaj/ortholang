module ShortCut.Test
  ( TestTree
  , mkTests
  , runTests
  )
  where

import Paths_ShortCut        (getDataFileName)
import ShortCut.Core.Types   (CutConfig(..), Locks, HashedSeqIDsRef)
import ShortCut.Test.Repl    (mkTestGroup)
import System.Directory      (getTemporaryDirectory, createDirectoryIfMissing,
                              setCurrentDirectory)
import System.Environment    (setEnv)
import System.FilePath.Posix ((</>))
import System.IO.Temp        (withTempDirectory)
import System.Process        (readCreateProcessWithExitCode, shell)
import Test.Tasty            (TestTree, defaultMain)
-- import Data.IORef            (IORef)

import qualified ShortCut.Test.Deps    as D
import qualified ShortCut.Test.Parse   as P
import qualified ShortCut.Test.Repl    as R
import qualified ShortCut.Test.Scripts as S

-- This is weird because all tests are always created;
-- filtering is done according to the TASTY_PATTERN environment var.
-- Gotcha: can't print the test pattern in place of "all tests"
-- because then they all match, ruining the filter.
mkTests :: CutConfig -> Locks -> HashedSeqIDsRef -> IO TestTree
mkTests cfg ref ids = setPtn >> mkTestGroup cfg ref ids "all tests" tests
  where
    tests  = [D.mkTests, P.mkTests, R.mkTests, S.mkTests]
    setPtn = case cfgTestPtn cfg of
               Nothing  -> return ()
               Just ptn -> setEnv "TASTY_PATTERN" ptn

mkTestConfig :: CutConfig -> FilePath -> CutConfig
mkTestConfig cfg dir = cfg
  { cfgScript  = Nothing
  , cfgTmpDir  = dir
  , cfgWorkDir = dir
  , cfgDebug   = False
  -- , cfgModules = mods
  -- , cfgWrapper = Nothing -- TODO test this?
  -- , cfgReport  = Nothing
  }

runTests :: CutConfig -> Locks -> HashedSeqIDsRef -> IO ()
runTests cfg ref ids = do
  tmpRootDir <- getTemporaryDirectory -- can't share /tmp on the Berkeley cluster!
  createDirectoryIfMissing True tmpRootDir
  withTempDirectory tmpRootDir "shortcut" $ \tmpSubDir -> do
    wd <- getDataFileName ""
    setCurrentDirectory wd -- TODO issue with this in the stack tests?
    -- TODO check exit code?
    setEnv "TASTY_NUM_THREADS" "1" -- TODO can more be done without repl issues?
    -- setEnv "TASTY_QUIET" "True"
    (_,_,_) <- readCreateProcessWithExitCode
      (shell $ unwords ["ln -s", wd </> "data", (tmpSubDir </> "data")]) ""
    tests <- mkTests (mkTestConfig cfg tmpSubDir) ref ids
    -- setCurrentDirectory $ cfgWorkDir cfg
    defaultMain tests
