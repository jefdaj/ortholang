module ShortCut.Test
  ( TestTree
  , mkTests
  , runTests
  )
  where

import Paths_ShortCut        (getDataFileName)
import ShortCut.Core.Types   (CutConfig(..), Locks)
import ShortCut.Test.Repl    (mkTestGroup)
import System.Directory      (setCurrentDirectory)
import System.Environment    (getEnv, setEnv)
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
mkTests :: CutConfig -> Locks -> IO TestTree
mkTests cfg ref = setPtn >> mkTestGroup cfg ref "all tests" tests
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

runTests :: CutConfig -> Locks -> IO ()
runTests cfg ref = do
  tmpRootDir <- getEnv "TMPDIR" -- can't share /tmp on the Berkeley cluster!
  withTempDirectory tmpRootDir "shortcut" $ \tmpSubDir -> do
    wd <- getDataFileName ""
    setCurrentDirectory wd -- TODO issue with this in the stack tests?
    -- TODO check exit code?
    setEnv "LANG" "C" -- TODO would something UTF-8 be better?
    -- setEnv "TASTY_NUM_THREADS" "10"
    (_,_,_) <- readCreateProcessWithExitCode
      (shell $ unwords ["ln -s", wd </> "data", (tmpSubDir </> "data")]) ""
    tests <- mkTests (mkTestConfig cfg tmpSubDir) ref
    -- setCurrentDirectory $ cfgWorkDir cfg
    defaultMain tests
