module OrthoLang.Test
  ( TestTree
  , mkTests
  , runTests
  )
  where

import Paths_OrthoLang        (getDataFileName)
import OrthoLang.Core.Types   (OrthoLangConfig(..), Locks, HashedIDsRef)
import OrthoLang.Test.Repl    (mkTestGroup)
import System.Directory      (getTemporaryDirectory, createDirectoryIfMissing,
                              setCurrentDirectory)
import System.Environment    (setEnv, withArgs)
import System.FilePath.Posix ((</>))
import System.IO.Temp        (withTempDirectory)
import System.Process        (readCreateProcessWithExitCode, shell)
import Test.Tasty            (TestTree, defaultMainWithIngredients)
import Test.Tasty.Ingredients.ConsoleReporter (consoleTestReporter)
import Test.Tasty.Ingredients.Rerun (rerunningTests)
-- import Data.IORef            (IORef)

import System.Console.Docopt (Arguments, longOption, getAllArgs)
import OrthoLang.Core.Util (trace, debug)

import qualified OrthoLang.Test.Versions as V
import qualified OrthoLang.Test.Parse    as P
import qualified OrthoLang.Test.Repl     as R
import qualified OrthoLang.Test.Scripts  as S

-- This is weird because all tests are always created;
-- filtering is done according to the TASTY_PATTERN environment var.
-- Gotcha: can't print the test pattern in place of "all tests"
-- because then they all match, ruining the filter.
mkTests :: OrthoLangConfig -> Locks -> HashedIDsRef -> IO TestTree
mkTests cfg ref ids = setPtn >> setLog >> mkTestGroup cfg ref ids "all tests" tests
  where
    tests  = [V.mkTests, P.mkTests, R.mkTests, S.mkTests]
    setLog = case cfgFailLog cfg of
               Nothing -> return ()
               Just p  -> do
                 -- TODO why don't these work? (maybe it's insensitive to cli options?)
                 setEnv "TASTY_RERUN_UPDATE" "True"
                 setEnv "TASTY_RERUN_LOG_FILE" p
                 setEnv "TASTY_RERUN_FILTER" "failures,new"

mkTestConfig :: OrthoLangConfig -> FilePath -> OrthoLangConfig
mkTestConfig cfg dir = cfg
  { cfgScript  = Nothing
  , cfgTmpDir  = dir
  , cfgWorkDir = dir
  -- , cfgDebug   = Nothing
  -- , cfgModules = mods
  -- , cfgWrapper = Nothing -- TODO test this?
  -- , cfgReport  = Nothing
  }

runTests :: Arguments -> OrthoLangConfig -> Locks -> HashedIDsRef -> IO ()
runTests args cfg ref ids = withArgs [] $ do
  let dbg = debug "test.runTests"
  tmpRootDir <- getTemporaryDirectory -- can't share /tmp on the Berkeley cluster!
  createDirectoryIfMissing True tmpRootDir
  withTempDirectory tmpRootDir "ortholang" $ \tmpSubDir -> do
    dbg $ "created tmpdir " ++ tmpSubDir
    wd <- getDataFileName ""
    setCurrentDirectory wd -- TODO issue with this in the stack tests?
    dbg $ "working dir is " ++ wd
    -- TODO check exit code?
    setEnv "TASTY_NUM_THREADS" "1" -- TODO can more be done without repl issues?
    case getAllArgs args (longOption "test") of
      [] -> return ()
      ps -> setEnv "TASTY_PATTERN" $ unwords ps
    let exSrc = wd </> "examples"
        exDst = tmpSubDir </> "examples"
    -- TODO why not use the symlink function here?
    (_,_,_) <- readCreateProcessWithExitCode
      (shell $ unwords ["ln -s", exSrc, exDst]) ""
    dbg $ "created examples dir " ++ exDst
    tests <- mkTests (mkTestConfig cfg tmpSubDir) ref ids
    defaultMainWithIngredients [rerunningTests [consoleTestReporter]] tests
