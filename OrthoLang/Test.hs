module OrthoLang.Test
  ( TestTree
  , mkTests
  , runTests
  )
  where

import OrthoLang.Core
import qualified OrthoLang.Util as U

import OrthoLang.Test.Repl   (mkTestGroup)
import Paths_OrthoLang       (getDataFileName)
import System.Console.Docopt (Arguments, longOption, getAllArgs)
import System.Directory      (getTemporaryDirectory, createDirectoryIfMissing, setCurrentDirectory)
import System.Environment    (setEnv, withArgs)
import System.FilePath.Posix ((</>))
import System.IO.Temp        (withTempDirectory)
import System.Process        (readCreateProcessWithExitCode, shell)
import Test.Tasty            (TestTree, defaultMain)

-- we just need <each of these>.mkTests
import qualified OrthoLang.Test.Versions as V
import qualified OrthoLang.Test.Parse    as P
import qualified OrthoLang.Test.Repl     as R
import qualified OrthoLang.Test.Scripts  as S

{-|
This is weird because all tests are always created;
filtering is done according to the TASTY_PATTERN environment var.
Gotcha: can't print the test pattern in place of "all tests"
because then they all match, ruining the filter.
-}
mkTests :: DigestsRef -> IO TestTree
mkTests cfg ref ids dRef = mkTestGroup cfg ref ids dRef "all tests" tests
  where
    tests  = [V.mkTests, P.mkTests, R.mkTests, S.mkTests]

mkTestConfig :: Config -> FilePath -> Config
mkTestConfig cfg dir = cfg
  { cfgScript  = Nothing
  , cfgTmpDir  = dir
  , cfgWorkDir = dir
  }

dbg :: String -> IO ()
dbg = U.debug "test.runTests"

runTests :: Arguments -> DigestsRef -> IO ()
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

    case getAllArgs args (longOption "test") of
      [] -> return ()
      ps -> do
        setEnv "TASTY_PATTERN" $ unwords ps
        setEnv "TASTY_TIMEOUT" "10m" -- TODO configure this?
    let exSrc = wd </> "examples"
        exDst = tmpSubDir </> "examples"
    (_,_,_) <- readCreateProcessWithExitCode
      (shell $ unwords ["ln -s", exSrc, exDst]) ""
    dbg $ "created examples dir " ++ exDst
    tests <- mkTests (mkTestConfig cfg tmpSubDir) ref ids dRef
    defaultMain tests
