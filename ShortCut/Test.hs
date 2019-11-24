module ShortCut.Test
  ( TestTree
  , mkTests
  , runTests
  )
  where

import Paths_ShortCut        (getDataFileName)
import ShortCut.Core.Types   (CutConfig(..), Locks, HashedIDsRef)
import ShortCut.Test.Repl    (mkTestGroup)
import System.Directory      (getTemporaryDirectory, createDirectoryIfMissing,
                              setCurrentDirectory)
import System.Environment    (setEnv, withArgs)
import System.FilePath.Posix ((</>))
import System.IO.Temp        (withTempDirectory)
import System.Process        (readCreateProcessWithExitCode, shell)
import Test.Tasty            (TestTree, defaultMain)
-- import Data.IORef            (IORef)

import System.Console.Docopt (Arguments, longOption, getAllArgs)
import ShortCut.Core.Util (trace, debug)

import qualified ShortCut.Test.Versions as V
import qualified ShortCut.Test.Parse    as P
import qualified ShortCut.Test.Repl     as R
import qualified ShortCut.Test.Scripts  as S

-- This is weird because all tests are always created;
-- filtering is done according to the TASTY_PATTERN environment var.
-- Gotcha: can't print the test pattern in place of "all tests"
-- because then they all match, ruining the filter.
mkTests :: CutConfig -> Locks -> HashedIDsRef -> IO TestTree
mkTests cfg ref ids = mkTestGroup cfg ref ids "all tests" tests
  where
    tests  = [V.mkTests, P.mkTests, R.mkTests, S.mkTests]

{- Tasty uses a subset of AWK to match patterns.
 - I thought that was confusing, so this generates it from a simple list.
 - If you find a need for more expressive patterns, email and I'll add them back!
 - TODO is this broken for multiple patterns? fix or remove
 -}
mkTastyPattern :: [String] -> String
mkTastyPattern ps = trace "test.mkTastyPatttern" msg awkPtn
  where
    matchPatterns = map ("$0 ~ " ++) $ map (\p -> "/" ++ p ++ "/") ps
    awkPtn = foldr1 (\a b -> a ++ " || " ++ b) matchPatterns
    msg = show ps ++ " -> '" ++ awkPtn ++ "'"

mkTestConfig :: CutConfig -> FilePath -> CutConfig
mkTestConfig cfg dir = cfg
  { cfgScript  = Nothing
  , cfgTmpDir  = dir
  , cfgWorkDir = dir
  -- , cfgDebug   = Nothing
  -- , cfgModules = mods
  -- , cfgWrapper = Nothing -- TODO test this?
  -- , cfgReport  = Nothing
  }

runTests :: Arguments -> CutConfig -> Locks -> HashedIDsRef -> IO ()
runTests args cfg ref ids = withArgs [] $ do
  let dbg = debug "test.runTests"
  tmpRootDir <- getTemporaryDirectory -- can't share /tmp on the Berkeley cluster!
  createDirectoryIfMissing True tmpRootDir
  withTempDirectory tmpRootDir "shortcut" $ \tmpSubDir -> do
    dbg $ "created tmpdir " ++ tmpSubDir
    wd <- getDataFileName ""
    setCurrentDirectory wd -- TODO issue with this in the stack tests?
    dbg $ "working dir is " ++ wd
    -- TODO check exit code?
    setEnv "TASTY_NUM_THREADS" "1" -- TODO can more be done without repl issues?
    case getAllArgs args (longOption "test") of
      [] -> return ()
      ps -> setEnv "TASTY_PATTERN" $ mkTastyPattern ps
    let dataSrc = wd </> "data"
        dataDst = tmpSubDir </> "data"
    -- TODO why not use the symlink function here?
    (_,_,_) <- readCreateProcessWithExitCode
      (shell $ unwords ["ln -s", dataSrc, dataDst]) ""
    dbg $ "created data dir " ++ dataDst
    tests <- mkTests (mkTestConfig cfg tmpSubDir) ref ids
    defaultMain tests
