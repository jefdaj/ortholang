module ShortCut.Test
  ( TestTree
  , mkTests
  , runTests
  )
  where

import Paths_ShortCut        (getDataFileName)
import ShortCut.Core.Types   (CutConfig(..))
import ShortCut.Core.Util    (mkTestGroup)
import System.Directory      (setCurrentDirectory)
import System.Environment    (setEnv)
import System.FilePath.Posix ((</>))
import System.IO.Temp        (withSystemTempDirectory)
import System.Process        (readCreateProcessWithExitCode, shell)
import Test.Tasty            (TestTree, defaultMain)

import qualified ShortCut.Test.Deps    as D
import qualified ShortCut.Test.Parse   as P
import qualified ShortCut.Test.Repl    as R
import qualified ShortCut.Test.Scripts as S

-- TODO reorder these to start with the latest scripts?
mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "all tests"
  [ D.mkTests
  , P.mkTests
  , R.mkTests
  , S.mkTests
  ]

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

runTests :: CutConfig -> IO ()
runTests cfg = withSystemTempDirectory "shortcut" $ \td -> do
  wd <- getDataFileName ""
  setCurrentDirectory wd -- TODO issue with this in the stack tests?
  -- TODO check exit code?
  (_,_,_) <- readCreateProcessWithExitCode
               (shell $ unwords ["ln -s", wd </> "data", (td </> "data")]) ""
  tests <- mkTests $ mkTestConfig cfg td
  setEnv "LANG" "C"
  defaultMain tests
