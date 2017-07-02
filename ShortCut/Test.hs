module ShortCut.Test
  ( TestTree
  , mkTests
  , runTests
  )
  where

-- import Control.Monad       (sequence)
import ShortCut.Core.Types (CutConfig(..), CutModule)
import ShortCut.Core.Util  (mkTestGroup)
import System.IO.Temp      (withSystemTempDirectory)
import Test.Tasty          (TestTree, defaultMain)
import System.Environment  (setEnv)
import Paths_ShortCut      (getDataFileName)
import System.FilePath.Posix ((</>))
import System.Process        (readCreateProcessWithExitCode, shell)

import qualified ShortCut.Test.Parse as P
import qualified ShortCut.Test.Eval  as E
import qualified ShortCut.Test.Repl  as R
import qualified ShortCut.Test.Deps  as D

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Test"
  [ D.mkTests
  , P.mkTests
  , E.mkTests
  , R.mkTests
  ]

mkTestConfig :: [CutModule] -> FilePath -> CutConfig
mkTestConfig mods dir = CutConfig
  { cfgScript  = Nothing
  , cfgTmpDir  = dir
  , cfgDebug   = False
  , cfgModules = mods
  }

-- TODO is d also the script's cwd where data files should go?
runTests :: [CutModule] -> IO ()
runTests mods = withSystemTempDirectory "shortcut" $ \d -> do
  dataDir <- getDataFileName "data"
  -- TODO check exit code?
  (_,_,_) <- readCreateProcessWithExitCode (shell $ unwords ["ln -s", dataDir, (d </> "data")]) ""
  tests <- mkTests $ mkTestConfig mods d
  setEnv "LANG" "C"
  -- setEnv "TASTY_HIDE_SUCCESSES" "True" -- TODO flag for this?
  defaultMain tests
