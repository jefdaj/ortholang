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
import Test.Tasty          (TestTree)
import Test.Tasty          (defaultMain)
import System.Environment  (setEnv)

import qualified ShortCut.Test.Parse as P
import qualified ShortCut.Test.Eval  as E
import qualified ShortCut.Test.Repl  as R

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Core"
  [ P.mkTests
  , E.mkTests
  , R.mkTests
  ]

mkTestConfig :: [CutModule] -> FilePath -> CutConfig
mkTestConfig mods dir = CutConfig
  { cfgScript  = Nothing
  , cfgTmpDir  = dir
  , cfgVerbose = True
  , cfgModules = mods
  }

runTests :: [CutModule] -> IO ()
runTests mods = withSystemTempDirectory "shortcut" $ \d -> do
  tests <- mkTests $ mkTestConfig mods d
  setEnv "LANG" "C"
  -- setEnv "TASTY_HIDE_SUCCESSES" "True" -- TODO shortcut flag for this?
  defaultMain tests
