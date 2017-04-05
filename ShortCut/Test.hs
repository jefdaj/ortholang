module ShortCut.Test
  ( TestTree
  , runTests
  )
  where

import Control.Monad       (sequence)
import ShortCut.Core.Types (CutConfig(..))
import ShortCut.Core.Util  (mkTestGroup)
import System.IO.Temp      (withSystemTempDirectory)
import Test.Tasty          (TestTree)
import Test.Tasty          (defaultMain)

import qualified ShortCut.Test.Parse     as P
import qualified ShortCut.Test.Interpret as I
import qualified ShortCut.Test.Repl      as R

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Core"
  [ P.mkTests
  , I.mkTests
  , R.mkTests
  ]

mkTestConfig :: FilePath -> CutConfig
mkTestConfig dir = CutConfig
  { cfgScript  = Nothing
  , cfgTmpDir  = dir
  , cfgVerbose = True
  }

runTests :: IO ()
runTests = withSystemTempDirectory "shortcut" $ \d -> do
  tests <- mkTests $ mkTestConfig d
  defaultMain tests
