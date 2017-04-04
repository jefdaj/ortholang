module ShortCut.Core.Tests where

import Control.Monad       (sequence)
import ShortCut.Core.Types (CutConfig)
import ShortCut.Core.Util  (mkTestGroup)
import Test.Tasty          (TestTree)

import qualified ShortCut.Core.Interpret.Tests as I
import qualified ShortCut.Core.Parse.Tests     as P
import qualified ShortCut.Core.Repl.Tests      as R

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Core"
  [  I.mkTests
  , P.mkTests
  , R.mkTests
  ]
