module ShortCut.Core.Tests where

import Control.Monad (sequence)
import Test.Tasty (TestTree)
import ShortCut.Core.Types (CutConfig)
import ShortCut.Core.Util (mkTestGroup)

-- import qualified ShortCut.Core.Interpret.Tests as I
-- import qualified ShortCut.Core.Parse.Tests     as P
import qualified ShortCut.Core.Repl.Tests      as R
-- import qualified ShortCut.Core.Util.Tests      as U

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Core" [R.mkTests]
