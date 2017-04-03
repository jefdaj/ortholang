module ShortCut.Tests where

import Test.Tasty (TestTree)
import qualified ShortCut.Core.Tests as C
import ShortCut.Core.Types (CutConfig)
import ShortCut.Core.Util (mkTestGroup)

-- tests :: TestTree
-- tests = testGroup "ShortCut" [C.tests]

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "ShortCut" [C.mkTests]

-- main :: IO ()
-- main = mkTests cfg >>= defaultMain
