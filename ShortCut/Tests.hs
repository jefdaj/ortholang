module ShortCut.Tests where

-- TODO reorganize so all tests go here, and make a Docs module too
--      ShortCut.Test.Repl, ShortCut.Test.Parse, etc

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
