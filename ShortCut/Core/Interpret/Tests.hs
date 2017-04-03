module ShortCut.Core.Interpret.Tests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden
import System.FilePath.Glob (globDir1)
import Paths_ShortCut (getDataFileName)

-- TODO import qualified ShortCut.Core.Interpret.Parse.Tests as P
-- import ShortCut.Core.Interpret.Compile
-- import ShortCut.Core.Interpret.Parse
-- import ShortCut.Core.Interpret.ParseSpec
-- import ShortCut.Core.Types

tests :: TestTree
tests = testGroup "Interpret" []
