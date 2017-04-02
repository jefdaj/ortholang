module ShortCut.Core.Tests where

import Test.Tasty (TestTree, testGroup)

import qualified ShortCut.Core.Interpret.Tests as I
import qualified ShortCut.Core.Parse.Tests     as P
import qualified ShortCut.Core.Repl.Tests      as R
import qualified ShortCut.Core.Util.Tests      as U

tests :: TestTree
tests = testGroup "Core" [P.tests, R.tests, I.tests, U.tests]
