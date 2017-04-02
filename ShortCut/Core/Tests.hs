module ShortCut.Core.Tests where

import Test.Tasty
import qualified ShortCut.Core.Parse.Tests     as P
-- import qualified ShortCut.Core.Repl.Tests      as R
-- import qualified ShortCut.Core.Interpret.Tests as I

tests :: TestTree
tests = testGroup "Core" [P.tests]

main :: IO ()
main = defaultMain tests
