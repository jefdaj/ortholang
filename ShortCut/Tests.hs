module ShortCut.Tests where

import Test.Tasty
import qualified ShortCut.Core.Tests as C

tests :: TestTree
tests = testGroup "ShortCut" [C.tests]

main :: IO ()
main = defaultMain tests
