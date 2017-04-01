module ShortCut.Core.Tests where

import Test.Hspec

import qualified ShortCut.Core.Parse.Tests     as P
import qualified ShortCut.Core.Repl.Tests      as R
import qualified ShortCut.Core.Interpret.Tests as I

spec :: Spec
spec = do
  describe "ShortCut.Core.Parse"     P.spec
  describe "ShortCut.Core.Repl"      R.spec
  describe "ShortCut.Core.Interpret" I.spec

main :: IO ()
main = hspec spec
