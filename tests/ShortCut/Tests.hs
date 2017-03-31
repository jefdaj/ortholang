module ShortCut.Tests where

import Test.Hspec

import qualified ShortCut.Types.Tests           as T
-- import qualified ShortCut.Interpret.Parse.Tests as P TODO update these!
import qualified ShortCut.Repl.Tests            as R
import qualified ShortCut.Interpret.Tests       as I

spec :: Spec
spec = do
  describe "ShortCut.Types" T.spec
  -- describe "ShortCut.Parse"     P.spec
  describe "ShortCut.Repl"      R.spec
  describe "ShortCut.Interpret" I.spec
