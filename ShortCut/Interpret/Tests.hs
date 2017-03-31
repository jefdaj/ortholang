module ShortCut.Interpret.Tests where

import Test.Hspec

-- TODO import qualified ShortCut.Interpret.Parse.Tests as P
-- import ShortCut.Interpret.Compile
-- import ShortCut.Interpret.Parse
-- import ShortCut.Interpret.ParseSpec
-- import ShortCut.Types

spec :: Spec
spec = do
  describe "interprets ShortCut code" $ do
    describe "iScript" $ do
      it "[c]ompiles expressions to Shake rules" $ pending
