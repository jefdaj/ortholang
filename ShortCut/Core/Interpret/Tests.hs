module ShortCut.Core.Interpret.Tests where

import Test.Hspec

-- TODO import qualified ShortCut.Core.Interpret.Parse.Tests as P
-- import ShortCut.Core.Interpret.Compile
-- import ShortCut.Core.Interpret.Parse
-- import ShortCut.Core.Interpret.ParseSpec
-- import ShortCut.Core.Types

spec :: Spec
spec = do
  describe "interprets ShortCut code" $ do
    describe "iScript" $ do
      it "[c]ompiles expressions to Shake rules" $ pending
