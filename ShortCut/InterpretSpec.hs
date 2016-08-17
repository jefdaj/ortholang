module ShortCut.InterpretSpec where

-- import ShortCut.Interpret.Compile
-- import ShortCut.Interpret.Parse
-- import ShortCut.Interpret.ParseSpec
-- import ShortCut.Types
import Test.Hspec

spec :: Spec
spec = do
  describe "interprets ShortCut code" $ do
    describe "iScript" $ do
      it "[c]ompiles expressions to Shake rules" $ pending
