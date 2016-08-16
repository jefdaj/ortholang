module ShortCut.InterpretSpec where

import ShortCut.Interpret.Parse
import ShortCut.Interpret.ParseSpec
import ShortCut.Types
import Test.Hspec

-- TODO remove these once rewrite is finished
undoAll :: IO ([ParsedScript], [TypedScript], [ParsedScript])
undoAll = do
  Right parsed <- pExCuts
  let tmps   = map (\p -> runCutM (tScript p) [] []) parsed
      typed  = map (\((Right s, _, _)) -> s) tmps
      undone = map uScript typed
  return (parsed, typed, undone)

spec :: Spec
spec = do
  describe "interprets ShortCut code" $ do

    -- TODO remove these once rewrite is finished
    describe "uExpr" $ do
      it "undoes tExpr" $ do
        (parsed, _, undone) <- undoAll
        let parsed' = (map . map) snd parsed
            undone' = (map . map) snd undone
            pairs   = zip parsed' undone'
        mapM_ (\(a,b) -> a `shouldBe` b) pairs

    -- TODO remove these once rewrite is finished
    describe "uScript" $ do
      it "undoes tScript" $ do
        (parsed, _, undone) <- undoAll
        parsed `shouldBe` undone
