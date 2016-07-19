-- Runs all the Hspec tests.
-- You can also run the nested Spec.hs files independently like:
-- `runhaskell ShortCut/Parse/Spec.hs`
-- `ghc --make ShortCut/Parse/Spec.hs -o parse-spec && ./parse-spec`.
-- See: https://github.com/hspec/hspec-example

-- TODO incorporate golden testing (it seems simple enough)
-- https://hackage.haskell.org/package/test-framework-golden-1.1.3.3

import Test.Hspec

import qualified ShortCut.ParseSpec as P
import qualified ShortCut.TypeCheckSpec as T
import qualified ShortCut.EvalSpec  as E

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ShortCut.Parse"     P.spec
  describe "ShortCut.TypeCheck" T.spec
  describe "ShortCut.Eval"      E.spec 
