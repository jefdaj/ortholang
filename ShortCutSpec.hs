{- Runs all the Hspec tests.
 - You can also run the nested Spec.hs files independently like:
 - `runhaskell ShortCut/ParseSpec.hs`
 - `ghc --make ShortCut/ParseSpec.hs -o parse-spec && ./parse-spec`.
 - See: https://github.com/hspec/hspec-example
 -}

-- TODO incorporate golden testing (it seems simple enough)
-- https://hackage.haskell.org/package/test-framework-golden-1.1.3.3

import Test.Hspec

import qualified ShortCut.TypesSpec           as T
import qualified ShortCut.Interpret.ParseSpec as P
import qualified ShortCut.ReplSpec            as R

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ShortCut.TypesSpec" T.spec
  describe "ShortCut.Parse"     P.spec
  describe "ShortCut.Repl"      R.spec
