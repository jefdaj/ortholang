module Main where

{- Runs all the Hspec tests.
 - You can also run the nested Spec.hs files independently like:
 - `runhaskell ShortCut/Parse/Tests.hs`
 - `ghc --make ShortCut/Parse/Tests.hs -o parse-spec && ./parse-spec`.
 - See: https://github.com/hspec/hspec-example
 -}

-- TODO incorporate golden testing (it seems simple enough)
-- https://hackage.haskell.org/package/test-framework-golden-1.1.3.3

-- TODO test for freeze when running the actual binary on a minimal script

import Test.Hspec
import ShortCut.Tests (spec)

main :: IO ()
main = hspec spec
