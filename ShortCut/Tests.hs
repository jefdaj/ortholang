module ShortCut.Tests where

import Test.Hspec
import qualified ShortCut.Core.Tests as C

spec :: Spec
spec = do
  describe "ShortCut.Core" C.spec

main :: IO ()
main = hspec spec
