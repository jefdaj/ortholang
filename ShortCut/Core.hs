module ShortCut.Core
  ( repl
  , spec
  , CutConfig(..)
  )
  where

import ShortCut.Core.Repl
import ShortCut.Core.Tests
import ShortCut.Core.Types
import Test.Hspec

-- TODO is this needed here, or just in Tests.hs?
main :: IO ()
main = hspec spec
