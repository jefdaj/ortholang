module ShortCut.Test
  ( TestTree
  , mkTests
  )
  where

import Control.Monad       (sequence)
import ShortCut.Core.Types (CutConfig)
import ShortCut.Core.Util  (mkTestGroup)
import Test.Tasty          (TestTree)

import qualified ShortCut.Test.Parse     as P
import qualified ShortCut.Test.Interpret as I
import qualified ShortCut.Test.Repl      as R

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Core"
  [ P.mkTests
  , I.mkTests
  , R.mkTests
  ]
