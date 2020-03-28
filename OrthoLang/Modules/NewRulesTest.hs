module OrthoLang.Modules.NewRulesTest where

import OrthoLang.Core.Types
import OrthoLang.Core.Compile (mkNewFn2, mkNewFn3)
import OrthoLang.Core.Actions (writeCachedLines, readLit)
import OrthoLang.Modules.SeqIO (faa)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans (lift)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "Test"
  , mDesc = "Test module for the 'new rules' infrastructure"
  , mTypes = [str]
  , mFunctions =
      [ test1
      , test2
      ]
  }

test1 :: Function
test1 = mkNewFn2 "newrulestest1" Nothing str [str, str] aTest1

-- TODO make these all Paths?
aTest1 :: ActionR2
aTest1 (ExprPath out) d1 d2 = do
  (cfg, lRef, _, _) <- ask
  s1 <- lift $ readLit cfg lRef d1
  s2 <- lift $ readLit cfg lRef d2
  lift $ writeCachedLines cfg lRef out ["result would go here, but for now these were the inputs:", s1, s2]

test2 :: Function
test2 = mkNewFn3 "newrulestest2" Nothing faa [str, faa, faa] aTest2

aTest2 :: ActionR3
aTest2 (ExprPath out) a1 a2 a3 = do
  (cfg, lRef, _, _) <- ask
  lift $ writeCachedLines cfg lRef out ["inputs:", a1, a2, a3]
