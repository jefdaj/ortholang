module OrthoLang.Modules.NewRulesTest where

import OrthoLang.Core.Types
import OrthoLang.Core.Compile (mkNewFn2, mkNewFn3)
import OrthoLang.Core.Actions (writeCachedLines, readLit)
import OrthoLang.Modules.SeqIO (faa)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "Test"
  , mDesc = "Test module for the 'new rules' infrastructure"
  , mTypes = [str]
  , mFunctions =
      [ test1
      , test2
      ]
  }

test1 :: OrthoLangFunction
test1 = mkNewFn2 "newrulestest1" str [str, str] aTest1

-- TODO make these all OrthoLangPaths?
aTest1 :: NewAction2
aTest1 cfg lRef _ (ExprPath out) d1 d2 = do
  s1 <- readLit cfg lRef d1
  s2 <- readLit cfg lRef d2
  writeCachedLines cfg lRef out ["result would go here, but for now these were the inputs:", s1, s2]

test2 :: OrthoLangFunction
test2 = mkNewFn3 "newrulestest2" faa [str, faa, faa] aTest2

aTest2 :: NewAction3
aTest2 cfg lRef _ (ExprPath out) a1 a2 a3 = writeCachedLines cfg lRef out ["inputs:", a1, a2, a3]
