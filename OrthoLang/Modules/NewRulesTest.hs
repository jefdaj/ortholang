module OrthoLang.Modules.NewRulesTest where

import OrthoLang.Core
-- import Development.Shake
import OrthoLang.Modules.SeqIO (faa)

olModule :: Module
olModule = Module
  { mName = "Test"
  , mDesc = "Test module for the 'new rules' infrastructure"
  , mTypes = [str]
  , mFunctions =
      [ test1
      , test2
      , test3
      ]
  }

test1 :: Function
test1 = newFnA2 "newrulestest1" (str, str) str aTest1

-- TODO make these all Paths?
aTest1 :: NewAction2
aTest1 (ExprPath out) a1 a2 = do
  s1 <- readLit a1
  s2 <- readLit a2
  writeCachedLines out ["result would go here, but for now these were the inputs:", s1, s2]

test2 :: Function
test2 = newFnA3 "newrulestest2" (str, faa, faa) faa aTest2

aTest2 :: NewAction3
aTest2 (ExprPath out) a1 a2 a3 = writeCachedLines out ["inputs:", a1, a2, a3]

-- TODO any obvious way to constrain the number of args?
test3 :: Function
test3 = newMacro "newrulestest3" [str, str, str] str mTest3

mTest3 :: MacroExpansion
mTest3 _ (Fun _ _ _ _ _) = undefined
mTest3 _ e = error $ "bad argument to mTest3: " ++ show e
