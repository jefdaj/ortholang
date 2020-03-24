module OrthoLang.Modules.NewRulesTest where

import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Paths (fromOrthoLangPath)
import OrthoLang.Core.Compile.Basic (defaultTypeCheck)
import OrthoLang.Core.Actions (writeCachedLines, need', readLit)
import System.FilePath ((</>))

-- TODO fix digests:
import OrthoLang.Modules.SeqIO (faa)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "Test"
  , mDesc = "Test module for the 'new rules' infrastructure"
  , mTypes = [str]
  , mFunctions =
      [ test1
      ]
  }

test1 :: OrthoLangFunction
test1 = let name = "newrulestest1" in OrthoLangFunction
  { fNames     = [name]
  , fTypeDesc  = mkTypeDesc  name [str, str] str
  , fTypeCheck = tTest1
  , fFixity    = Prefix, fTags = []
  , fOldRules = undefined
  , fNewRules = Just rTest1
  }

tTest1 :: TypeChecker
tTest1 = defaultTypeCheck [str, str] str

rTest1 :: OrthoLangConfig -> Locks -> HashedIDsRef -> Rules ()
rTest1 cfg lRef iRef = do
  let exprDir = cfgTmpDir cfg </> "exprs"
      pattern = exprDir </> "newrulestest1" </> "*" </> "*" </> "*" </> "result"
  pattern %> \p -> aTest1 cfg lRef iRef (ExprPath p)
  return ()

aTest1 :: OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> Action ()
aTest1 cfg lRef iRef o@(ExprPath out) = do
  (types, deps) <- liftIO $ decodeNewRulesDeps cfg iRef o
  let deps' = map (fromOrthoLangPath cfg) deps
  need' cfg lRef "ortholang.modules.newrulestest.test1" deps'
  s1 <- readLit cfg lRef $ deps' !! 0
  s2 <- readLit cfg lRef $ deps' !! 1
  -- liftIO $ putStrLn $ "aTest1 types: " ++ show types
  -- liftIO $ putStrLn $ "aTest1 typechecker says: " ++ show (tTest1 types)
  -- liftIO $ putStrLn $ "aTest1 deps: " ++ show deps
  writeCachedLines cfg lRef out ["result would go here, but for now these were the inputs:", s1, s2]
