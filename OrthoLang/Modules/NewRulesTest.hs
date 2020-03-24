module OrthoLang.Modules.NewRulesTest where

import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Compile.Basic (defaultTypeCheck)
import System.FilePath ((</>))

-- TODO fix digests:
import OrthoLang.Modules.SeqIO (faa)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "NewRulesTest"
  , mDesc = "Test module for the 'new rules' infrastructure"
  , mTypes = [str]
  , mFunctions =
      [ newRulesTest1
      ]
  }

newRulesTest1 :: OrthoLangFunction
newRulesTest1 = let name = "newrulestest1" in OrthoLangFunction
  { fNames     = [name]
  , fTypeDesc  = mkTypeDesc  name [str, str] str
  , fTypeCheck = tNewRulesTest1
  , fFixity    = Prefix, fTags = []
  , fOldRules = undefined
  , fNewRules = Just rNewRulesTest1
  }

tNewRulesTest1 :: TypeChecker
tNewRulesTest1 = defaultTypeCheck [str, str] str

rNewRulesTest1 :: OrthoLangConfig -> HashedIDsRef -> Rules ()
rNewRulesTest1 cfg iRef = do
  let exprDir = cfgTmpDir cfg </> "exprs"
      pattern = exprDir </> "newrulestest1" </> "*" </> "*" </> "*" </> "result"
  pattern %> \p -> aNewRulesTest1 cfg iRef (ExprPath p)
  return ()

aNewRulesTest1 :: OrthoLangConfig -> HashedIDsRef -> ExprPath -> Action ()
aNewRulesTest1 cfg iRef out = do
  (types, deps) <- liftIO $ decodeNewRulesDeps cfg iRef out
  liftIO $ putStrLn $ "aNewRulesTest1 types: " ++ show types
  liftIO $ putStrLn $ "aNewRulesTest1 typechecker says: " ++ show (tNewRulesTest1 types)
  liftIO $ putStrLn $ "aNewRulesTest1 deps: " ++ show deps
  undefined
