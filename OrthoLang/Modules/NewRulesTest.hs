module OrthoLang.Modules.NewRulesTest where

import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Compile.Basic (defaultTypeCheck)
import OrthoLang.Modules.SeqIO (faa)
import System.FilePath ((</>))

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "NewRulesTest"
  , mDesc = "Test module for the 'new rules' infrastructure"
  , mTypes = [str, faa]
  , mFunctions =
      [ newRulesTest1
      ]
  }

newRulesTest1 :: OrthoLangFunction
newRulesTest1 = let name = "newrulestest1" in OrthoLangFunction
  { fNames     = [name]
  , fTypeDesc  = mkTypeDesc  name [str, faa] str
  , fTypeCheck = defaultTypeCheck [str, faa] str
  , fFixity    = Prefix, fTags = []
  , fOldRules = undefined
  , fNewRules = Just rNewRulesTest1
  }

rNewRulesTest1 :: OrthoLangConfig -> HashedIDsRef -> Rules ()
rNewRulesTest1 cfg iRef = do
  let exprDir = cfgTmpDir cfg </> "exprs"
      pattern = exprDir </> "newrulestest1" </> "*" </> "*" </> "*" </> "result"
  pattern %> aNewRulesTest1 cfg iRef
  return ()

aNewRulesTest1 :: OrthoLangConfig -> HashedIDsRef -> FilePath -> Action ()
aNewRulesTest1 cfg iRef out = do
  deps <- liftIO $ decodeNewRulesDeps cfg iRef out
  liftIO $ putStrLn $ "deps: " ++ show deps
  undefined
