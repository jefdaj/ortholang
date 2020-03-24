module OrthoLang.Modules.NewRulesTest where

import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Paths (fromOrthoLangPath, decodeNewRulesDeps)
import OrthoLang.Core.Compile.Basic (defaultTypeCheck)
import OrthoLang.Core.Actions (writeCachedLines, need', readLit)
import System.FilePath ((</>))
import Control.Monad (when)

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
  , fNewRules = Just $ rNewRules tTest1 aTest1
  }

tTest1 :: TypeChecker
tTest1 = defaultTypeCheck [str, str] str

type NewAction = TypeChecker -> OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> Action ()
type NewRulesFn = OrthoLangConfig -> Locks -> HashedIDsRef -> Rules ()

rNewRules :: TypeChecker -> NewAction -> NewRulesFn
rNewRules tFn aFn cfg lRef iRef = do
  let exprDir = cfgTmpDir cfg </> "exprs"
      pattern = exprDir </> "newrulestest1" </> "*" </> "*" </> "*" </> "result"
  pattern %> \p -> aFn tFn cfg lRef iRef (ExprPath p)
  return ()

aTest1 :: NewAction
aTest1 tFn cfg lRef iRef o@(ExprPath out) = do
  (oType, dTypes, deps) <- liftIO $ decodeNewRulesDeps cfg iRef o
  case tFn dTypes of
    Left err -> error err
    Right rType -> do
      when (rType /= oType) $ error $ "typechecking error: " ++ show rType ++ " /= " ++ show oType
      let deps' = map (fromOrthoLangPath cfg) deps
      need' cfg lRef "ortholang.modules.newrulestest.test1" deps'
      s1 <- readLit cfg lRef $ deps' !! 0
      s2 <- readLit cfg lRef $ deps' !! 1
      -- TODO look up out too and assert that its type matches typechecker result
      -- liftIO $ putStrLn $ "aTest1 dTypes: " ++ show dTypes
      liftIO $ putStrLn $ "aTest1 typechecker says: " ++ show (tFn dTypes)
      -- liftIO $ putStrLn $ "aTest1 deps: " ++ show deps
      writeCachedLines cfg lRef out ["result would go here, but for now these were the inputs:", s1, s2]
