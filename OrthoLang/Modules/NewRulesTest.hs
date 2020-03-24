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
  , fNewRules = Just $ rNewRules name 2 tTest1 aTest1
  }

tTest1 :: TypeChecker
tTest1 = defaultTypeCheck [str, str] str

-- TODO ExprPaths for deps?
-- TODO or OrthoLangPaths throughout?
-- TODO can you encode NewAction1, 2, 3... easily?
type NewAction  = OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> [FilePath] -> Action ()
type NewRulesFn = OrthoLangConfig -> Locks -> HashedIDsRef -> Rules ()

rNewRules :: String -> Int -> TypeChecker -> NewAction -> NewRulesFn
rNewRules name nArgs tFn aFn cfg lRef iRef = do
  let exprDir = cfgTmpDir cfg </> "exprs"
      pattern = exprDir </> name </> (foldl1 (</>) (take (nArgs+1) $ repeat "*")) </> "result"
  pattern %> \p -> aNewRules tFn aFn cfg lRef iRef (ExprPath p)
  return ()

aNewRules :: TypeChecker -> NewAction -> OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> Action ()
aNewRules tFn aFn cfg lRef iRef o@(ExprPath out) = do
  (oType, dTypes, deps) <- liftIO $ decodeNewRulesDeps cfg iRef o
  case tFn dTypes of
    Left err -> error err
    Right rType -> do
      when (rType /= oType) $ error $ "typechecking error: " ++ show rType ++ " /= " ++ show oType
      let deps' = map (fromOrthoLangPath cfg) deps
      need' cfg lRef "ortholang.modules.newrulestest.test1" deps'
      -- TODO look up out too and assert that its type matches typechecker result
      -- liftIO $ putStrLn $ "aNewRules dTypes: " ++ show dTypes
      -- liftIO $ putStrLn $ "aNewRules typechecker says: " ++ show (tFn dTypes)
      -- liftIO $ putStrLn $ "aNewRules deps: " ++ show deps
      aFn cfg lRef iRef o deps'

aTest1 :: NewAction
aTest1 cfg lRef iRef (ExprPath out) deps' = do
  s1 <- readLit cfg lRef $ deps' !! 0
  s2 <- readLit cfg lRef $ deps' !! 1
  writeCachedLines cfg lRef out ["result would go here, but for now these were the inputs:", s1, s2]
