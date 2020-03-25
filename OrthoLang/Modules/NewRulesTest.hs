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

------------------------------
-- new rules infrastructure --
------------------------------

-- TODO ExprPaths for deps?
-- TODO or OrthoLangPaths throughout?
-- TODO can you encode NewAction1, 2, 3... easily?

type NewAction1 = OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> FilePath                         -> Action ()
type NewAction2 = OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> FilePath -> FilePath             -> Action ()
type NewAction3 = OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> FilePath -> FilePath -> FilePath -> Action ()

type NewRulesFn = OrthoLangConfig -> Locks -> HashedIDsRef -> Rules ()

rNewRules1 :: String -> TypeChecker -> NewAction1 -> NewRulesFn
rNewRules1 = rNewRules 1 $ \fn deps -> fn (deps !! 0)

rNewRules2 :: String -> TypeChecker -> NewAction2 -> NewRulesFn
rNewRules2 = rNewRules 2 $ \fn deps -> fn (deps !! 0) (deps !! 1)

rNewRules3 :: String -> TypeChecker -> NewAction3 -> NewRulesFn
rNewRules3 = rNewRules 3 $ \fn deps -> fn (deps !! 0) (deps !! 1) (deps !! 2)

-- TODO can you add more rules simply by doing >> moreRulesFn after this?
rNewRules
  :: Int -> (t -> [FilePath] -> Action ()) -> String
  -> TypeChecker
  -> (OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> t)
  -> NewRulesFn
rNewRules nArgs applyDeps name tFn aFn cfg lRef iRef = do
  let exprDir = cfgTmpDir cfg </> "exprs"
      pattern = exprDir </> name </> (foldl1 (</>) (take (nArgs+1) $ repeat "*")) </> "result"
  pattern %> \p -> aNewRules applyDeps tFn aFn cfg lRef iRef (ExprPath p)
  return ()

aNewRules
  :: (t -> [FilePath] -> Action ())
  -> TypeChecker
  -> (OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> t)
  ->  OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath
  -> Action ()
aNewRules applyDeps tFn aFn cfg lRef iRef o@(ExprPath out) = do
  (oType, dTypes, deps) <- liftIO $ decodeNewRulesDeps cfg iRef o
  case tFn dTypes of
    Left err -> error err
    Right rType -> do
      when (rType /= oType) $ error $ "typechecking error: " ++ show rType ++ " /= " ++ show oType
      let deps' = map (fromOrthoLangPath cfg) deps
      need' cfg lRef "ortholang.modules.newrulestest.anewrules" deps'
      -- TODO look up out too and assert that its type matches typechecker result
      -- liftIO $ putStrLn $ "aNewRules dTypes: " ++ show dTypes
      -- liftIO $ putStrLn $ "aNewRules typechecker says: " ++ show (tFn dTypes)
      -- liftIO $ putStrLn $ "aNewRules deps: " ++ show deps
      applyDeps (aFn cfg lRef iRef o) deps'

mkNewFn1 :: String -> OrthoLangType -> [OrthoLangType] -> NewAction1 -> OrthoLangFunction
mkNewFn1 = mkNewFn rNewRules1

mkNewFn2 :: String -> OrthoLangType -> [OrthoLangType] -> NewAction2 -> OrthoLangFunction
mkNewFn2 = mkNewFn rNewRules2

mkNewFn3 :: String -> OrthoLangType -> [OrthoLangType] -> NewAction3 -> OrthoLangFunction
mkNewFn3 = mkNewFn rNewRules3

mkNewFn
  :: (String -> TypeChecker -> t -> NewRulesFn)
  -> String -> OrthoLangType -> [OrthoLangType] -> t
  -> OrthoLangFunction
mkNewFn rFn name oType dTypes aFn =
  let tFn = defaultTypeCheck dTypes oType
  in OrthoLangFunction
       { fNames     = [name]
       , fTypeDesc  = mkTypeDesc name dTypes oType
       , fTypeCheck = tFn
       , fFixity    = Prefix, fTags = []
       , fOldRules  = undefined
       , fNewRules  = Just $ rFn name tFn aFn
       }

-----------
-- test1 --
-----------

-- TODO factor the common elements out of this to get it as simple as possible
test1 :: OrthoLangFunction
test1 = mkNewFn2 "newrulestest1" str [str, str] aTest1

-- TODO make these all OrthoLangPaths
aTest1 :: NewAction2
aTest1 cfg lRef _ (ExprPath out) d1 d2 = do
  s1 <- readLit cfg lRef d1
  s2 <- readLit cfg lRef d2
  writeCachedLines cfg lRef out ["result would go here, but for now these were the inputs:", s1, s2]
