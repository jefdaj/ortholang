module OrthoLang.Core.Compile.NewRules
  ( newCoreRules
  , newFunctionRules
  , mkNewFn1
  , mkNewFn2
  , mkNewFn3
  )
  where

import Control.Monad (when)
import Development.Shake
import Development.Shake.FilePath ((</>))
import OrthoLang.Core.Actions (need')
import OrthoLang.Core.Compile.Basic
import OrthoLang.Core.Paths (fromOrthoLangPath, decodeNewRulesDeps)
import OrthoLang.Core.Types
import Data.Maybe (catMaybes)


newFunctionRules :: OrthoLangConfig -> Locks -> HashedIDsRef -> Rules ()
newFunctionRules cfg lRef iRef = mconcat $ map (\r -> r cfg lRef iRef) rules
  where
   fns   = concatMap mFunctions $ cfgModules cfg
   rules = catMaybes $ map fNewRules fns

newCoreRules :: NewRulesFn
newCoreRules cfg lRef iRef = do

  -- this is a nice idea in general, but won't work with the special lit compilers
  -- (because they need direct access to the expressions to get their lit values)
  -- newPattern cfg "str" 2 %> \p -> aNewRules applyList1 (defaultTypeCheck [str] str) aLit cfg lRef iRef (ExprPath p)
  -- newPattern cfg "num" 2 %> \p -> aNewRules applyList1 (defaultTypeCheck [str] num) aLit cfg lRef iRef (ExprPath p)

  -- TODO rList{,Lists,Paths}
  -- TODO rAssign?
  -- TODO rRef?
  -- TODO rBop
  -- TODO rLoad{,List,ListLits,ListLinks}
  -- TODO rSimple{,Tmp,Script,ScriptPar,ScriptNoFix,'}
  -- TODO rReplace{,',Each}
  -- TODO rMap{,Tmp,Tmps,SimpleScript}
  -- TODO rCompose1
  -- TODO rFun{1,3}
  -- TODO rRepeatN

  return ()


------------------------------
-- new rules infrastructure --
------------------------------

-- TODO ExprPaths for deps?
-- TODO or OrthoLangPaths throughout?
-- TODO can you encode NewAction1, 2, 3... easily?

rNewRules1 :: String -> TypeChecker -> NewAction1 -> NewRulesFn
rNewRules1 = rNewRules 1 applyList1

applyList1
  :: (FilePath -> Action ())
  -> [FilePath] -> Action ()
applyList1 fn deps = fn (deps !! 0)

rNewRules2 :: String -> TypeChecker -> NewAction2 -> NewRulesFn
rNewRules2 = rNewRules 2 applyList2

applyList2
  :: (FilePath -> FilePath -> Action ())
  -> [FilePath] -> Action ()
applyList2 fn deps = fn (deps !! 0) (deps !! 1)

rNewRules3 :: String -> TypeChecker -> NewAction3 -> NewRulesFn
rNewRules3 = rNewRules 3 applyList3

applyList3
  :: (FilePath -> FilePath -> FilePath -> Action ())
  -> [FilePath] -> Action ()
applyList3 fn deps = fn (deps !! 0) (deps !! 1) (deps !! 2)

newPattern :: OrthoLangConfig -> String -> Int -> FilePattern
newPattern cfg name nArgs =
  cfgTmpDir cfg </> "exprs" </> name </> (foldl1 (</>) (take (nArgs+1) $ repeat "*")) </> "result"

-- TODO can you add more rules simply by doing >> moreRulesFn after this?
rNewRules
  :: Int -> (t -> [FilePath] -> Action ()) -> String
  -> TypeChecker
  -> (OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> t)
  -> NewRulesFn
rNewRules nArgs applyFn name tFn aFn cfg lRef iRef = do
  let exprDir = cfgTmpDir cfg </> "exprs"
      pattern = newPattern cfg name nArgs
      -- pattern = exprDir </> name </> (foldl1 (</>) (take (nArgs+1) $ repeat "*")) </> "result"
  pattern %> \p -> aNewRules applyFn tFn aFn cfg lRef iRef (ExprPath p)
  return ()

aNewRules
  :: (t -> [FilePath] -> Action ()) -- one of the apply{1,2,3} fns
  -> TypeChecker
  -> (OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath -> t)
  ->  OrthoLangConfig -> Locks -> HashedIDsRef -> ExprPath
  -> Action ()
aNewRules applyFn tFn aFn cfg lRef iRef out = do
  (oType, dTypes, deps) <- liftIO $ decodeNewRulesDeps cfg iRef out
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
      applyFn (aFn cfg lRef iRef out) deps'

-- TODO do the applyFn thing at the action level rather than rules?
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
