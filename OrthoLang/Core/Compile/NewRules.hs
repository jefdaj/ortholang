{-|
A work in progress. If this goes well, it will replace
'OrthoLang.Core.Compile.Basic.rNamedFunction' for all function calls.

The big change is that it decodes dependencies from expression paths rather
than explicitly making a Shake pattern for each file. That lets Shake discover
them as it's supposed to, and should prevent the lingering file lock issues
that stem from mapped function variants \"surprising\" Shake with what they
build.

It should also reduce boilerplace in the modules: in most cases only one
'NewActionN' function will be needed (where N = 1, 2, 3, ...) per
'OrthoLangFunction' definition.
-}

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

-- TODO any need to look up prefixOf to get the canonical name?
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
  newPattern cfg name nArgs %> \p -> aNewRules applyFn tFn aFn cfg lRef iRef (ExprPath p)
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
mkNewFn1 :: String -> Maybe Char -> OrthoLangType -> [OrthoLangType] -> NewAction1 -> OrthoLangFunction
mkNewFn1 = mkNewFn rNewRules1

mkNewFn2 :: String -> Maybe Char -> OrthoLangType -> [OrthoLangType] -> NewAction2 -> OrthoLangFunction
mkNewFn2 = mkNewFn rNewRules2

mkNewFn3 :: String -> Maybe Char -> OrthoLangType -> [OrthoLangType] -> NewAction3 -> OrthoLangFunction
mkNewFn3 = mkNewFn rNewRules3

mkNewFn
  :: (String -> TypeChecker -> t -> NewRulesFn)
  -> String -> Maybe Char -> OrthoLangType -> [OrthoLangType] -> t
  -> OrthoLangFunction
mkNewFn rFn name mChar oType dTypes aFn =
  let tFn = defaultTypeCheck dTypes oType
  in OrthoLangFunction
       { fOpChar    = mChar
       , fName      = name
       , fTypeDesc  = mkTypeDesc name dTypes oType
       , fTypeCheck = tFn
       , fTags      = []
       , fOldRules  = undefined
       , fNewRules  = Just $ rFn name tFn aFn
       }
