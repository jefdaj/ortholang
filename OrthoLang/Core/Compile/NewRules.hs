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
'Function' definition.
-}

module OrthoLang.Core.Compile.NewRules
  ( newCoreRules
  , newFunctionRules
  , mkNewFn1
  , mkNewFn2
  , mkNewFn3
  )
  where

import Debug.Trace

import Control.Monad (when)
import Development.Shake
import Development.Shake.FilePath ((</>))
import OrthoLang.Core.Actions (need')
import OrthoLang.Core.Compile.Basic
import OrthoLang.Core.Paths (fromPath, decodeNewRulesDeps)
import OrthoLang.Core.Types
import Data.Maybe (catMaybes)
import Control.Monad.Reader

newFunctionRules :: RulesR ()
newFunctionRules = do
  (cfg, _, _, _) <- ask
  let fns   = concatMap mFunctions $ cfgModules cfg
      rules = catMaybes $ map fNewRules fns
  sequence_ rules

-- TODO can this replace NewRulesFn everywhere?
-- type NewRulesFn2 = RulesR ()

-- TODO try ActionR first, then come back to this if it still sounds useful
-- type NewRulesFn = Config -> LocksRef -> IDsRef -> Rules ()
-- newFunctionRules2 :: RulesR ()
-- newFunctionRules2 = do
--   (_, cfg, lRef, iRef, dMap) <- ask
--   let fns   = concatMap mFunctions $ cfgModules cfg
--       rules = catMaybes $ map fNewRules fns
--   mapM_ (\r -> r cfg lRef iRef, dMap) rules

newCoreRules :: RulesR ()
newCoreRules = do

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
-- TODO or Paths throughout?
-- TODO can you encode NewAction1, 2, 3... easily?

rNewRules1 :: String -> TypeChecker -> ActionR1 -> RulesR ()
rNewRules1 = rNewRules 1 applyList1

applyList1 :: (FilePath -> ActionR ()) -> [FilePath] -> ActionR ()
applyList1 fn deps = fn (deps !! 0)

rNewRules2 :: String -> TypeChecker -> ActionR2 -> RulesR ()
rNewRules2 = rNewRules 2 applyList2

applyList2 :: (FilePath -> FilePath -> ActionR ()) -> [FilePath] -> ActionR ()
applyList2 fn deps = fn (deps !! 0) (deps !! 1)

rNewRules3 :: String -> TypeChecker -> ActionR3 -> RulesR ()
rNewRules3 = rNewRules 3 applyList3

applyList3 :: (FilePath -> FilePath -> FilePath -> ActionR ()) -> [FilePath] -> ActionR ()
applyList3 fn deps = fn (deps !! 0) (deps !! 1) (deps !! 2)

-- TODO any need to look up prefixOf to get the canonical name?
newPattern :: Config -> String -> Int -> FilePattern
newPattern cfg name nArgs =
  cfgTmpDir cfg </> "exprs" </> name </> (foldl1 (</>) (take (nArgs+1) $ repeat "*")) </> "result"

-- TODO can you add more rules simply by doing >> moreRulesFn after this?
-- TODO one less * if not using repeat salt
rNewRules
  :: Int -> (t -> [FilePath] -> ActionR ()) -> String -> TypeChecker
  -> (ExprPath -> t) -> RulesR ()
rNewRules nArgs applyFn name tFn aFn = do
  (cfg, lRef, iRef, dMap) <- ask
  let ptn = newPattern cfg name nArgs
      ptn' = trace ("rNewRules ptn: '" ++ show ptn ++ "'") ptn
  ptn' %>> aNewRules applyFn tFn aFn

(%>>) :: FilePattern -> (ExprPath -> ActionR ()) -> RulesR ()
ptn %>> act = do
  (cfg, lRef, iRef, dMap) <- ask
  let run = runActionR (cfg, lRef, iRef, dMap)
  lift $ ptn %> \p -> run $ act $ ExprPath p

aNewRules
  :: (t -> [FilePath] -> ActionR ()) -- one of the apply{1,2,3} fns
  -> TypeChecker
  -> (ExprPath -> t)
  ->  ExprPath -> ActionR ()
aNewRules applyFn tFn aFn out = do
  (cfg, lRef, iRef, dMap) <- ask
  (oType, dTypes, deps) <- liftIO $ decodeNewRulesDeps cfg dMap out
  case tFn dTypes of
    Left err -> error err
    Right rType -> do
      when (rType /= oType) $ error $ "typechecking error: " ++ show rType ++ " /= " ++ show oType
      let deps' = map (fromPath cfg) deps

      -- TODO this needs to be either wrapped or used inside ActionR, right?
      needR "ortholang.modules.newrulestest.aNewRules" deps'

      -- TODO look up out too and assert that its type matches typechecker result
      -- liftIO $ putStrLn $ "aNewRules dTypes: " ++ show dTypes
      -- liftIO $ putStrLn $ "aNewRules typechecker says: " ++ show (tFn dTypes)
      -- liftIO $ putStrLn $ "aNewRules deps: " ++ show deps
      applyFn (aFn out) deps'

needR :: String -> [FilePath] -> ActionR ()
needR name deps = do
  (cfg, lRef, _, _) <- ask
  lift $ need' cfg lRef name deps

mkNewFn1 :: String -> Maybe Char -> Type -> [Type] -> ActionR1 -> Function
mkNewFn1 = mkNewFn rNewRules1

mkNewFn2 :: String -> Maybe Char -> Type -> [Type] -> ActionR2 -> Function
mkNewFn2 = mkNewFn rNewRules2

mkNewFn3 :: String -> Maybe Char -> Type -> [Type] -> ActionR3 -> Function
mkNewFn3 = mkNewFn rNewRules3

mkNewFn
  :: (String -> TypeChecker -> t -> RulesR ())
  -> String -> Maybe Char -> Type -> [Type] -> t -> Function
mkNewFn rFn name mChar oType dTypes aFn =
  let tFn = defaultTypeCheck dTypes oType
  in Function
       { fOpChar    = mChar
       , fName      = name
       , fTypeDesc  = mkTypeDesc name dTypes oType
       , fTypeCheck = tFn
       , fTags      = []
       , fOldRules  = undefined
       , fNewRules  = Just $ rFn name tFn aFn
       }
