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
  (

  -- * The new API
    mkNewBop
  , mkNewFn1
  , mkNewFn2
  , mkNewFn3

  -- * Types
  , ActionEnv
  , ActionR
  , ActionR1
  , ActionR2
  , ActionR3
  , runActionR
  , askConfig -- TODO needs to work with multiple Env monads?
  , askLocks

  -- * Static rules to add to every eval call
  , newFunctionRules
  , newCoreRules

  )
  where

import Control.Monad.Reader
import Development.Shake
import OrthoLang.Core.Compile.Basic
import OrthoLang.Core.Types

import Control.Monad              (when)
import Data.Maybe                 (catMaybes)
import Development.Shake.FilePath ((</>))
import OrthoLang.Core.Actions     (need')
import OrthoLang.Core.Paths       (fromPath, decodeNewRulesDeps)
import OrthoLang.Core.Util        (traceShow)

-----------------------------------------------
-- experimental: add state to Rules + Action --
-----------------------------------------------

-- this is needed to pass DigestMap around, and makes the rest easier
type ActionEnv = (Config, LocksRef, IDsRef, DigestMap)
type ActionR a = ReaderT ActionEnv Action a

runActionR :: ActionEnv -> ActionR a -> Action a
runActionR env act = runReaderT act env

askConfig :: ActionR Config
askConfig = do
  (cfg, _, _, _) <- ask
  return cfg

askLocks :: ActionR LocksRef
askLocks = do
  (_, lRef, _, _) <- ask
  return lRef

type ActionR1 = ExprPath -> FilePath                         -> ActionR ()
type ActionR2 = ExprPath -> FilePath -> FilePath             -> ActionR ()
type ActionR3 = ExprPath -> FilePath -> FilePath -> FilePath -> ActionR ()

{-|
The old-style rules in use throughout OrthoLang now require the compilers to
return exact paths. These new ones use proper patterns instead, so they can be
added once per program run rather than once per expression. They should also
allow Shake to infer mapping patterns, but that isn't implemented yet.
-}
newFunctionRules :: RulesR ()
newFunctionRules = do
  (cfg, _, _, _) <- ask
  let fns   = concatMap mFunctions $ cfgModules cfg
      rules = catMaybes $ map fNewRules fns
  sequence_ rules

{-|
I'm not sure yet which of the core language feature compilers can be converted
to new-style rules. Perhaps none of them? If not, remove this.
-}
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
      ptn' = traceShow "rNewrules" ptn
  ptn' %>> aNewRules applyFn tFn aFn

(%>>) :: FilePattern -> (ExprPath -> ActionR ()) -> RulesR ()
ptn %>> act = do
  (cfg, lRef, iRef, dMap) <- ask
  let run = runActionR (cfg, lRef, iRef, dMap)
  lift $ ptn %> \p -> run $ act $ ExprPath p

aNewRules
  :: (t -> [FilePath] -> ActionR ()) -- ^ one of the apply{1,2,3} fns
  -> TypeChecker
  -> (ExprPath -> t)
  ->  ExprPath -> ActionR ()
aNewRules applyFn tFn aFn out = do
  (cfg, lRef, iRef, dMap) <- ask
  (oType, dTypes, deps) <- liftIO $ decodeNewRulesDeps cfg dMap out
  case tFn dTypes of
    Left err -> error err
    Right rType -> do
      when (rType /= oType) $
        error $ "typechecking error: " ++ show rType ++ " /= " ++ show oType
      let deps' = map (fromPath cfg) deps
      needR "ortholang.modules.newrulestest.aNewRules" deps'
      applyFn (aFn out) deps'

needR :: String -> [FilePath] -> ActionR ()
needR name deps = do
  (cfg, lRef, _, _) <- ask
  lift $ need' cfg lRef name deps

mkNewBop :: String   -- ^ name
         -> Char     -- ^ opchar
         -> Type     -- ^ return type
         -> Type     -- ^ 1 argument type (each side of the bop will be this)
         -> ActionR1 -- ^ 1-argument action (list of 2 args in case of bop, or 2+ for the prefix fn)
         -> Function
mkNewBop n c r a1 = mkNewFn rNewRules1
         n (Just c) r [ListOf a1]

mkNewFn1 :: String     -- ^ name
         -> Type       -- ^ return type
         -> Type       -- ^ 1 argument type
         -> ActionR1   -- ^ 1-argument action
         -> Function
mkNewFn1 n r a1 = mkNewFn rNewRules1
         n Nothing r [a1]

mkNewFn2 :: String       -- ^ name
         -> Type         -- ^ return type
         -> (Type, Type) -- ^ 2 argument types
         -> ActionR2     -- ^ 2-argument action
         -> Function
mkNewFn2 n r (a1, a2) = mkNewFn rNewRules2
         n Nothing r [a1, a2]

mkNewFn3 :: String             -- ^ name
         -> Type               -- ^ return type
         -> (Type, Type, Type) -- ^ 3 argument types
         -> ActionR3           -- ^ 3-argument action
         -> Function
mkNewFn3 n r (a1, a2, a3) = mkNewFn rNewRules3
         n Nothing r [a1, a2, a3]

-- | Use the argument-specific numbered version above instead.
mkNewFn
  :: (String -> TypeChecker -> t -> RulesR ())
  -> String -> Maybe Char -> Type -> [Type] -> t -> Function
mkNewFn rFn name mChar oType dTypes aFn =
  let tFn = defaultTypeCheck name dTypes oType
  in Function
       { fOpChar    = mChar
       , fName      = name
       , fTypeDesc  = mkTypeDesc name dTypes oType
       , fTypeCheck = tFn
       , fTags      = []
       , fOldRules  = undefined
       , fNewRules  = Just $ rFn name tFn aFn
       }
