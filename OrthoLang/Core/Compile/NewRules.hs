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

Functions are listed in order of increasing complexity, so you should probably
prefer the first one that handles your problem somewhat elegantly.
-}

module OrthoLang.Core.Compile.NewRules
  (

  -- * Types
  -- $newactions
    NewAction1
  , NewAction2
  , NewAction3

  -- * Static rules to add to every eval call
  , newFunctionRules

  -- * Basic functions
  -- $basic
  , newFn1
  , newFn2
  , newFn3

  -- * Binary operators
  , newBop

  -- * Expr transformers
  -- $transformers

  -- * Extra steps
  -- $extrasteps

  )
  where

import Control.Monad.Reader
import Development.Shake
import OrthoLang.Core.Compile.Basic
import OrthoLang.Core.Types

import Control.Monad              (when)
import Data.Maybe                 (catMaybes, fromJust)
import Development.Shake.FilePath ((</>))
import OrthoLang.Core.Actions     (need')
import OrthoLang.Core.Paths (fromPath, decodeNewRulesDeps)
import OrthoLang.Util        (traceShow)

{-|
The old-style rules in use throughout OrthoLang now require the compilers to
return exact paths. These new ones use proper patterns instead, so they can
be added once per program run rather than once per expression. They should
also allow Shake to infer mapping patterns, but that isn't implemented yet.
This set of "new function" rules should be added to every eval call.
-}
newFunctionRules :: Rules ()
newFunctionRules = do
  cfg <- fmap fromJust $ getShakeExtraRules
  let fns   = concatMap mFunctions $ cfgModules cfg
      rules = catMaybes $ map fNewRules fns
  sequence_ rules

-- $newactions
-- These are the types of typical 'Development.Shake.Action's to be added to
-- 'Function' definitions. They enforce that the 'Development.Shake.Action'
-- expects the same number of input files that the 'Function' expects to pass
-- it.

type NewAction1 = ExprPath -> FilePath                         -> Action ()
type NewAction2 = ExprPath -> FilePath -> FilePath             -> Action ()
type NewAction3 = ExprPath -> FilePath -> FilePath -> FilePath -> Action ()

-- $basic
-- If you just need to turn a standalone script into an OrthoLang function,
-- you probably want one of these!

newFn1 :: String     -- ^ name
       -> Type       -- ^ return type
       -> Type       -- ^ 1 argument type
       -> NewAction1 -- ^ 1-argument action
       -> Function
newFn1 n r a1 = newFn rNewRules1 n Nothing r [a1]

newFn2 :: String       -- ^ name
       -> Type         -- ^ return type
       -> (Type, Type) -- ^ 2 argument types
       -> NewAction2   -- ^ 2-argument action
       -> Function
newFn2 n r (a1, a2) = newFn rNewRules2 n Nothing r [a1, a2]

newFn3 :: String             -- ^ name
       -> Type               -- ^ return type
       -> (Type, Type, Type) -- ^ 3 argument types
       -> NewAction3         -- ^ 3-argument action
       -> Function
newFn3 n r (a1, a2, a3) = newFn rNewRules3 n Nothing r [a1, a2, a3]

{-|
This is for the specific case where you want to make a binary operator.
It probably isn't very useful outside math and set operations.
-}
newBop :: String     -- ^ name
       -> Char       -- ^ opchar
       -> Type       -- ^ return type
       -> Type       -- ^ 1 argument type (each side of the bop will be this)
       -> NewAction1 -- ^ 1-argument action (list of 2 args in case of bop, or 2+ for the prefix fn)
       -> Function
newBop n c r a1 = newFn rNewRules1 n (Just c) r [ListOf a1]

-- TODO ExprPaths for deps?
-- TODO or Paths throughout?
-- TODO can you encode NewNewAction1, 2, 3... easily?

rNewRules1 :: String -> TypeChecker -> NewAction1 -> Rules ()
rNewRules1 = rNewRules 1 applyList1

applyList1 :: (FilePath -> Action ()) -> [FilePath] -> Action ()
applyList1 fn deps = fn (deps !! 0)

rNewRules2 :: String -> TypeChecker -> NewAction2 -> Rules ()
rNewRules2 = rNewRules 2 applyList2

applyList2 :: (FilePath -> FilePath -> Action ()) -> [FilePath] -> Action ()
applyList2 fn deps = fn (deps !! 0) (deps !! 1)

rNewRules3 :: String -> TypeChecker -> NewAction3 -> Rules ()
rNewRules3 = rNewRules 3 applyList3

applyList3 :: (FilePath -> FilePath -> FilePath -> Action ()) -> [FilePath] -> Action ()
applyList3 fn deps = fn (deps !! 0) (deps !! 1) (deps !! 2)

-- TODO any need to look up prefixOf to get the canonical name?
newPattern :: Config -> String -> Int -> FilePattern
newPattern cfg name nArgs =
  cfgTmpDir cfg </> "exprs" </> name </> (foldl1 (</>) (take (nArgs+1) $ repeat "*")) </> "result"

-- TODO can you add more rules simply by doing >> moreRulesFn after this?
-- TODO one less * if not using repeat salt
rNewRules
  :: Int -> (t -> [FilePath] -> Action ()) -> String -> TypeChecker
  -> (ExprPath -> t) -> Rules ()
rNewRules nArgs applyFn name tFn aFn = do
  cfg <- fmap fromJust $ getShakeExtraRules
  let ptn = newPattern cfg name nArgs
      ptn' = traceShow "rNewrules" ptn
  ptn' %> \p -> aNewRules applyFn tFn aFn (ExprPath p)

aNewRules
  :: (t -> [FilePath] -> Action ()) -- ^ one of the apply{1,2,3} fns
  -> TypeChecker
  -> (ExprPath -> t)
  ->  ExprPath -> Action ()
aNewRules applyFn tFn aFn out = do
  cfg  <- fmap fromJust $ getShakeExtra
  dRef <- fmap fromJust $ getShakeExtra
  (oType, dTypes, deps) <- liftIO $ decodeNewRulesDeps cfg dRef out
  case tFn dTypes of
    Left err -> fail err -- TODO bop type error here :(
    Right rType -> do
      when (rType /= oType) $
        error $ "typechecking error: " ++ show rType ++ " /= " ++ show oType
      let deps' = map (fromPath cfg) deps
      need' "ortholang.modules.newrulestest.aNewRules" deps'
      applyFn (aFn out) deps'

-- | Use the argument-specific numbered version above instead.
newFn
  :: (String -> TypeChecker -> t -> Rules ())
  -> String -> Maybe Char -> Type -> [Type] -> t -> Function
newFn rFn name mChar oType dTypes aFn =
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

-- $transformers
-- Some of the current OrthoLang function compilers are implemented partly by
-- transforming their input Exprs to soemthing else, then compiling them using
-- standard and/or custom code. This is an attempt to standardize that.
--
-- If given, these will be used to transform the input expression before
-- passing it to the Rules. They could be implemented as a new fTransform
-- Function field, or as in fOldRules by passing the script + expression
-- directly. But to be honest that got very confusing!
--
-- It might actually be worth making fNewRules optional too, because some
-- functions could be implemented entirely with transforms + re-compiling the
-- new expression normally through rExpr.

-- $extrasteps
-- Some of the current OrthoLang RulesFns only require one script/step to run,
-- and they're simple. But others need to build tmpfiles first, and this is an
-- attempt to standardize that. The basic pattern is that each step is a Shake
-- pattern + associated Action. They can be put together in one RulesFn, or
-- probably separated.
--
-- Use cases:
--
-- * blast databases
-- * busco lineages
-- * curl output (tarball?)
-- * loaders?
-- * crb-blast tmpdirs?
-- * bin caches?
-- * concat caches?
-- * mmseqs caches?
--
-- Could they be implemented simply with hidden fns + transformers? That might
-- be much nicer than the current way.
