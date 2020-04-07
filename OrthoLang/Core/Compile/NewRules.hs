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

TODO can API-facing Rules be entirely eliminated? Maybe just NewActions + Macros is enough.
-}

-- TODO versions of the newFn* functions that take custom typecheckers and lists of args?
--      do it because it lets you keep the same custom typechecker behavior as the current fns

module OrthoLang.Core.Compile.NewRules
  (

  -- * Functions from external scripts
  -- $fromscripts
    newFnS1
  , newFnS2
  , newFnS3
  , newFnST1
  , newFnST2
  , newFnST3

  -- * Functions from Actions
  -- $fromactions
  , NewAction1
  , NewAction2
  , NewAction3
  , newFnA1
  , newFnA2
  , newFnA3
  , newFnAT1
  , newFnAT2
  , newFnAT3

  -- * Binary operators from Actions
  , newBop

  -- * Functions from Macros
  -- $frommacros
  , MacroExpansion
  , newMacro
  , hidden

  -- * Implementation details
  , newRules
  , newPattern
  , newFn
  , rNewRulesA1
  , rNewRulesA2
  , rNewRulesA3
  , rNewRules
  , applyList1
  , applyList2
  , applyList3
  , aNewRulesS1
  , aNewRulesS2
  , aNewRulesS3
  , aNewRulesS
  , aNewRules

  )
  where

import Prelude hiding (error)
import OrthoLang.Debug
import Control.Monad.Reader
import Development.Shake
import OrthoLang.Core.Actions       (runCmd, CmdDesc(..))
import OrthoLang.Core.Compile.Basic
import OrthoLang.Core.Types
import System.Exit (ExitCode(..))

import Control.Monad              (when)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((</>), takeDirectory)
import OrthoLang.Core.Actions     (need')
import OrthoLang.Core.Paths (toPath, fromPath, decodeNewRulesDeps, addDigest)

---------
-- API --
---------

-- $fromscripts
-- This is the best way to create an OrthoLang function if you want to write minimal Haskell.
-- Write everything in an external script, add it to your PATH, and wrap it using one of these functions.
-- Note that to fully integrate it with OrthoLang you'll also need to package the dependencies with Nix.

-- TODO you can get the return types here too...
newFnS1
  :: String               -- ^ name
  -> Type                 -- ^ 1 argument type
  -> Type                 -- ^ return type
  -> String               -- ^ script basename
  -> (CmdDesc -> CmdDesc) -- ^ extra options
  -> Function
newFnS1 n a1 r s os = newFn n Nothing [a1] r rNewRulesA1 $ aNewRulesS1 s os

-- TODO but here? maybe we need to completely remove custom typecheckers
newFnST1
  :: String      -- ^ name
  -> TypeChecker -- ^ type checker (should take 1 argument)
  -> String      -- ^ type description
  -> String      -- ^ script basename
  -> (CmdDesc -> CmdDesc) -- ^ extra options
  -> Function
newFnST1 n tFn td s os = newFnT n Nothing tFn td rNewRulesA1 $ aNewRulesS1 s os

-- TODO failed to lookup out path in here?
aNewRulesS1 :: String -> (CmdDesc -> CmdDesc) -> NewAction1
aNewRulesS1 sname opts o a1 = aNewRulesS sname opts o [a1]

newFnS2
  :: String               -- ^ name
  -> (Type, Type)         -- ^ 2 argument types
  -> Type                 -- ^ return type
  -> String               -- ^ script basename
  -> (CmdDesc -> CmdDesc) -- ^ extra options
  -> Function
newFnS2 n (a1, a2) r s os = newFn n Nothing [a1, a2] r rNewRulesA2 $ aNewRulesS2 s os

newFnST2
  :: String      -- ^ name
  -> TypeChecker -- ^ type checker (should take 2 arguments)
  -> String      -- ^ type description
  -> String      -- ^ script basename
  -> (CmdDesc -> CmdDesc) -- ^ extra options
  -> Function
newFnST2 n tFn td s os = newFnT n Nothing tFn td rNewRulesA2 $ aNewRulesS2 s os

aNewRulesS2 :: String -> (CmdDesc -> CmdDesc) -> NewAction2
aNewRulesS2 sname opts o a1 a2 = aNewRulesS sname opts o [a1, a2]

newFnS3
  :: String               -- ^ name
  -> (Type, Type, Type)   -- ^ 3 argument types
  -> Type                 -- ^ return type
  -> String               -- ^ script basename
  -> (CmdDesc -> CmdDesc) -- ^ extra options
  -> Function
newFnS3 n (a1, a2, a3) r s os = newFn n Nothing [a1, a2, a3] r rNewRulesA3 $ aNewRulesS3 s os

newFnST3
  :: String      -- ^ name
  -> TypeChecker -- ^ type checker (should take 3 arguments)
  -> String      -- ^ type description
  -> String      -- ^ script basename
  -> (CmdDesc -> CmdDesc) -- ^ extra options
  -> Function
newFnST3 n tFn td s os = newFnT n Nothing tFn td rNewRulesA3 $ aNewRulesS3 s os

aNewRulesS3 :: String -> (CmdDesc -> CmdDesc) -> NewAction3
aNewRulesS3 sname opts o a1 a2 a3 = aNewRulesS sname opts o [a1, a2, a3]

{-|
Approximate rewrite of 'OrthoLang.Core.Compile.Simple.aSimpleScript'.
Use the safer versions above if possible; this one does not check number of arguments.
Use cmdOpts to update the 'CmdDesc' with any non-default options needed.

Note that you can also use one or more of these inside a larger NewAction1,2,3.

TODO can this add the outpath digest, or does that need handling individually?
-}
aNewRulesS :: String -> (CmdDesc -> CmdDesc) -> ExprPath -> [FilePath] -> Action ()
aNewRulesS sname opts (ExprPath out) args = do
  let eDir = takeDirectory out -- TODO gotta be a more elegant way right?
  runCmd $ opts $ CmdDesc
    { cmdBinary        = sname
    , cmdArguments     = out:args -- TODO pre- and post-processing?
    , cmdFixEmpties    = True     -- TODO any cases where you wouldn't want to?
    , cmdParallel      = False    -- TODO how to handle this?
    , cmdInPatterns    = args     -- TODO remove and always use the whole folder?
    , cmdOutPath       = out      -- TODO remove and always use the whole folder?
    , cmdExtraOutPaths = []       -- TODO should this just refer to cache paths/dirs?
    , cmdSanitizePaths = []
    , cmdOptions       = [Cwd eDir] -- TODO remove? but then how would makeblastdb work?
    , cmdExitCode      = ExitSuccess
    , cmdRmPatterns    = [eDir </> "*"] -- TODO remove and always use the whole folder?
    }

-- $fromactions
-- Use these functions when writing a "script" that would be easier in Haskell, or benefit from Shake integration.
-- The NewAction{1,2,3} types enforce that the 'Development.Shake.Action'
-- expects the same number of input files that the 'Function' will pass it.

type NewAction1 = ExprPath -> FilePath                         -> Action ()
type NewAction2 = ExprPath -> FilePath -> FilePath             -> Action ()
type NewAction3 = ExprPath -> FilePath -> FilePath -> FilePath -> Action ()

-- TODO all these could pass the return type, but not the script ones right? :/
newFnA1
  :: String     -- ^ name
  -> Type       -- ^ 1 argument type
  -> Type       -- ^ return type
  -> NewAction1 -- ^ 1-argument action function
  -> Function
newFnA1 n a1 r = newFn n Nothing [a1] r rNewRulesA1

newFnAT1
  :: String      -- ^ name
  -> TypeChecker -- ^ type checker (should take 1 argument)
  -> String      -- ^ type description
  -> NewAction1  -- ^ 1-argument action function
  -> Function
newFnAT1 n tFn td aFn = newFnT n Nothing tFn td rNewRulesA1 aFn

newFnA2
  :: String       -- ^ name
  -> (Type, Type) -- ^ 2 argument types
  -> Type         -- ^ return type
  -> NewAction2   -- ^ 2-argument action function
  -> Function
newFnA2 n (a1, a2) r = newFn n Nothing [a1, a2] r rNewRulesA2

newFnAT2
  :: String      -- ^ name
  -> TypeChecker -- ^ type checker (should take 2 arguments)
  -> String      -- ^ type description
  -> NewAction2  -- ^ 2-argument action function
  -> Function
newFnAT2 n tFn td aFn = newFnT n Nothing tFn td rNewRulesA2 aFn

newFnA3
  :: String             -- ^ name
  -> (Type, Type, Type) -- ^ 3 argument types
  -> Type               -- ^ return type
  -> NewAction3         -- ^ 3-argument action function
  -> Function
newFnA3 n (a1, a2, a3) r = newFn n Nothing [a1, a2, a3] r rNewRulesA3

newFnAT3
  :: String      -- ^ name
  -> TypeChecker -- ^ type checker (should take 3 arguments)
  -> String      -- ^ type description
  -> NewAction3  -- ^ 3-argument action function
  -> Function
newFnAT3 n tFn td aFn = newFnT n Nothing tFn td rNewRulesA3 aFn

{-|
This is for the specific case where you want to make a binary operator.
It probably isn't very useful outside math and set operations.
-}
newBop
  :: String     -- ^ name
  -> Char       -- ^ opchar
  -> Type       -- ^ 1 argument type (each side of the bop will be this)
  -> Type       -- ^ return type
  -> NewAction1 -- ^ 1-argument action function (list of 2 args in case of bop, or 2+ for the prefix fn)
  -> Function
newBop n c a1 r = newFn n (Just c) [ListOf a1] r rNewRulesA1

--------------------
-- implementation --
--------------------

{-|
This gathers all the 'fNewRules' 'Development.Shake.Rules' together for use in
'OrthoLang.Core.Eval.eval'. The old-style rules in use throughout OrthoLang now
require the compilers to return exact paths. These new ones use proper patterns
instead, so they can be added once per program run rather than once per
expression. They should also allow Shake to infer mapping patterns, but that
isn't implemented yet.
-}
newRules :: Rules ()
newRules = do
  cfg <- fmap fromJust $ getShakeExtraRules
  let fns   = concatMap mFunctions $ cfgModules cfg
      rules = catRules $ map fNewRules fns
  sequence_ rules
  where
    catRules [] = []
    catRules ((NewRules r):xs) = r : catRules xs
    catRules (_:xs) = catRules xs

-- TODO any need to look up prefixOf to get the canonical name?
{-|
-}
newPattern :: Config -> String -> Int -> FilePattern
newPattern cfg name nArgs =
  cfgTmpDir cfg </> "exprs" </> name </> (foldl1 (</>) (take (nArgs+1) $ repeat "*")) </> "result"

-- TODO need to addDigest in here somehow?
-- TODO can you add more rules simply by doing >> moreRulesFn after this?
-- TODO one less * if not using repeat salt
{-|
-}
rNewRules
  :: Int -> (t -> [FilePath] -> Action ())
  -> TypeChecker
  -> String
  -> (ExprPath -> t)
  -> Rules ()
rNewRules nArgs applyFn tFn name aFn = do
  cfg <- fmap fromJust $ getShakeExtraRules
  let ptn = newPattern cfg name nArgs
      ptn' = traceShow "rNewrules" ptn
  ptn' %> \p -> do
    -- TODO if adding rules works anywhere in an action it'll be here right?
    aNewRules applyFn tFn aFn (ExprPath p)

-- TODO is it possible to get the return type here?
rNewRulesA1 :: TypeChecker -> String -> NewAction1 -> Rules ()
rNewRulesA1 = rNewRules 1 applyList1

applyList1 :: (FilePath -> Action ()) -> [FilePath] -> Action ()
applyList1 fn deps = fn (deps !! 0)

rNewRulesA2 :: TypeChecker -> String -> NewAction2 -> Rules ()
rNewRulesA2 = rNewRules 2 applyList2

applyList2 :: (FilePath -> FilePath -> Action ()) -> [FilePath] -> Action ()
applyList2 fn deps = fn (deps !! 0) (deps !! 1)

rNewRulesA3 :: TypeChecker -> String -> NewAction3 -> Rules ()
rNewRulesA3 = rNewRules 3 applyList3

applyList3 :: (FilePath -> FilePath -> FilePath -> Action ()) -> [FilePath] -> Action ()
applyList3 fn deps = fn (deps !! 0) (deps !! 1) (deps !! 2)

{-|
-}
aNewRules
  :: (t -> [FilePath] -> Action ()) -- ^ one of the apply{1,2,3} fns
  -> TypeChecker
  -> (ExprPath -> t)
  ->  ExprPath -> Action ()
aNewRules applyFn tFn aFn o@(ExprPath out) = do
  cfg  <- fmap fromJust $ getShakeExtra
  dRef <- fmap fromJust $ getShakeExtra
  -- TODO don't try to return oType here because outpath won't have a digest entry yet
  (oType, dTypes, dPaths) <- liftIO $ decodeNewRulesDeps cfg dRef o
  case tFn dTypes of
    Left err -> fail err -- TODO bop type error here :(
    Right rType -> do
      when (rType /= oType) $
        error "aNewRules" $ "typechecking error: " ++ show rType ++ " /= " ++ show oType

      -- TODO shit, does this need to be known at rules time before running anything?
      --      if so that would result in a more elegant + typesafe system overall i guess
      --      start by separating + committing the addDigest changes, because those were good
      liftIO $ addDigest dRef rType $ toPath cfg out

      let dPaths' = map (fromPath cfg) dPaths
      need' "ortholang.modules.newrulestest.aNewRules" dPaths'
      applyFn (aFn o) dPaths'

{-|
Use the more specific, polished, versions above instead if possible. This one
is not type safe because it assumes the list of argument types will match the
rules function + action function.
-}
newFn
  :: String     -- ^ name
  -> Maybe Char -- ^ opchar
  -> [Type]     -- ^ list of argument types
  -> Type       -- ^ return type
  -> (TypeChecker -> String -> t -> Rules ()) -- ^ rules function
  -> t                                        -- ^ matching action function
  -> Function
newFn name mChar dTypes oType rFn aFn =
  let tFn = defaultTypeCheck name dTypes oType
  in Function
       { fOpChar    = mChar
       , fName      = name
       , fTypeDesc  = mkTypeDesc name dTypes oType
       , fTypeCheck = tFn
       , fTags      = []
       , fOldRules  = undefined
       , fNewRules  = NewRules $ rFn tFn name aFn
       }

{-|
Like 'newFn', but you include a custom 'TypeChecker' and a description of it.
For functions that require more flexibility than the regular typechecking
provides. See if you can use a 'TypeGroup' instead!
-}
newFnT
  :: String      -- ^ name
  -> Maybe Char  -- ^ opchar
  -> TypeChecker -- ^ type checker
  -> String      -- ^ type description
  -> (TypeChecker -> String -> t -> Rules ()) -- ^ rules function
  -> t                                        -- ^ matching action function
  -> Function
newFnT name mChar tFn td rFn aFn =
  Function
    { fOpChar    = mChar
    , fName      = name
    , fTypeDesc  = td
    , fTypeCheck = tFn
    , fTags      = []
    , fOldRules  = undefined
    , fNewRules  = NewRules $ rFn tFn name aFn
    }

-- TODO move macros to a separate file?

-- $frommacros
-- Some of the current 'Function' compilers are implemented partly by
-- transforming their input 'Expr's to something else, then compiling them
-- under other function names. This is an attempt to standardize that.
--
-- These can be given instead of 'fNewRules'. They will be used to expand the
-- input expression, and then it will be passed back to 'rExpr' for
-- compilation. The expanded functions may be standard user-facing functions,
-- or 'hidden' ones if this is their only use case.
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
-- * mmseqs databases
-- * psiblast PSSMs
-- * all-vs-all searches
-- * ortholog searches starting from any kind of blast
--
-- Macros get read access to the entire 'Script' up to where they were called,
-- but can only return one altered 'Expr'.

type MacroExpansion = Script -> Expr -> Expr

{-|
Macros are straighforward to implement: use any 'MacroExpansion' to create one
directly.  The only type checking planned so far is that
'OrthoLang.Core.Compile.Basic.rMacro' will prevent macro expansions from
changing the return type of their input 'Expr'.
-}
newMacro :: String -> [Type] -> Type -> MacroExpansion -> Function
newMacro name dTypes oType mFn =
  let tFn = defaultTypeCheck name dTypes oType
  in Function
       { fOpChar    = Nothing
       , fName      = name
       , fTypeDesc  = mkTypeDesc name dTypes oType
       , fTypeCheck = tFn
       , fTags      = []
       , fOldRules  = undefined
       , fNewRules  = NewMacro mFn
       }

{-|
Some functions are only meant to be generated by macro expansion (above)
rather than called by the user. This hides them to prevent confusion.
They're implemented the same as regular functions, but don't have to have
docs and don't show up in the REPL unless you turn on 'cfgDevMode'.
No need to call this if you've manually added 'Hidden' to 'fTags'.
-}
hidden :: Function -> Function
hidden fn = fn { fTags = Hidden : fTags fn }
