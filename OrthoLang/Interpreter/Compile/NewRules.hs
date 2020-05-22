{-# LANGUAGE ScopedTypeVariables #-}

{-|
A work in progress. If this goes well, it will replace
'OrthoLang.Interpreter.Compile.Basic.rNamedFunction' for all function calls.

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

module OrthoLang.Interpreter.Compile.NewRules
  (

  -- * Functions from external scripts
  -- $fromscripts
    newFnS1
  , newFnS2
  , newFnS3

  -- * Functions from Actions
  -- $fromactions
  , NewAction1
  , NewAction2
  , NewAction3
  , newFnA1
  , newFnA2
  , newFnA3

  -- * Binary operators from Actions
  , newBop

  -- * Functions from Macros
  -- $frommacros
  , MacroExpansion
  , newMacro
  , hidden

  -- * Action transformations
  -- $actionmacros
  -- , newPathChange
  , PathExpansion
  , newPathsEach1
  , newPathsEach2
  , newPathsEach3
  , newPathsAll1
  , newPathsAll2
  , newPathsAll3

  -- * Implementation details
  , rReloadIDs
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
  , aLoadIDs -- TODO where should this go? Actions?
  , newPathExpansion

  )
  where

import Prelude hiding (error)
import OrthoLang.Debug
import Control.Monad.Reader
import Development.Shake hiding (doesDirectoryExist)
import System.Directory (doesDirectoryExist)
import OrthoLang.Interpreter.Actions       (runCmd, CmdDesc(..))
import OrthoLang.Interpreter.Sanitize (readIDs)
import OrthoLang.Types
import System.Exit (ExitCode(..))

import Control.Monad              (when)
import Data.Either.Utils          (fromRight)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((</>), takeDirectory, dropExtension, takeBaseName)
import OrthoLang.Interpreter.Actions     (need')
import OrthoLang.Interpreter.Paths (toPath, fromPath, decodeNewRulesDeps, addDigest)
import qualified Data.Map.Strict as M
import Data.IORef                 (atomicModifyIORef')

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
  -> TypeSig              -- ^ 1 argument type
  -> TypeSig              -- ^ return type
  -> String               -- ^ script basename
  -> [FnTag]              -- ^ tags
  -> (CmdDesc -> CmdDesc) -- ^ extra options
  -> Function
newFnS1 n i1 r s ts os = newFn n Nothing [i1] r rNewRulesA1 (aNewRulesS1 s os) ts

-- TODO failed to lookup out path in here?
aNewRulesS1 :: String -> (CmdDesc -> CmdDesc) -> NewAction1
aNewRulesS1 sname opts o i1 = aNewRulesS sname opts o [i1]

newFnS2
  :: String               -- ^ name
  -> (TypeSig, TypeSig)   -- ^ 2 argument types
  -> TypeSig              -- ^ return type
  -> String               -- ^ script basename
  -> [FnTag]              -- ^ tags
  -> (CmdDesc -> CmdDesc) -- ^ extra options
  -> Function
newFnS2 n (i1, i2) r s ts os = newFn n Nothing [i1, i2] r rNewRulesA2 (aNewRulesS2 s os) ts

aNewRulesS2 :: String -> (CmdDesc -> CmdDesc) -> NewAction2
aNewRulesS2 sname opts o i1 i2 = aNewRulesS sname opts o [i1, i2]

newFnS3
  :: String                      -- ^ name
  -> (TypeSig, TypeSig, TypeSig) -- ^ 3 argument types
  -> TypeSig                     -- ^ return type
  -> String                      -- ^ script basename
  -> [FnTag]                     -- ^ tags
  -> (CmdDesc -> CmdDesc)        -- ^ extra options
  -> Function
newFnS3 n (i1, i2, i3) r s ts os = newFn n Nothing [i1, i2, i3] r rNewRulesA3 (aNewRulesS3 s os) ts

aNewRulesS3 :: String -> (CmdDesc -> CmdDesc) -> NewAction3
aNewRulesS3 sname opts o i1 i2 i3 = aNewRulesS sname opts o [i1, i2, i3]

{-|
Approximate rewrite of 'OrthoLang.Interpreter.Compile.Simple.aSimpleScript'.
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
  -> TypeSig    -- ^ 1 input type
  -> TypeSig    -- ^ return type
  -> NewAction1 -- ^ 1-input action function
  -> [FnTag]    -- ^ tags
  -> Function
newFnA1 n i1 r = newFn n Nothing [i1] r rNewRulesA1

newFnA2
  :: String             -- ^ name
  -> (TypeSig, TypeSig) -- ^ 2 input types
  -> TypeSig            -- ^ return type
  -> NewAction2         -- ^ 2-input action function
  -> [FnTag]            -- ^ tags
  -> Function
newFnA2 n (i1, i2) r = newFn n Nothing [i1, i2] r rNewRulesA2

newFnA3
  :: String                      -- ^ name
  -> (TypeSig, TypeSig, TypeSig) -- ^ 3 input types
  -> TypeSig                     -- ^ return type
  -> NewAction3                  -- ^ 3-input action function
  -> [FnTag]                     -- ^ tags
  -> Function
newFnA3 n (i1, i2, i3) r = newFn n Nothing [i1, i2, i3] r rNewRulesA3

{-|
This is for the specific case where you want to make a binary operator.
It probably isn't very useful outside math and set operations.
-}
newBop
  :: String     -- ^ name
  -> Char       -- ^ opchar
  -> TypeSig    -- ^ 1 input type (each side of the bop will be this)
  -> TypeSig    -- ^ return type
  -> NewAction1 -- ^ 1-input action function (list of 2 args in case of bop, or 2+ for the prefix fn)
  -> [FnTag]    -- ^ tags
  -> Function
newBop n c i r = newFn n (Just c) [ListSigs i] r rNewRulesA1

--------------------
-- implementation --
--------------------

{-|
The load_* functions handle hashing seqids of newly-loaded files the first
time, but they fail to enfore re-loading them during the next program run when
they might still be needed. This is a bit of a hack, but does enforce it
properly (so far!)
-}
rReloadIDs :: Rules ()
rReloadIDs = "reloadids" ~> do
  alwaysRerun
  cfg <- fmap fromJust getShakeExtra
  -- find ids in shared load cache, if any
  case shared cfg of
    Nothing -> return ()
    Just sd -> aReloadIDsDir sd
  -- find ids in regular load cache
  let ld = tmpdir cfg </> "cache" </> "load"
  aReloadIDsDir ld

{-|
Note that getDirectoryFilesIO and the non-Shake
'doesDirectoryExist' are used on purpose, because it prevents
'Development.Shake.need'ing the matches (I think).
-}
aReloadIDsDir :: FilePath -> Action ()
aReloadIDsDir loadCacheDir = do
  exists <- liftIO $ doesDirectoryExist loadCacheDir
  idPaths <- (fmap . map) (loadCacheDir </>) $
    if not exists
      then return []
      else liftIO $ getDirectoryFilesIO loadCacheDir ["//*.ids"]
  liftIO $ debug "interpreter.sanitize.aReloadIDsDir" $ "idPaths: " ++ show idPaths
  mapM_ aLoadIDs idPaths

{-|
This is called from 'OrthoLang.Interpreter.Sanitize.hashIDsFile' the first time a
sequence file is loaded, and from 'rReloadIDs' during subsequent program runs.
-}
aLoadIDs :: FilePath -> Action ()
aLoadIDs idsPath' = do
  alwaysRerun
  liftIO $ debug "interpreter.sanitize.aLoadIDs" $ "idsPath': " ++ idsPath'
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadIDs"
      idsPath = toPath loc cfg idsPath'
  newIDs <- readIDs idsPath
  -- TODO what should the keys actually be? probably not the idsPath
  let (Path p) = idsPath
      k = dropExtension p
      v = takeBaseName idsPath' -- TODO is this good enough? maybe try to find the original filename
  -- liftIO $ debug "interpreter.sanitize.aLoadIDs" $ "k: " ++ k
  -- liftIO $ debug "interpreter.sanitize.aLoadIDs" $ "v: " ++ v
  ids <- fmap fromJust getShakeExtra
  liftIO $ atomicModifyIORef' ids $ \h@(IDs {hFiles = fs, hSeqIDs = is}) ->
    (h { hFiles  = M.insert k v fs
       , hSeqIDs = M.insert k newIDs    is
       -- , hSeqHashes = M.insert k newHashes hs
       }, ())

{-|
This gathers all the 'fNewRules' 'Development.Shake.Rules' together for use in
'OrthoLang.Interpreter.Eval.eval'. The old-style rules in use throughout OrthoLang now
require the compilers to return exact paths. These new ones use proper patterns
instead, so they can be added once per program run rather than once per
expression. They should also allow Shake to infer mapping patterns, but that
isn't implemented yet.

TODO get modules via getShakeExtraRules here?
-}
newRules :: [Module] -> Rules ()
newRules mods = sequence_ $ rReloadIDs : fnRules
  where
    fns     = concatMap mFunctions mods
    fnRules = catRules $ map fNewRules fns
    catRules [] = []
    catRules ((NewRules r):xs) = r : catRules xs
    catRules (_:xs) = catRules xs

{-|
TODO any need to look up prefixOf to get the canonical name?
-}
newPattern :: Config -> Bool -> String -> Int -> FilePattern
newPattern cfg useSeed name nArgs =
  tmpdir cfg </> "exprs" </> name </> (foldl1 (</>) (take nStars $ repeat "*")) </> "result"
  where
    nStars = if useSeed then nArgs+1 else nArgs

-- TODO need to addDigest in here somehow?
-- TODO can you add more rules simply by doing >> moreRulesFn after this?
-- TODO one less * if not using repeat seed
-- TODO when a function (only macros?) has the ReadsCode tag, treat that as an input to hash
{-|
-}
rNewRules
  :: Int
  -> (t -> [FilePath] -> Action ())
  -> TypeSig
  -> String
  -> (ExprPath -> t)
  -> Rules ()
rNewRules nArgs applyFn oSig name aFn = do
  cfg  <- fmap fromJust $ getShakeExtraRules
  mods <- fmap fromJust $ getShakeExtraRules
  let useSeed = elem Nondeterministic $ fTags $ fromRight $ findFun mods name
      ptn = newPattern cfg useSeed name nArgs
      ptn' = traceShow "rNewrules" ptn
  ptn' %> \p -> do
    -- TODO if adding rules works anywhere in an action it'll be here right?
    aNewRules applyFn oSig aFn (ExprPath p)

-- TODO is it possible to get the return type here?
rNewRulesA1 :: TypeSig -> String -> NewAction1 -> Rules ()
rNewRulesA1 = rNewRules 1 applyList1

applyList1 :: (FilePath -> Action ()) -> [FilePath] -> Action ()
applyList1 fn deps = fn (deps !! 0)

rNewRulesA2 :: TypeSig -> String -> NewAction2 -> Rules ()
rNewRulesA2 = rNewRules 2 applyList2

applyList2 :: (FilePath -> FilePath -> Action ()) -> [FilePath] -> Action ()
applyList2 fn deps = fn (deps !! 0) (deps !! 1)

rNewRulesA3 :: TypeSig -> String -> NewAction3 -> Rules ()
rNewRulesA3 = rNewRules 3 applyList3

applyList3 :: (FilePath -> FilePath -> FilePath -> Action ()) -> [FilePath] -> Action ()
applyList3 fn deps = fn (deps !! 0) (deps !! 1) (deps !! 2)

{-|
-}
aNewRules
  :: (t -> [FilePath] -> Action ()) -- ^ one of the apply{1,2,3} fns
  -> TypeSig
  -> (ExprPath -> t)
  ->  ExprPath -> Action ()
aNewRules applyFn oSig aFn o@(ExprPath out) = do
  cfg  <- fmap fromJust $ getShakeExtra
  dRef <- fmap fromJust $ getShakeExtra
  let loc = "modules.newrulestest.aNewRules"
  -- TODO don't try to return oType here because outpath won't have a digest entry yet
  (oType, _, dPaths) <- liftIO $ decodeNewRulesDeps cfg dRef o

  -- TODO produce a better error message here
  when (not $ oSig `typeSigMatches` oType) $
    error "aNewRules" $ "typechecking error: " ++ show oSig ++ " /= " ++ show oType

  -- dRef <- fmap fromJust $ getShakeExtra
  liftIO $ addDigest dRef oType $ toPath loc cfg out

  let dPaths' = map (fromPath loc cfg) dPaths
  need' loc dPaths'
  applyFn (aFn o) dPaths'

{-|
Use the more specific, polished, versions above instead if possible. This one
is not type safe because it assumes the list of input types will match the
rules function + action function.
-}
newFn
  :: String                               -- ^ name
  -> Maybe Char                           -- ^ opchar
  -> [TypeSig]                            -- ^ list of input types
  ->  TypeSig                             -- ^ return type
  -> (TypeSig -> String -> t -> Rules ()) -- ^ rules function
  -> t                                    -- ^ matching action function
  -> [FnTag]                              -- ^ tags
  -> Function
newFn name mChar iSigs oSig rFn aFn tags = Function
  { fOpChar    = mChar
  , fName      = name
  , fInputs    = iSigs
  , fOutput    = oSig
  , fTags      = tags
  , fOldRules  = undefined -- TODO why is this being called during blast?
  , fNewRules  = NewRules $ rFn oSig name aFn
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

-- TODO can a Haskell closure be "hidden" in here to implement graph_script? try it!
type MacroExpansion = Script -> Expr -> Expr

{-|
Macros are straighforward to implement: use any 'MacroExpansion' to create one
directly.  The only type checking planned so far is that
'OrthoLang.Interpreter.Compile.Basic.rMacro' will prevent macro expansions from
changing the return type of their input 'Expr'.
-}
newMacro :: String -> [TypeSig] -> TypeSig -> MacroExpansion -> [FnTag] -> Function
newMacro name iSigs oSig mFn tags = Function
  { fOpChar    = Nothing
  , fName      = name
  , fInputs    = iSigs
  , fOutput    = oSig
  , fTags      = tags
  , fOldRules  = undefined
  , fNewRules  = NewMacro mFn
  }

{-|
Some functions are only meant to be generated by macro expansion (above)
rather than called by the user. This hides them to prevent confusion.
They're implemented the same as regular functions, but don't have to have
docs and don't show up in the REPL unless you turn on 'showhidden'.
No need to call this if you've manually added 'Hidden' to 'fTags'.
-}
hidden :: Function -> Function
hidden fn = fn { fTags = Hidden : fTags fn }

-- $actionmacros
-- This is mostly geared toward writing the _each and _all mapped function
-- variants easily. It's analagous to macro expansion, but the transformation
-- is done on the input Paths directly rather than on the Exprs that were
-- compiled to generate those Paths.
--
-- The same priciple applies as when writing Expr macros: you have to make sure
-- that the system knows how to handle the results already. For example if you
-- make a macro that rewrites "exprs/load_faa_each" paths to "exprs/load_faa"
-- paths, there must already be Rules for "exprs/load_faa" or Shake will fail
-- at runtime.
--
-- Sometimes you might need both a path macro and a custom action to handle the
-- paths it creates. To keep things maintainable, it's recommended to create
-- two functions: one for the path macro, and one that handles the results as
-- if they were passed in separately. The second one can be hidden from users.
-- It's only a little more work than writing the Action only, and makes it much
-- easier to interactively debug.
--
-- TODO is all-vs-all a good example of that?
--
-- One brittle part of this design is the assumption that the expanded paths
-- always have the same seed as the path they were generated from. Because of
-- the way seeds work this seems reasonable. But are there any edge cases we
-- need to be aware of?
--
-- Fundamentally, the reason to do this stuff at the Action rather than Rules
-- level is that it can read the results of previous Actions; it should avoid
-- the problem of mapping over function-generated lists that proved so
-- intractable with Rules.

type Prefix = String

-- the Action here is required because one of the input paths will normally be
-- needed + read to generate the list of output paths. The One variant means
-- the macro returns one path, which will be symlinked to the actual outpath.
-- Many means it returns a list of paths, which will be written to the actual
-- outpath.
--
-- TODO put the Maybe Seed back? Not sure how to pass it here
--
-- TODO should there be 1,2, and 3-arg versions?
--
-- type PathChange    = Prefix -> Maybe Seed -> [FilePath] -> Action  FilePath
type PathExpansion = Prefix -> [FilePath] -> Action [FilePath]

-- For one-to-one path changes. The result will be symlinked to the final output path.
-- TODO is this ever used? Maybe it's not needed except for API consistency
-- newPathChange
--   :: String    -- ^ name of this function
--   -> Prefix    -- ^ prefix (aka name) of the function to expand to
--   -> [TypeSig] -- ^ input types (before macro is applied)
--   -> TypeSig   -- ^ return type (final result after macro + path gathering)
--   -> PathChange -- ^ path changing (1-to-1) macro
--   -> [FnTag]   -- ^ tags
--   -> Function
-- newPathChange fnName mapPrefix inTypes outType pathMacro tags = undefined

-- For one-to-many path changes (mostly maps). The results will be written to a final output path.
-- TODO how should this be arranged to allow more than one at a time? mainly for all-vs-all
--      maybe they could be chained and you take a list of macros here?
--      ooh, or maybe just make one or more intermediate functions so each is a single one -> many step
-- TODO only export the Each and All variants but not this one?
newPathExpansion
  :: String        -- ^ name of this function
  -> Prefix        -- ^ prefix (aka name) of the function to expand to
  -> [TypeSig]     -- ^ input types (before macro is applied)
  -> TypeSig       -- ^ return type (final result after macro + path gathering)
  -> PathExpansion -- ^ path expanding (1-to-many) macro
  -> [FnTag]       -- ^ tags
  -> Function
newPathExpansion fnName mapPrefix inTypes outType pathMacro tags = undefined

-- yesterday's implementation for reference:

-- newMap1 :: String -> NewAction1 -> NewAction1
-- newMap1 prefix actFn (ExprPath outPath) arg1Path = do
--   let loc = "ortholang.interpreter.compile.newmap.newMap1"
--   elems <- readList loc arg1Path -- TODO get the type and do readStrings instead?
--   cfg <- fmap fromJust getShakeExtra
--   let mSeed  = Nothing -- TODO how to get the seed??
--   let mkPath p = exprPathExplicit cfg prefix mSeed [digest p]
--   let outPaths = map mkPath elems -- TODO will fail on lits? paths?
--       outPaths' = map (fromPath loc cfg) outPaths
--   need' loc outPaths' -- TODO should this be needed later only? decides evaluation order
--   writePaths loc outPath outPaths -- TODO will fail on lits?

-- | Gathers the macro-generated output paths into one final output
-- Note: should *not* need the list of paths, right? preserve the laziness if possible
-- TODO have I written this already under a different name?
-- gatherMapResults :: ExprPath -> [FilePath] -> Action ()
-- gatherMapResults = undefined

-- quickly generate _each and _all variants of 1-arg functions, for example makeblastdb

-- TODO does this need to be more modular to handle the psiblast fns?

newPathsEach1 = undefined
newPathsEach2 = undefined
newPathsEach3 = undefined

newPathsAll1 = undefined
newPathsAll2 = undefined
newPathsAll3 = undefined

-- TODO possibly simpler/better interface:
-- each1of1 :: Prefix -> NewAction1 -> NewAction1
-- each1of2 :: Prefix -> NewAction2 -> NewAction2
-- each2of2 :: Prefix -> NewAction2 -> NewAction2
-- ...
-- all1of1 :: Prefix -> NewAction1 -> NewAction1
-- all1of2 :: Prefix -> NewAction2 -> NewAction2
-- all2of2 :: Prefix -> NewAction2 -> NewAction2
-- ...
