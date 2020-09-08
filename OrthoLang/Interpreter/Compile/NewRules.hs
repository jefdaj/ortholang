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
  , newFnS4

  -- * Functions from Actions
  -- $fromactions
  , NewAction1
  , NewAction2
  , NewAction3
  , NewAction4
  , newFnA1
  , newFnA2
  , newFnA3

  -- * Binary operators from Actions
  , newBop

  -- * Functions from Macros
  -- $frommacros
  , ExprExpansion
  , newExprExpansion
  , hidden

  -- * Path expansions
  -- $pathexpansions
  , newMap1of1
  , newMap2of2
  , newMap2of3
  , newMap3of3
  , newDate1of1
  , newDate1of2
  , newDate1of3
  , newDate1of4

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
  , aNewRulesS4
  , aNewRulesS
  , aNewRules
  , aLoadIDs -- TODO where should this go? Actions?
  -- , newPathExpansion

  )
  where

import Prelude hiding (error)
import OrthoLang.Debug
import Control.Monad.Reader
import Development.Shake hiding (doesDirectoryExist)
import System.Directory (doesDirectoryExist)
import OrthoLang.Interpreter.Actions       (runCmd, CmdDesc(..), writePaths, trackWrite', readLit, writeLit)
import OrthoLang.Interpreter.Sanitize (readIDs)
import OrthoLang.Locks (withWriteOnce)
import OrthoLang.Types
import OrthoLang.Util (resolveSymlinks)
import System.Exit (ExitCode(..))

import Control.Monad              (when)
import Data.Either.Utils          (fromRight)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((</>), takeDirectory, dropExtension, takeBaseName, makeRelative)
import OrthoLang.Interpreter.Actions     (need', readPaths)
import OrthoLang.Interpreter.Paths (toPath, fromPath, decodeNewRulesDeps, addDigest, listDigestsInPath, getExprPathSeed, pathDigest, exprPath, pathDigest)
import qualified Data.Map.Strict as M
import Data.IORef                 (atomicModifyIORef')
import System.Directory           (createDirectoryIfMissing)

import Data.Time
import Text.Printf
-- import System.FilePath ((</>))
import Data.List.Split (splitOn)
import OrthoLang.Util (absolutize, globFiles)
import Data.List (sort)
import Data.List.Utils (replace)
import System.Directory (doesPathExist)
import System.FilePath (takeBaseName, takeDirectory)

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

newFnS4
  :: String                               -- ^ name
  -> (TypeSig, TypeSig, TypeSig, TypeSig) -- ^ 4 argument types
  -> TypeSig                              -- ^ return type
  -> String                               -- ^ script basename
  -> [FnTag]                              -- ^ tags
  -> (CmdDesc -> CmdDesc)                 -- ^ extra options
  -> Function
newFnS4 n (i1, i2, i3, i4) r s ts os = newFn n Nothing [i1, i2, i3, i4] r rNewRulesA4 (aNewRulesS4 s os) ts

aNewRulesS4 :: String -> (CmdDesc -> CmdDesc) -> NewAction4
aNewRulesS4 sname opts o i1 i2 i3 i4 = aNewRulesS sname opts o [i1, i2, i3, i4]

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
    , cmdNoNeedDirs = []
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
-- expects the same number of input files that the 'Function' will pass it,
-- but says nothing about their types.

type NewAction1 = ExprPath -> FilePath                                     -> Action ()
type NewAction2 = ExprPath -> FilePath -> FilePath                         -> Action ()
type NewAction3 = ExprPath -> FilePath -> FilePath -> FilePath             -> Action ()
type NewAction4 = ExprPath -> FilePath -> FilePath -> FilePath -> FilePath -> Action ()

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
  let loc = "ortholang.interpreter.compile.newrules.aLoadIDs"
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
  let useSeed = usesSeed $ fromRight $ findFun mods name
      ptn = newPattern cfg useSeed name nArgs
      ptn' = traceShow "rNewrules" ptn
  ptn' %> \p -> do
    -- TODO if adding rules works anywhere in an action it'll be here right?
    aNewRules applyFn oSig aFn (ExprPath p)

-- | This deduplicates singleton paths (and others?) to prevent duplicate blast tmpfiles
canonicalExprLinks :: [FilePath] -> Action [FilePath]
canonicalExprLinks deps = do
  cfg  <- fmap fromJust $ getShakeExtra
  liftIO $ mapM (resolveSymlinks (Just [tmpdir cfg </> "vars", tmpdir cfg </> "exprs"])) deps

-- TODO is it possible to get the return type here?
rNewRulesA1 :: TypeSig -> String -> NewAction1 -> Rules ()
rNewRulesA1 = rNewRules 1 applyList1

applyList1 :: (FilePath -> Action ()) -> [FilePath] -> Action ()
applyList1 fn deps = do
  deps' <- canonicalExprLinks deps
  fn (deps' !! 0)

rNewRulesA2 :: TypeSig -> String -> NewAction2 -> Rules ()
rNewRulesA2 = rNewRules 2 applyList2

applyList2 :: (FilePath -> FilePath -> Action ()) -> [FilePath] -> Action ()
applyList2 fn deps = do
  deps' <- canonicalExprLinks deps
  fn (deps' !! 0) (deps' !! 1)

rNewRulesA3 :: TypeSig -> String -> NewAction3 -> Rules ()
rNewRulesA3 = rNewRules 3 applyList3

applyList3 :: (FilePath -> FilePath -> FilePath -> Action ()) -> [FilePath] -> Action ()
applyList3 fn deps = do
  deps' <- canonicalExprLinks deps
  fn (deps' !! 0) (deps' !! 1) (deps' !! 2)

rNewRulesA4 :: TypeSig -> String -> NewAction4 -> Rules ()
rNewRulesA4 = rNewRules 4 applyList4

applyList4 :: (FilePath -> FilePath -> FilePath -> FilePath -> Action ()) -> [FilePath] -> Action ()
applyList4 fn deps = do
  deps' <- canonicalExprLinks deps
  fn (deps' !! 0) (deps' !! 1) (deps' !! 2) (deps' !! 3)

{-|
-}
aNewRules
  :: (t -> [FilePath] -> Action ()) -- ^ one of the apply{1..4} fns
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
type ExprExpansion = [Module] -> Script -> Expr -> Expr

{-|
Macros are straighforward to implement: use any 'ExprExpansion' to create one
directly.  The only type checking planned so far is that
'OrthoLang.Interpreter.Compile.Basic.rMacro' will prevent macro expansions from
changing the return type of their input 'Expr'.
-}
newExprExpansion :: String -> [TypeSig] -> TypeSig -> ExprExpansion -> [FnTag] -> Function
newExprExpansion name iSigs oSig mFn tags = Function
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

-- $pathexpansions
-- This is mostly geared toward writing the \_each and \_all mapped function
-- variants easily. It's analagous to macro expansion, but the transformation
-- is done on the input Paths directly rather than on the Exprs that were
-- compiled to generate those Paths.
--
-- Fundamentally, the reason to do this stuff at the Action rather than Rules
-- level is that it can read the results of previous Actions; it should avoid
-- the problem of mapping over function-generated lists that proved so
-- intractable with Rules.
--
-- The same priciple applies as when writing Expr expansions: you have to make
-- sure that the system knows how to handle the results already. For example if
-- you make a macro that rewrites "exprs/load\_faa\_each" paths to
-- "exprs/load\_faa" paths, there must already be Rules for "exprs/load\_faa" or
-- Shake will fail at runtime.
--
-- Sometimes you might need both a path expansion and a custom action to handle
-- the paths it creates. To keep things maintainable, it's recommended to
-- create two functions: one for the path expansion, and one that handles the
-- results as if they were passed that way originally. The second one can be
-- hidden from users. It's only a little more work than writing the Action
-- only, and makes it much easier to interactively debug.
--
-- TODO is all-vs-all a good example of that?
--
-- One brittle part of this design is the assumption that the expanded paths
-- always have the same seed as the path they were generated from. Because of
-- the way seeds work this seems reasonable. But are there any edge cases we
-- need to be aware of?

type Prefix = String

-- the Action here is required because one of the input paths will normally be
-- needed + read to generate the list of output paths.
--
-- type PathChange    = Prefix -> Maybe Seed -> [FilePath] -> Action  FilePath
-- type PathExpansion = Prefix -> [FilePath] -> Action [FilePath]

-- quickly generate _each and _all variants of 1-arg functions, for example makeblastdb

-- | Maps a NewAction1 over its only argument and writes the result list to the
--   final output path.
newMap1of1 :: Prefix -> NewAction1
newMap1of1 prefix out lst = newMap prefix 1 out lst

-- | Maps a NewAction2 over its 2nd argument and writes the result list to the
--   final output path.
newMap2of2 :: Prefix -> NewAction2
newMap2of2 prefix out _ lst = newMap prefix 2 out lst

-- | Maps a NewAction3 over its 2nd argument and writes the result list to the
--   final output path.
newMap2of3 :: Prefix -> NewAction3
newMap2of3 prefix out _ lst _ = newMap prefix 2 out lst

-- | Maps a NewAction3 over its 3rd argument and writes the result list to the
--   final output path.
newMap3of3 :: Prefix -> NewAction3
newMap3of3 prefix out _ _ lst = newMap prefix 3 out lst

-- | Pass it a 1-argument Action. It maps it over the list and writes the outputs to a list file.
--   Used to implement all the newMapNofN fns above.
newMap :: Prefix -> Int -> ExprPath -> FilePath -> Action ()
newMap mapPrefix mapIndex out@(ExprPath outList) listToMapOver = do
  let loc = "ortholang.interpreter.compile.newrules.newMap"
  liftIO $ debug loc $ "mapPrefix: " ++ mapPrefix
  liftIO $ debug loc $ "mapIndex: " ++ show mapIndex
  liftIO $ debug loc $ "outList: " ++ outList
  liftIO $ debug loc $ "listToMapOver: " ++ listToMapOver
  cfg <- fmap fromJust getShakeExtra
  
  liftIO $ debug loc $ "about to readPaths from  " ++ listToMapOver
  inPaths <- readPaths loc listToMapOver -- TODO get the type and do readStrings instead?
  liftIO $ debug loc $ "successfully readPaths from  " ++ listToMapOver

  dRef <- fmap fromJust getShakeExtra
  ((ListOf oType), dTypes, dPaths) <- liftIO $ decodeNewRulesDeps cfg dRef out
  liftIO $ debug loc $ "oType: " ++ show oType
  liftIO $ debug loc $ "dTypes: " ++ show dTypes
  liftIO $ debug loc $ "dPaths: " ++ show dPaths

  -- TODO remove the addDigests?
  let elemType = dTypes !! (mapIndex - 1)
      elemType' = traceShow loc elemType
  liftIO $ addDigest dRef (ListOf elemType') $ toPath loc cfg listToMapOver

  liftIO $ mapM_ (addDigest dRef elemType') inPaths -- TODO remove? also this is the wrong type i think
  liftIO $ addDigest dRef (ListOf oType) $ toPath loc cfg outList
  let dPaths' = map (fromPath loc cfg) dPaths
  -- need' loc dPaths'

  -- let inPaths' = map (fromPath loc cfg) inPaths
  liftIO $ debug loc $ "inPaths: " ++ show inPaths
  mods <- fmap fromJust getShakeExtra
  let outPaths = newMapOutPaths mods cfg mapPrefix mapIndex outList inPaths -- TODO what happens if these are lits?
  liftIO $ mapM_ (addDigest dRef oType) outPaths
  let outPaths' = map (fromPath loc cfg) outPaths
  liftIO $ debug loc $ "outPaths: " ++ show outPaths

  -- TODO are the need and trackwrite parts redundant?
  -- forM_ (zip outPaths' inPaths') $ \(o, i) -> do
    -- need' loc [i]
    -- liftIO $ createDirectoryIfMissing True o
    -- actToMap (ExprPath o) i
    -- trackWrite' [o]

  -- need' loc inPaths'
  need' loc outPaths'
  writePaths loc outList outPaths -- TODO will fail on lits?
  -- trackWrite' (outList:outPaths')

-- TODO does this need to be added to the digestmap?
newMapOutPaths :: [Module] -> Config -> Prefix -> Int -> FilePath -> [Path] -> [Path]
newMapOutPaths mods cfg newFnName mapIndex oldPath newPaths = map (toPath loc cfg . mkPath) newPaths
  where
    loc = "ortholang.interpreter.compile.newrules.newMapOutPaths"
    oldDigests = listDigestsInPath cfg oldPath
    oldDigests' = traceShow loc oldDigests

    -- TODO can we remove this?
    oldPath' = trace loc ("oldPath: " ++ show oldPath) oldPath
    oldSeed    = getExprPathSeed oldPath'

    -- newFn = findFun undefined newFnName
    -- seed = if Nondeterministic `elem` (fTags newFn) then Just (Seed 0) else Nothing
    -- newSuffix = case seed of
    newSuffix = case findFun mods newFnName of
                  Left e -> error loc e
                  Right f -> if not (usesSeed f)
                               then "result"
                               else case oldSeed of
                                      Nothing -> "result" -- TODO what if only the new fn needs it?
                                      Just (Seed n) -> ('s':show n) </> "result" -- TODO could this be the read issue?
    newDigests path = map (\(PathDigest s) -> s) $
                      replaceN oldDigests' (mapIndex - 1, pathDigest path)
    mkPath p = tmpdir cfg </> "exprs" </> newFnName </>
               (foldl1 (</>) (newDigests p)) </>
               newSuffix

-- replace the Nth element in a list
-- old.reddit.com/r/haskell/comments/8jui5k/how_to_replace_an_element_at_an_index_in_a_list/dz2i8rj/
replaceN :: [a] -> (Int, a) -> [a]
replaceN [] _ = []
replaceN (_:xs) (0,a) = a:xs
replaceN (x:xs) (n,a) =
  if n < 0
    then (x:xs)
    else x: replaceN xs (n-1,a)

------------------------------------------------------
-- expand user-supplied cache dates to proper dates --
------------------------------------------------------

-- | Expands a 1-argument function to the corresponding _date version
newDate1of1 :: Prefix -> NewAction1
newDate1of1 prefix out a1 = newDate prefix out a1

-- | Expands a 2-argument function to the corresponding _date version
newDate1of2 :: Prefix -> NewAction2
newDate1of2 prefix out a1 _ = newDate prefix out a1

-- | Expands a 3-argument function to the corresponding _date version
newDate1of3 :: Prefix -> NewAction3
newDate1of3 prefix out a1 _ _ = newDate prefix out a1

-- | Expands a 4-argument function to the corresponding _date version
newDate1of4 :: Prefix -> NewAction4
newDate1of4 prefix out a1 _ _ _ = newDate prefix out a1

-- TODO should the prefix here have _date added?
-- TODO should the actual date be used instead of the hash in these expr paths?
newDate :: Prefix -> ExprPath -> FilePath -> Action ()
newDate prefix (ExprPath outPath) userPath = do
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  let loc = "ortholang.interpreter.compile.newrules.newDate"
      cacheDir   = tmpdir cfg </> "exprs" </> prefix
      cachePath  = makeRelative (cacheDir) outPath
      cacheDirD  = cacheDir ++ "_date"
  userDate   <- readLit loc userPath
  properDate <- liftIO $ resolveCache cacheDirD cachePath userDate
  let properPath   = exprPath cfg dRef emptyScript $ Lit str properDate
      properPath'  = fromPath loc cfg properPath
      (PathDigest old) = pathDigest $ toPath loc cfg userPath
      (PathDigest new) = pathDigest properPath
      outPathD     = cacheDirD </> replace old new outPath
  writeLit loc properPath' properDate    -- TODO remove?
  liftIO $ addDigest dRef str properPath -- TODO remove?
  need' loc [outPathD]

-----------------------------------
-- future core library functions --
-----------------------------------

dayToDir :: Day -> FilePath
dayToDir date = sYear </> sMonth </> sDay
  where
    (year, month, day) = toGregorian date
    sYear  = printf "%04d" year
    sMonth = printf "%02d" month
    sDay   = printf "%02d" day

dirToDay :: FilePath -> Day
dirToDay dir = fromGregorian nYear nMonth nDay
  where
    (day:month:year:_) = reverse $ splitOn "/" dir
    nYear  = read year  :: Integer
    nMonth = read month :: Int
    nDay   = read day   :: Int

getToday :: IO Day
getToday = do
  (UTCTime today _) <- getCurrentTime
  return today

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- | Returns an existing cache matching a specific path + date, or Nothing
existingCacheDated :: FilePath -> FilePath -> Day -> IO (Maybe FilePath)
existingCacheDated cacheDir cachePath day = do
  cacheDir' <- absolutize cacheDir
  let dated = cacheDir' </> dayToDir day </> cachePath
  exists <- doesPathExist dated
  return $ if exists
    then Just dated
    else Nothing

-- | Returns the latest existing cache matching a specific path, or Nothing
--   Warning: assumes all files in the cache dir are yyyy/mm/dd formatted
existingCacheLatest :: FilePath -> FilePath -> IO (Maybe FilePath)
existingCacheLatest cacheDir cachePath = do
  matches <- globFiles $ cacheDir </> "*" </> "*" </> "*" </> cachePath
  if null matches then return Nothing
  else return $ Just $ head $ sort matches

-- | Returns today's cache for a specific file, whether or not it exists yet
cacheToday :: FilePath -> FilePath -> IO FilePath
cacheToday cacheDir cachePath = do
  today <- getToday
  cacheDir' <- absolutize cacheDir
  let dated = cacheDir' </> dayToDir today </> cachePath
  return dated

-- | Entry point for finding a cached file from a user-specified date.
--   If this returns Nothing, it means we'll need to download the file.
--
--   TODO confirmation dialog before downloading, or just assume?
cacheUser :: FilePath -> FilePath -> String -> IO (Maybe FilePath)
cacheUser cacheDir cachePath userInput = do
  let userDay = parseDate userInput
  userCache <- case userDay of
    Nothing -> return Nothing
    Just d -> existingCacheDated cacheDir cachePath d
  cached  <- existingCacheLatest cacheDir cachePath
  today   <- cacheToday cacheDir cachePath
  return $ if      userInput == "cached" then cached
           else if userInput == "today"  then Just today
           else    userCache

-- | Overall entry point, which would include user-facing warnings (if any).
resolveCache :: FilePath -> FilePath -> String -> IO FilePath
resolveCache cacheDir cachePath userInput = do
  -- if the cache path resolution works, this is Just something
  -- TODO if not, just return cacheToday here? or should the distinction be used for a warning?
  mUserCachePath <- liftIO $ cacheUser cacheDir cachePath userInput
  cachePath' <- case mUserCachePath of
    Nothing -> liftIO $ cacheToday cacheDir cachePath
    Just p -> return p

  -- TODO the final output path should depend on this rather than the initial userCacheDescPath', right?
  -- let cachePath = toPath loc cfg cachePath'
  -- liftIO $ createDirectoryIfMissing True $ takeDirectory cachePath'
  return cachePath'
