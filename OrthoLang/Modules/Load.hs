module OrthoLang.Modules.Load
  (

  -- * Module with load_list, glob_files
    olModule

  -- * Functions for use in other modules
  , mkLoad
  , mkLoadList
  , mkLoadGlob
  , mkLoaders

  -- * Implementation details
  , rLoad
  , rLoadList
  , rLoadListLits
  , rLoadListPaths
  , aLoad
  , aLoadListLits
  , aLoadListPaths

  )
  where

-- TODO should load_* be a macro that expands to load* (abs_path ...)?

import OrthoLang.Core
import OrthoLang.Modules.Curl (curl, isURL)

import qualified Data.Map.Strict as M

import Control.Monad.IO.Class     (liftIO)
import Data.IORef                 (atomicModifyIORef')
import Data.List                  (sort)
import Data.List.Utils            (replace)
import Data.Maybe                 (fromJust)
import Data.String.Utils          (strip)
import Development.Shake          (Action, getShakeExtra, getShakeExtraRules, (%>), alwaysRerun)
import Development.Shake.FilePath ((</>), (<.>), takeFileName, isAbsolute)
import OrthoLang.Util             (absolutize, resolveSymlinks, headOrDie, unlessExists)
import System.Directory           (makeRelativeToCurrentDirectory)
import System.FilePath            (takeExtension)
import System.FilePath.Glob       (glob)

olModule :: Module
olModule = Module
  { mName = "Load"
  , mDesc = "Load generic lists"
  , mTypes = [str]
  , mGroups = []
  , mFunctions = [loadList, globFiles, realpath, realpathEach]
  }

---------------
-- load_list --
---------------

loadList :: Function
loadList = mkLoad False "load_list" (Exactly $ ListOf str)

----------------
-- glob_files --
----------------

globFiles :: Function
globFiles = newFnA1 "glob_files" (Exactly str) (Exactly (ListOf str)) aGlobNew [ReadsDirs]

aGlobNew :: NewAction1
aGlobNew (ExprPath out) a1 = do
  let loc = "modules.load.aGlobNew"
  ptn    <- fmap strip $ readLit loc a1
  paths  <- liftIO $ fmap sort $ glob ptn
  paths' <- liftIO $ mapM makeRelativeToCurrentDirectory paths
  writeLits loc out paths'

------------
-- load_* --
------------

mkLoadGlob :: String -> Function -> Function
mkLoadGlob name eachFn = compose1 name [ReadsDirs, ReadsFile, ReadsURL] globFiles eachFn

mkLoaders :: Bool -> Type -> [Function]
mkLoaders hashSeqIDs loadType = [single, each, glb]
  where
    ext    = tExtOf loadType
    single = mkLoad     hashSeqIDs ("load_" ++ ext           ) (Exactly loadType) -- TODO is this right?
    each   = mkLoadList hashSeqIDs ("load_" ++ ext ++ "_each") (Exactly loadType) -- TODO is this right?
    glb    = mkLoadGlob ("load_" ++ ext ++ "_glob") each

-------------
-- mkLoad* --
-------------

{-|
Takes a string with the filepath to load. Creates a trivial expression file
that's just a symlink to the given path. These should be the only absolute
links, and the only ones that point outside the temp dir.
-}
mkLoad :: Bool -> String -> TypeSig -> Function
mkLoad hashSeqIDs name oSig = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly str]
  , fOutput = oSig
  , fTags = [ReadsFile, ReadsURL]
  , fOldRules = rLoad hashSeqIDs
  , fNewRules = NewNotImplemented -- TODO mLoad here
  }

-- TODO naming convention: _path, _paths?
-- mkLoadNew :: Function
-- mkLoadNew hashSeqIDs name oSig = compose1 name [ReadsDirs] (mkLoad hashSeqIDs (name ++ "_path") oSig) realpath

-- TODO is the oSig needed to?
-- TODO have the to_path fn return whatever string is used internally now, initially
-- TODO this should still leave URL handling to the main load functions for now!
-- TODO and it should be a special load-related expansion as done here, not a generic one for all strs!
mLoad :: MacroExpansion
mLoad scr (Fun r ms ds n [p]) = Fun r ms ds (replace "_path" "" n) [Fun str ms ds "realpath" [p]]
mLoad _ e = error "modules.load.mLoad" $ "bad argument: " ++ show e

mLoadEach :: MacroExpansion
mLoadEach scr (Fun r ms ds n [p]) = Fun r ms ds (replace "_path" "" n) [Fun (ListOf str) ms ds "realpath_each" [p]]
mLoadEach _ e = error "modules.load.mLoadEach" $ "bad argument: " ++ show e

{-|
Converts user-specified strs (which may represent relative or absolute paths)
to properly formatted OrthoLang Paths. Used to sanitize inputs to the load_* functions
-}
realpath :: Function
realpath = hidden $ newFnA1 "realpath" (Exactly str) (Exactly str) aRealpath [ReadsDirs]

realpathEach :: Function
realpathEach = hidden $ newFnA1 "realpath_each" (Exactly $ ListOf str) (Exactly $ ListOf str) aRealpathEach [ReadsDirs]

aRealpath :: NewAction1
aRealpath (ExprPath out) userStrPath = undefined

aRealpathEach :: NewAction1
aRealpathEach (ExprPath out) userStrListPath = undefined

{-|
Like mkLoad, except it operates on a list of strings. Note that you can also
load lists using mkLoad, but it's not recommended because then you have to write
the list in a file, whereas this can handle literal lists in the source code.
-}
mkLoadList :: Bool -> String -> TypeSig -> Function
mkLoadList hashSeqIDs name elemSig = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly (ListOf str)]
  , fOutput = ListSigs elemSig
  , fTags = [ReadsFile, ReadsURL]
  , fOldRules = rLoadList hashSeqIDs
  , fNewRules = NewNotImplemented
  }

--------------------
-- implementation --
--------------------

{-|
The paths here are a little confusing: expr is a str of the path we want to
link to. So after compiling it we get a path to *that str*, and have to read
the file to access it. Then we want to `ln` to the file it points to.
-}
rLoad :: Bool -> RulesFn
rLoad hashSeqIDs scr e@(Fun _ _ _ _ [p]) = do
  (ExprPath strPath) <- rExpr scr p
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "modules.load.rLoad"
      out  = exprPath cfg dRef scr e
      out' = fromPath loc cfg out
  out' %> \_ -> aLoad hashSeqIDs (toPath loc cfg strPath) out
  return (ExprPath out')
rLoad _ _ _ = fail "bad argument to rLoad"

-- note: .faa ext turns out to be required for orthofinder to recognize fasta files
aLoadHash :: Bool -> Type -> Path -> Action Path
aLoadHash hashSeqIDs t src = do
  alwaysRerun -- makes sure we can decode hashed seqids
  -- liftIO $ putStrLn $ "aLoadHash " ++ show src
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadHash"
      src' = fromPath loc cfg src
  need' "modules.load.aLoadHash" [src']
  md5 <- hashContent src
  let loc = "modules.load.aLoadHash"
      tmpDir'   = fromPath loc cfg $ cacheDir cfg "load"
      ext = tExtOf t -- TODO safe because always an exact type?
      hashPath' = tmpDir' </> md5 <.> ext
      hashPath  = toPath loc cfg hashPath'
  if not hashSeqIDs
    then symlink hashPath src
    else do
      let idsPath' = hashPath' <.> "ids"
          idsPath  = toPath loc cfg idsPath'
      unlessExists idsPath' $ hashIDsFile2 src hashPath -- TODO remove unlessExists?
      let (Path k) = hashPath
          v = takeFileName src' -- TODO ext issue here?
      newIDs <- readIDs idsPath
      -- TODO factor this out into a fn like addDigests?
      ids <- fmap fromJust getShakeExtra
      liftIO $ atomicModifyIORef' ids $
        \h@(IDs {hFiles = f, hSeqIDs = s}) -> (h { hFiles  = M.insert k v f
                                                 , hSeqIDs = M.insert k newIDs s}, ())
      dRef <- fmap fromJust getShakeExtra
      liftIO $ addDigest dRef t hashPath
  return hashPath

aLoad :: Bool -> Path -> Path -> Action ()
aLoad hashSeqIDs strPath outPath = do
  alwaysRerun
  cfg <- fmap fromJust getShakeExtra
  let strPath'  = fromPath loc cfg strPath
      outPath'  = fromPath loc cfg outPath
      loc = "modules.load.aLoad"
      outPath'' = traceA loc outPath [strPath', outPath']
      toAbs line = if isAbsolute line
                     then line
                     else cfgWorkDir cfg </> line
  dRef <- fmap fromJust getShakeExtra
  t <- liftIO $ decodeNewRulesType cfg dRef $ ExprPath $ outPath'
  need' loc [strPath']
  pth <- fmap (headOrDie "read lits in aLoad failed") $ readLits loc strPath' -- TODO safer!
  pth' <- if isURL pth
            then curl pth
            else fmap (toPath loc cfg . toAbs) $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) pth
  hashPath <- aLoadHash hashSeqIDs t pth'
  -- liftIO $ putStrLn $ "ext: " ++ takeExtension outPath'
  symlink outPath'' hashPath
  where

rLoadList :: Bool -> RulesFn
rLoadList hashSeqIDs s e@(Fun (ListOf r) _ _ _ [es])
  | r `elem` [str, num] = rLoadListLits s es
  | otherwise = rLoadListPaths hashSeqIDs s e
rLoadList _ _ _ = fail "bad arguments to rLoadList"

-- | Special case for lists of str and num
-- TODO is it different from rLink? seems like it's just a copy/link operation...
-- TODO need to readLitPaths?
rLoadListLits :: RulesFn
rLoadListLits scr expr = do
  (ExprPath litsPath') <- rExpr scr expr
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "modules.load.rLoadListLits"
      outPath  = exprPath cfg dRef scr expr
      outPath' = fromPath loc cfg outPath
      litsPath = toPath loc cfg litsPath'
  outPath' %> \_ -> aLoadListLits outPath litsPath
  return (ExprPath outPath')

-- TODO have to listLitPaths here too?
aLoadListLits :: Path -> Path -> Action ()
aLoadListLits outPath litsPath = do
  cfg  <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadListLits"
      litsPath' = fromPath loc cfg litsPath
      outPath'  = fromPath loc cfg outPath
      out       = traceA loc outPath' [outPath', litsPath']
  lits  <- readLits loc litsPath'
  lits' <- liftIO $ mapM absolutize lits
  writeLits loc out lits'

-- | Regular case for lists of any other file type
-- TODO hash mismatch here?
rLoadListPaths :: Bool -> RulesFn
rLoadListPaths hashSeqIDs scr e@(Fun _ _ _ _ [es]) = do
  (ExprPath pathsPath) <- rExpr scr es
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let loc = "modules.load.rLoadListPaths"
      outPath  = exprPath cfg dRef scr e
      outPath' = fromPath loc cfg outPath
  outPath' %> \_ -> aLoadListPaths hashSeqIDs (toPath loc cfg pathsPath) outPath
  return (ExprPath outPath')
rLoadListPaths _ _ _ = fail "bad arguments to rLoadListPaths"

aLoadListPaths :: Bool -> Path -> Path -> Action ()
aLoadListPaths hashSeqIDs pathsPath outPath = do
  -- Careful! The user will write paths relative to workdir and those come
  -- through as a (ListOf str) here; have to read as Strings and convert to
  -- Paths
  -- TODO this should be a macro expansion in the loader fns rather than a basic rule!
  -- alwaysRerun -- TODO does this help?
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadListPaths"
      outPath'   = fromPath loc cfg outPath
      pathsPath' = fromPath loc cfg pathsPath
      out = traceA loc outPath' [outPath', pathsPath']
  paths <- readLitPaths loc pathsPath'
  let paths' = map (fromPath loc cfg) paths
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths'
  let paths''' = map (toPath loc cfg) paths''
  dRef <- fmap fromJust getShakeExtra
  (ListOf t) <- liftIO $ decodeNewRulesType cfg dRef $ ExprPath $ outPath'
  hashPaths <- mapM (aLoadHash hashSeqIDs t) paths'''
  let hashPaths' = map (fromPath loc cfg) hashPaths
  need' loc hashPaths'
  writePaths loc out hashPaths
