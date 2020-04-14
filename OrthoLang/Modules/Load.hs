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
import OrthoLang.Modules.Curl (curl)

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
  , mTypes = [str, path]
  , mGroups = []
  , mFunctions =
    [ loadListPath
    , loadList
    , globFiles
    , topath
    , topathEach
    ]
  }

---------------
-- load_list --
---------------

loadListPath :: Function
loadListPath = mkLoad False "load_list_path" (Exactly path)

loadList :: Function
loadList = compose1 "load_list" [ReadsFile] topath loadListPath

----------------
-- glob_files --
----------------

globFiles :: Function
globFiles = newFnA1 "glob_files" (Exactly str) (Exactly (ListOf path)) aGlobNew [ReadsDirs]

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

-- TODO does it ever read a URL?
mkLoadGlob :: String -> Function -> Function
mkLoadGlob name eachFn = compose1 name [ReadsDirs, ReadsFile, ReadsURL] globFiles eachFn

mkLoaders :: Bool -> Type -> [Function]
mkLoaders hashSeqIDs loadType = [single, each, glb]
  where
    ext    = tExtOf loadType
    single = mkLoad          hashSeqIDs ("load_" ++ ext                ) (Exactly loadType)
    -- each   = mkLoadList      hashSeqIDs ("load_" ++ ext ++      "_each") (Exactly loadType)
    each   = mkLoadListPaths hashSeqIDs ("load_" ++ ext ++ "_each") (Exactly loadType)
    glb    = mkLoadGlob ("load_" ++ ext ++ "_glob") each

-------------
-- mkLoad* --
-------------

{-|
Takes a string with the filepath to load. Creates a trivial expression file
that's just a symlink to the given path. These should be the only absolute
links, and the only ones that point outside the temp dir.
-}

-- TODO add _path to name here?
mkLoadPath :: Bool -> String -> TypeSig -> Function
mkLoadPath hashSeqIDs name oSig = newMacro name [Exactly path] oSig mLoad [ReadsFile]

mkLoad :: Bool -> String -> TypeSig -> Function
mkLoad hashSeqIDs name oSig = compose1 name [ReadsFile]
  topath
  (mkLoadPath hashSeqIDs (name ++ "_path") oSig)

-- TODO naming convention: _path, _paths?
-- mkLoadNew :: Function
-- mkLoadNew hashSeqIDs name oSig = compose1 name [ReadsDirs] (mkLoad hashSeqIDs (name ++ "_path") oSig) to_path

-- TODO is the oSig needed to?
-- TODO have the to_path fn return whatever string is used internally now, initially
-- TODO this should still leave URL handling to the main load functions for now!
-- TODO and it should be a special load-related expansion as done here, not a generic one for all strs!
mLoad :: MacroExpansion
mLoad scr (Fun r ms ds n [p]) = Fun r ms ds (n ++ "_path") [Fun path ms ds "to_path" [p]]
mLoad _ e = error "modules.load.mLoad" $ "bad argument: " ++ show e

-- TODO have to rename fun so rNamedFunction' doesn't complain
mLoadEach :: MacroExpansion
mLoadEach scr (Fun r ms ds n [p]) = Fun r ms ds (replace "_each" "_path_each" n)
  [Fun (ListOf path) ms ds "to_path_each" [p]]
mLoadEach _ e = error "modules.load.mLoadEach" $ "bad argument: " ++ show e

{-|
Converts user-specified strs (which may represent relative or absolute paths)
to properly formatted OrthoLang Paths. Used to sanitize inputs to the load_* functions
Note the weird name keeps it from conflicting with 'OrthoLang.Core.Paths.toPath'
-}
topath :: Function
topath = hidden $ newFnA1 "to_path" (Exactly str) (Exactly path) aTopath [ReadsDirs]

-- TODO probably do it by mapping over to_path instead? whatever
--      just duplicate for now though and do that once mapping works
topathEach :: Function
topathEach = hidden $ newFnA1 "to_path_each" (Exactly $ ListOf str) (Exactly $ ListOf path) aTopathEach [ReadsDirs]

path :: Type
path = Type
  { tExt = "path"
  , tDesc = "a somewhat-sanitized path (or url)"
  , tShow = defaultShow -- TODO will that work?
  }

-- TODO make up a few test cases: single url, single file (rel and abs), list of each of those, mixed list
-- seems to work on the rel path so far but not the url
-- TODO it should work whether or not the file actually exists! just concat on "$WORKDIR" if no slash
-- TODO and no need to resolve symlinks here the way it does now, right?
-- TODO hey the obvious thing: make a pure function, then have single + mapped action wrappers. easy to test!
-- TODO hey make a new OrthoLang Type called path? it would capture the load types a bit better than str
aTopath :: NewAction1
aTopath (ExprPath outPath) strPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aTopath"
  pth <- fmap (headOrDie "read lits in aLoad failed") $ readLits loc strPath -- TODO safer!
  let pth' = toPath loc cfg pth
  writePath loc outPath pth'

aTopathEach :: NewAction1
aTopathEach (ExprPath outPath) strsPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aTopathEach"
  pths <- readLits loc strsPath -- TODO safer!
  let pths' = map (toPath loc cfg) pths
  writePaths loc outPath pths'

{-|
Like mkLoad, except it operates on a list of strings. Note that you can also
load lists using mkLoad, but it's not recommended because then you have to write
the list in a file, whereas this can handle literal lists in the source code.
-}

mkLoadListPaths :: Bool -> String -> TypeSig -> Function
mkLoadListPaths hashSeqIDs name elemSig = newMacro
  name -- TODO what's the format at this point?
  [Exactly $ ListOf path]
  (ListSigs elemSig)
  mLoadEach
  [ReadsFile]

mkLoadList :: Bool -> String -> TypeSig -> Function
mkLoadList hashSeqIDs name elemSig = compose1
  name
  [ReadsFile]
  topathEach
  (mkLoadListPaths
    hashSeqIDs
    name -- (replace "_each" "_path_each" name)
    (ListSigs elemSig))

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
      -- toAbs line = if isAbsolute line
      --                then line
      --                else cfgWorkDir cfg </> line
  -- need' loc [strPath']

  dRef <- fmap fromJust getShakeExtra
  pth <- readPath loc strPath'

  -- pth <- fmap (headOrDie "read lits in aLoad failed") $ readLits loc strPath' -- TODO safer!
--   pth' <- if isURL pth
--             then curl pth
--             else fmap (toPath loc cfg . toAbs) $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) pth

  t <- liftIO $ decodeNewRulesType cfg dRef $ ExprPath $ outPath'
  hashPath <- aLoadHash hashSeqIDs t pth

  -- liftIO $ putStrLn $ "ext: " ++ takeExtension outPath'
  symlink outPath'' hashPath

-- TODO this should be able to be a list of URLs too. demo that!
rLoadList :: Bool -> RulesFn
rLoadList hashSeqIDs s e@(Fun (ListOf r) _ _ _ [es])
  | r `elem` [str, num] = rLoadListLits s es
  | otherwise = rLoadListPaths hashSeqIDs s e
rLoadList _ _ _ = fail "bad arguments to rLoadList"

-- | Special case for lists of str and num
-- TODO is it different from rLink? seems like it's just a copy/link operation...
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

  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadListPaths"
      outPath'   = fromPath loc cfg outPath
      pathsPath' = fromPath loc cfg pathsPath
      out = traceA loc outPath' [outPath', pathsPath']

  -- paths <- readLitPaths loc pathsPath'
  paths <- readPaths loc pathsPath'
  let paths' = map (fromPath loc cfg) paths
  paths'' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) paths' -- TODO remove?
  let paths''' = map (toPath loc cfg) paths''

  dRef <- fmap fromJust getShakeExtra
  (ListOf t) <- liftIO $ decodeNewRulesType cfg dRef $ ExprPath $ outPath'
  hashPaths <- mapM (aLoadHash hashSeqIDs t) paths'''
  let hashPaths' = map (fromPath loc cfg) hashPaths
  need' loc hashPaths'

  writePaths loc out hashPaths
