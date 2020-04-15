module OrthoLang.Modules.Load
  (

  -- * Module with load_list, glob_files
    olModule

  -- * Functions for use in other modules
  , mkLoad
  , mkLoadPath
  , mkLoadEach
  , mkLoadPathEach
  , mkLoadGlob
  -- , mkLoaders

  -- * Implementation details

  )
  where

-- TODO should load_* be a macro that expands to load* (abs_path ...)?

import OrthoLang.Core
import OrthoLang.Modules.Curl (curl)

import qualified Data.Map.Strict as M

import Control.Monad.IO.Class     (liftIO)
import Data.IORef                 (atomicModifyIORef')
import Data.List                  (sort)
-- import Data.List.Utils            (replace)
import Data.Maybe                 (fromJust)
import Data.String.Utils          (strip)
import Development.Shake          (Action, getShakeExtra, alwaysRerun)
import Development.Shake.FilePath ((</>), (<.>), takeFileName)
import OrthoLang.Util             (absolutize, resolveSymlinks, unlessExists)
-- import System.Directory           (makeRelativeToCurrentDirectory)
-- import System.FilePath            (takeExtension)
import System.FilePath.Glob       (glob)

olModule :: Module
olModule = Module
  { mName = "Load"
  , mDesc = "Load generic lists"
  , mTypes = [str]
  , mGroups = []
  , mEncodings = []
  , mFunctions =
    [ loadList
    , globFiles
    ]
  }

-- note path here refers to the path of the list; elements inside are still strs
-- loadListPath :: Function
-- loadListPath = hidden $ mkLoadPath False "load_list_path" (Exactly $ ListOf str)

-- note path here refers to the path of the list; elements inside are still strs
-- loadList :: Function
-- loadList = mkLoad "load_list" loadListPath

-- loadListPaths :: Function
-- loadListPaths = compose1 "load_list_paths" [ReadsFile] loadList topathEach

globFiles :: Function
globFiles = newFnA1 "glob_files" (Exactly str) (Exactly $ ListOf str) aGlobNew [ReadsDirs]

aGlobNew :: NewAction1
aGlobNew (ExprPath out) a1 = do
  let loc = "modules.load.aGlobNew"
  globptn  <- fmap strip $ readLit loc a1
  relpaths <- liftIO $ fmap sort $ glob globptn
  writeLits loc out relpaths
  -- TODO does this part help with anything? doesn't make paths absolute
  -- liftIO $ putStrLn $ "aGlobNew paths: " ++ show paths
  -- paths' <- liftIO $ mapM makeRelativeToCurrentDirectory paths
  -- let paths'' = map (toPath loc cfg) paths
  -- TODO is there a better way to do this than assuming WORKDIR based paths?
  -- cfg <- fmap fromJust getShakeExtra
  -- let abspaths = map (\p -> toPath loc cfg $ cfgWorkDir cfg </> p) relpaths

-- TODO does it ever read a URL?
mkLoadGlob :: String -> Function -> Function
mkLoadGlob name eachFn = compose1 name [ReadsDirs, ReadsFile, ReadsURL] globFiles eachFn

{-|
Takes a string with the filepath to load. Creates a trivial expression file
that's just a symlink to the given path. These should be the only absolute
links, and the only ones that point outside the temp dir.
-}

-- TODO add _path to name here?
-- mkLoadPath :: Bool -> String -> TypeSig -> Function
-- mkLoadPath hashSeqIDs name oSig = newFnA1 name (Exactly path) oSig (aLoadPath hashSeqIDs) [ReadsFile]

mkLoad :: Bool -> String -> TypeSig -> Function
-- mkLoad name loadPathFn = compose1 name [ReadsFile] topath loadPathFn
mkLoad hashSeqIDs name oSig = newFnA1 name (Exactly str) oSig (aLoad hashSeqIDs) [ReadsFile]

-- | For loading directly from a path, as opposed to the normal "read a str,
--   then load that path" indirection. Used in macro expansions for
--   "re-loading" a file after processing it.
mkLoadPath :: Bool -> String -> TypeSig -> Function
mkLoadPath hashSeqIDs name oSig = newFnA1 name (Exactly str) oSig (aLoadPath hashSeqIDs) [ReadsFile]

loadList :: Function
loadList = mkLoad False "load_list" (Exactly $ ListOf str)

-- TODO is the oSig needed to?
-- TODO have the to_path fn return whatever string is used internally now, initially
-- TODO this should still leave URL handling to the main load functions for now!
-- TODO and it should be a special load-related expansion as done here, not a generic one for all strs!
-- mLoad :: MacroExpansion
-- mLoad scr (Fun r ms ds n [p]) = Fun r ms ds (n ++ "_path") [Fun path ms ds "to_path" [p]]
-- mLoad _ e = error "modules.load.mLoad" $ "bad argument: " ++ show e

-- TODO have to rename fun so rNamedFunction' doesn't complain
-- mLoadEach :: MacroExpansion
-- mLoadEach _ (Fun r ms ds n [p]) = Fun r ms ds (replace "_each" "_path_each" n)
--   [Fun (ListOf path) ms ds "to_path_each" [p]]
-- mLoadEach _ e = error "modules.load.mLoadEach" $ "bad argument: " ++ show e

{-|
Converts user-specified strs (which may represent relative or absolute paths)
to properly formatted OrthoLang Paths. Used to sanitize inputs to the load_* functions
Note the weird name keeps it from conflicting with 'OrthoLang.Core.Paths.toPath'
-}
-- topath :: Function
-- topath = hidden $ newFnA1 "to_path" (Exactly str) (Exactly str) aTopath [ReadsDirs]

-- TODO probably do it by mapping over to_path instead? whatever
--      just duplicate for now though and do that once mapping works
-- topathEach :: Function
-- topathEach = hidden $ newFnA1 "to_path_each" (Exactly $ ListOf str) (Exactly $ ListOf path) aTopathEach [ReadsDirs]

-- TODO make up a few test cases: single url, single file (rel and abs), list of each of those, mixed list
-- seems to work on the rel path so far but not the url
-- TODO it should work whether or not the file actually exists! just concat on "$WORKDIR" if no slash
-- TODO and no need to resolve symlinks here the way it does now, right?
-- TODO hey the obvious thing: make a pure function, then have single + mapped action wrappers. easy to test!
-- TODO hey make a new OrthoLang Type called path? it would capture the load types a bit better than str
-- aTopath :: NewAction1
-- aTopath (ExprPath outPath) strPath = do
--   cfg <- fmap fromJust getShakeExtra
--   let loc = "modules.load.aTopath"
--   pth <- fmap (headOrDie "read lits in aTopath failed") $ readLits loc strPath -- TODO safer!
--   let pth' = toPath loc cfg pth
--   writePath loc outPath pth'

-- aTopathEach :: NewAction1
-- aTopathEach (ExprPath outPath) strsPath = do
--   cfg <- fmap fromJust getShakeExtra
--   let loc = "modules.load.aTopathEach"
--   pths <- readLits loc strsPath -- TODO safer!
--   let pths' = map (toPath loc cfg) pths
--   writePaths loc outPath pths'

{-|
Like mkLoad, except it operates on a list of strings. Note that you can also
load lists using mkLoad, but it's not recommended because then you have to write
the list in a file, whereas this can handle literal lists in the source code.
-}

-- mkLoadPathEach :: Bool -> String -> TypeSig -> Function
-- mkLoadPathEach hashSeqIDs name elemSig = newFnA1 name (Exactly $ ListOf path) (ListSigs elemSig) (aLoadEach hashSeqIDs) [ReadsFile]

mkLoadEach :: Bool -> String -> TypeSig -> Function
-- mkLoadEach name loadPathEachFn = compose1 name [ReadsFile] topathEach loadPathEachFn
mkLoadEach hashSeqIDs name elemSig = newFnA1 name (Exactly $ ListOf str) (ListSigs elemSig) (aLoadEach hashSeqIDs) [ReadsFile]

mkLoadPathEach :: Bool -> String -> TypeSig -> Function
mkLoadPathEach hashSeqIDs name elemSig = newFnA1 name (Exactly $ ListOf str) (ListSigs elemSig) (aLoadListPaths hashSeqIDs) [ReadsFile]

{-|
The paths here are a little confusing: expr is a str of the path we want to
link to. So after compiling it we get a path to *that str*, and have to read
the file to access it. Then we want to `ln` to the file it points to.
-}

-- note: .faa ext turns out to be required for orthofinder to recognize fasta files
aLoadHash :: Bool -> Type -> Path -> Action Path
aLoadHash hashSeqIDs t src = do
  alwaysRerun -- makes sure we can decode hashed seqids
  -- liftIO $ putStrLn $ "aLoadHash " ++ show src
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadHash"
      src' = fromPath loc cfg src
  need' loc [src']
  md5 <- hashContent src
  -- let loc = "modules.load.aLoadHash"
  let tmpDir'   = fromPath loc cfg $ cacheDir cfg "load"
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

-- TODO issue here is that when run from a macro we want to load the input path itself,
--      whereas when run by the user we want to read a str and then load that path
aLoad :: Bool -> NewAction1
aLoad hashSeqIDs o@(ExprPath out') strPath' = do
  alwaysRerun
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoad"
      out = toPath loc cfg out'
  pth'  <- readLitPath loc strPath' -- TODO or is the bug here?

  -- pth'  <- readString loc t strPath' -- TODO fix bug here: sometimes includes lits

  -- TODO is this the proper place to actually do the download?
  pth'' <- if isURL (fromPath loc cfg pth') then curl pth' else return pth'

  dRef <- fmap fromJust getShakeExtra
  t    <- liftIO $ decodeNewRulesType cfg dRef o
  -- liftIO $ putStrLn $ "aLoadPath pth': " ++ show pth'
  -- liftIO $ putStrLn $ "aLoadPath pth'': " ++ show pth''
  -- liftIO $ putStrLn $ "aLoadPath t: " ++ show t
  hashPath <- aLoadHash hashSeqIDs t pth''

  symlink out hashPath

-- TODO should this alwaysRerun?
aLoadPath :: Bool -> NewAction1
aLoadPath hashSeqIDs o@(ExprPath out') loadPath' = do
  alwaysRerun
  cfg  <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadPath"
      out = toPath loc cfg out'
      loadPath = toPath loc cfg loadPath'
  dRef <- fmap fromJust getShakeExtra
  t    <- liftIO $ decodeNewRulesType cfg dRef o
  hashPath <- aLoadHash hashSeqIDs t loadPath
  symlink out hashPath

-- TODO this should be able to be a list of URLs too. demo that!
aLoadEach :: Bool -> NewAction1
aLoadEach hashSeqIDs o lstPath = do
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  (ListOf et) <- liftIO $ decodeNewRulesType cfg dRef o
  let aFn = if et `elem` [str, num]
              then aLoadListLits
              else aLoadListPaths hashSeqIDs
  aFn o lstPath

aLoadListLits :: NewAction1
aLoadListLits (ExprPath outPath') litsPath' = do
  -- cfg  <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadListLits"
      -- litsPath  = toPath loc cfg litsPath'
      -- outPath   = toPath loc cfg outPath'
      out       = traceA loc outPath' [outPath', litsPath']
  lits  <- readLits loc litsPath'
  lits' <- liftIO $ mapM absolutize lits
  writeLits loc out lits'

aLoadListPaths :: Bool -> NewAction1
aLoadListPaths hashSeqIDs o@(ExprPath out') linksPath' = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.load.aLoadListPaths"
  dRef <- fmap fromJust getShakeExtra
  (ListOf t) <- liftIO $ decodeNewRulesType cfg dRef o -- TODO does this make sense?
  links  <- readLitPaths loc linksPath' -- TODO or is the bug here?
  paths' <- liftIO $ mapM (resolveSymlinks $ Just $ cfgTmpDir cfg) $ map (fromPath loc cfg) links -- TODO remove?
  let paths = map (toPath loc cfg) paths'
  hashPaths <- mapM (aLoadHash hashSeqIDs t) paths
  let hashPaths' = map (fromPath loc cfg) hashPaths
  need' loc hashPaths' -- TODO remove?
  writePaths loc out' hashPaths
