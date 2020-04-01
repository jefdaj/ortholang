module OrthoLang.Modules.Load where

-- TODO move all the mkLoad* stuff from Core here? it's still kind of core

import Development.Shake
import OrthoLang.Core

import Data.List            (sort)
import Data.String.Utils    (strip)
import OrthoLang.Core       (compose1)
import System.Directory     (makeRelativeToCurrentDirectory)
import System.FilePath.Glob (glob)
import Data.Maybe (fromJust)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "Load"
  , mDesc = "Load generic lists"
  , mTypes = [] -- TODO include str?
  , mFunctions = [loadList, globFiles]
  }

-- See also the mkLoaders fn at the bottom, which should be used whenever
-- another module introduces a loadable type

---------------
-- load_list --
---------------

loadList :: Function
loadList = mkLoad False "load_list" (ListOf str)

----------------
-- glob_files --
----------------

globFiles :: Function
globFiles = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [str] (ListOf str)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rGlobFiles
  }
  where
    name = "glob_files"

-- TODO ok first part looks good, but now need another step?
-- 1. user gives glob as a str
-- 2. compiler saves that to (ExprPath path)
-- 3. this fn needs path, then reads it to ptn
-- 4. this fn does the actual globbing, creating paths
-- 5. toLstStr puts them in OrthoLang literal format
--    (should use str rather than elemRtnType tho)
-- ... looks like this is actually rGlobFiles!
-- now just need to hook it up to other types: load_faa_all etc.
rGlobFiles :: RulesFn
rGlobFiles scr e@(Fun _ _ _ _ [p]) = do
  (ExprPath path) <- rExpr scr p
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let outPath = exprPath cfg dRef scr e
      out'    = fromPath cfg outPath
      path'   = toPath cfg path
  out' %> \_ -> aGlobFiles outPath path'
  return (ExprPath out')
rGlobFiles _ _ = fail "bad arguments to rGlobFiles"

aGlobFiles :: Path -> Path -> Action ()
aGlobFiles outPath path = do
  cfg <- fmap fromJust getShakeExtra
  let out'  = fromPath cfg outPath
      path' = fromPath cfg path
      out'' = traceA "aGlobFiles" out' [out', path']
  ptn <- fmap strip $ readLit path'
  -- liftIO $ putStrLn $ "ptn: " ++ show ptn
  -- paths <- liftIO $ mapM absolutize =<< glob ptn
  paths  <- liftIO $ fmap sort $ glob ptn
  paths' <- liftIO $ mapM makeRelativeToCurrentDirectory paths
  -- toLstStr cfg str (ExprPath outPath) paths
  writeLits out'' paths'

------------
-- load_* --
------------

-- These are the Haskell functions for generating the Functions;
-- They should be called in other modules with specific types to make loaders for

mkLoadGlob :: String -> Type -> Function -> Function
mkLoadGlob name loadType eachFn = compose1 name desc globFiles (ListOf str) eachFn
  where
    desc = mkTypeDesc name [str] (ListOf loadType)

mkLoaders :: Bool -> Type -> [Function]
mkLoaders hashSeqIDs loadType = [single, each, glb]
  where
    ext    = extOf loadType
    single = mkLoad     hashSeqIDs ("load_" ++ ext           ) loadType
    each   = mkLoadList hashSeqIDs ("load_" ++ ext ++ "_each") loadType
    glb    = mkLoadGlob ("load_" ++ ext ++ "_glob") loadType each
