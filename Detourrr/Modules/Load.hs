module Detourrr.Modules.Load where

-- TODO move all the mkLoad* stuff from Core here? it's still kind of core

import Development.Shake
import Detourrr.Core.Types
import Detourrr.Core.Compile.Basic        (rExpr, defaultTypeCheck, mkLoad,
                                    mkLoadList, )
import Detourrr.Core.Actions (readLit, writeLits, debugA)
import Detourrr.Core.Paths (exprPath, DtrPath, toDtrPath, fromDtrPath)
import Data.List                  (sort)
import Data.String.Utils          (strip)
import Detourrr.Core.Compile.Compose (compose1)

import System.FilePath.Glob       (glob)
import System.Directory (makeRelativeToCurrentDirectory)
-- import Detourrr.Core.Debug        (debugA)

dtrModule :: DtrModule
dtrModule = DtrModule
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

loadList :: DtrFunction
loadList = mkLoad False "load_list" (ListOf str)

----------------
-- glob_files --
----------------

globFiles :: DtrFunction
globFiles = DtrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] (ListOf str)
  , fDesc = Nothing, fTypeDesc  = mkTypeDesc name  [str] (ListOf str)
  , fFixity    = Prefix
  , fRules  = rGlobFiles
  }
  where
    name = "glob_files"

-- TODO ok first part looks good, but now need another step?
-- 1. user gives glob as a str
-- 2. compiler saves that to (ExprPath path)
-- 3. this fn needs path, then reads it to ptn
-- 4. this fn does the actual globbing, creating paths
-- 5. toDetourrrListStr puts them in Detourrr literal format
--    (should use str rather than elemRtnType tho)
-- ... looks like this is actually rGlobFiles!
-- now just need to hook it up to other types: load_faa_all etc.
rGlobFiles :: DtrState -> DtrExpr -> Rules ExprPath
rGlobFiles s@(_, cfg, ref, _) e@(DtrFun _ _ _ _ [p]) = do
  (ExprPath path) <- rExpr s p
  let outPath = exprPath s e
      out'    = fromDtrPath cfg outPath
      path'   = toDtrPath cfg path
  out' %> \_ -> aGlobFiles cfg ref outPath path'
  return (ExprPath out')
rGlobFiles _ _ = error "bad arguments to rGlobFiles"

aGlobFiles :: DtrConfig -> Locks -> DtrPath -> DtrPath -> Action ()
aGlobFiles cfg ref outPath path = do
  ptn   <- fmap strip $ readLit cfg ref path'
  -- liftIO $ putStrLn $ "ptn: " ++ show ptn
  -- paths <- liftIO $ mapM absolutize =<< glob ptn
  paths  <- liftIO $ fmap sort $ glob ptn
  paths' <- liftIO $ mapM makeRelativeToCurrentDirectory paths
  -- toDetourrrListStr cfg str (ExprPath outPath) paths
  writeLits cfg ref out'' paths'
  where
    out'  = fromDtrPath cfg outPath
    path' = fromDtrPath cfg path
    out'' = debugA cfg "aGlobFiles" out' [out', path']

------------
-- load_* --
------------

-- These are the Haskell functions for generating the DtrFunctions;
-- They should be called in other modules with specific types to make loaders for

mkLoadGlob :: String -> DtrType -> DtrFunction -> DtrFunction
mkLoadGlob name loadType eachFn = compose1 name desc globFiles (ListOf str) eachFn
  where
    desc = mkTypeDesc name [str] (ListOf loadType)

mkLoaders :: Bool -> DtrType -> [DtrFunction]
mkLoaders hashSeqIDs loadType = [single, each, glb]
  where
    ext    = extOf loadType
    single = mkLoad     hashSeqIDs ("load_" ++ ext           ) loadType
    each   = mkLoadList hashSeqIDs ("load_" ++ ext ++ "_each") loadType
    glb    = mkLoadGlob ("load_" ++ ext ++ "_glob") loadType each
