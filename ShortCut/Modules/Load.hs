module ShortCut.Modules.Load where

-- TODO move all the mkLoad* stuff from Core here? it's still kind of core

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic        (rExpr, defaultTypeCheck, mkLoad,
                                    mkLoadList, )
import ShortCut.Core.Actions (readLit, writeLits, debugA)
import ShortCut.Core.Paths (exprPath, CutPath, toCutPath, fromCutPath)
import Data.List                  (sort)
import Data.String.Utils          (strip)
import ShortCut.Core.Compile.Compose (compose1)

import System.FilePath.Glob       (glob)
import System.Directory (makeRelativeToCurrentDirectory)
-- import ShortCut.Core.Debug        (debugA)

cutModule :: CutModule
cutModule = CutModule
  { mName = "Load"
  , mDesc = "Load generic lists"
  , mFunctions = [loadList, globFiles]
  }

-- See also the mkLoaders fn at the bottom, which should be used whenever
-- another module introduces a loadable type

---------------
-- load_list --
---------------

loadList :: CutFunction
loadList = mkLoad "load_list" (ListOf str)

----------------
-- glob_files --
----------------

globFiles :: CutFunction
globFiles = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [str] (ListOf str)
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
-- 5. toShortCutListStr puts them in ShortCut literal format
--    (should use str rather than elemRtnType tho)
-- ... looks like this is actually rGlobFiles!
-- now just need to hook it up to other types: load_faa_all etc.
rGlobFiles :: CutState -> CutExpr -> Rules ExprPath
rGlobFiles s@(_,cfg,ref) e@(CutFun _ _ _ _ [p]) = do
  (ExprPath path) <- rExpr s p
  let outPath = exprPath s e
      out'    = fromCutPath cfg outPath
      path'   = toCutPath cfg path
  out' %> \_ -> aGlobFiles cfg ref outPath path'
  return (ExprPath out')
rGlobFiles _ _ = error "bad arguments to rGlobFiles"

aGlobFiles :: CutConfig -> Locks -> CutPath -> CutPath -> Action ()
aGlobFiles cfg ref outPath path = do
  ptn   <- fmap strip $ readLit cfg ref path'
  -- liftIO $ putStrLn $ "ptn: " ++ show ptn
  -- paths <- liftIO $ mapM absolutize =<< glob ptn
  paths  <- liftIO $ fmap sort $ glob ptn
  paths' <- liftIO $ mapM makeRelativeToCurrentDirectory paths
  -- toShortCutListStr cfg str (ExprPath outPath) paths
  writeLits cfg ref out'' paths'
  where
    out'  = fromCutPath cfg outPath
    path' = fromCutPath cfg path
    out'' = debugA cfg "aGlobFiles" out' [out', path']

------------
-- load_* --
------------

-- These are the Haskell functions for generating the CutFunctions;
-- They should be called in other modules with specific types to make loaders for

mkLoadGlob :: String -> CutType -> CutFunction -> CutFunction
mkLoadGlob name loadType eachFn = compose1 name desc globFiles (ListOf str) eachFn
  where
    desc = mkTypeDesc name [str] (ListOf loadType)

mkLoaders :: CutType -> [CutFunction]
mkLoaders loadType = [single, each, glob]
  where
    ext    = extOf loadType
    single = mkLoad     ("load_" ++ ext           ) loadType
    each   = mkLoadList ("load_" ++ ext ++ "_each") loadType
    glob   = mkLoadGlob ("load_" ++ ext ++ "_glob") loadType each
