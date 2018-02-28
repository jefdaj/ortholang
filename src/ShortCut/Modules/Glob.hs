module ShortCut.Modules.Glob where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic        (rExpr, defaultTypeCheck)
import ShortCut.Core.Actions (readLit, writeLits)
import ShortCut.Core.Paths (exprPath, CutPath, toCutPath, fromCutPath)
import Data.String.Utils          (strip)

import System.FilePath.Glob       (glob)
import ShortCut.Core.Debug        (debugAction)
import ShortCut.Core.Util         (absolutize)

cutModule :: CutModule
cutModule = CutModule
  { mName = "glob"
  , mFunctions = [globFiles]
  }

globFiles :: CutFunction
globFiles = CutFunction
  { fName      = "glob_files"
  , fTypeCheck = defaultTypeCheck [str] (ListOf str)
  , fFixity    = Prefix
  , fRules  = rGlobFiles
  }

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
rGlobFiles s@(_,cfg,_) e@(CutFun _ _ _ _ [p]) = do
  (ExprPath path) <- rExpr s p
  let outPath = exprPath s e
      out'    = fromCutPath cfg outPath
      path'   = toCutPath cfg path
  out' %> \_ -> aGlobFiles cfg outPath path'
  return (ExprPath out')
rGlobFiles _ _ = error "bad arguments to rGlobFiles"

aGlobFiles :: CutConfig -> CutPath -> CutPath -> Action ()
aGlobFiles cfg outPath path = do
  ptn   <- fmap strip $ readLit cfg path'
  -- liftIO $ putStrLn $ "ptn: " ++ show ptn
  paths <- liftIO $ mapM absolutize =<< glob ptn
  -- toShortCutListStr cfg str (ExprPath outPath) paths
  writeLits cfg out'' paths
  where
    out'  = fromCutPath cfg outPath
    path' = fromCutPath cfg path
    out'' = debugAction cfg "aGlobFiles" out' [out', path']
