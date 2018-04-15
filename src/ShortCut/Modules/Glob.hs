module ShortCut.Modules.Glob where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic        (rExpr, defaultTypeCheck)
import ShortCut.Core.Actions (readLit, writeLits, debugA)
import ShortCut.Core.Paths (exprPath, CutPath, toCutPath, fromCutPath)
import Data.String.Utils          (strip)

import System.FilePath.Glob       (glob)
import System.Directory (makeRelativeToCurrentDirectory)

cutModule :: CutModule
cutModule = CutModule
  { mName = "glob"
  , mFunctions = [globFiles]
  }

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
  paths  <- liftIO $ glob ptn
  paths' <- liftIO $ mapM makeRelativeToCurrentDirectory paths
  writeLits cfg ref out'' paths'
  where
    out'  = fromCutPath cfg outPath
    path' = fromCutPath cfg path
    out'' = debugA cfg "aGlobFiles" out' [out', path']
