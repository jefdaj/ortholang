module ShortCut.Modules.Glob where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.ModuleAPI (defaultTypeCheck)
import ShortCut.Core.Compile      (cExpr)
import ShortCut.Core.Paths        (exprPath)
import Data.String.Utils          (strip)

import System.FilePath.Glob       (glob)
import ShortCut.Core.Debug        (debugReadFile, debugWriteLines)
import ShortCut.Core.Util         (absolutize)

cutModule :: CutModule
cutModule = CutModule
  { mName = "glob"
  , mFunctions = [globFiles]
  }

globFiles :: CutFunction
globFiles = CutFunction
  { fName      = "glob_files"
  , fTypeCheck = defaultTypeCheck [str] (SetOf str)
  , fFixity    = Prefix
  , fCompiler  = cGlobFiles
  }

-- TODO ok first part looks good, but now need another step?
-- 1. user gives glob as a str
-- 2. compiler saves that to (ExprPath path)
-- 3. this fn needs path, then reads it to ptn
-- 4. this fn does the actual globbing, creating paths
-- 5. toShortCutSetStr puts them in ShortCut literal format
--    (should use str rather than elemRtnType tho)
-- ... looks like this is actually cGlobFiles!
-- now just need to hook it up to other types: load_faa_all etc.
cGlobFiles :: CutState -> CutExpr -> Rules ExprPath
cGlobFiles s@(_,cfg) e@(CutFun _ _ _ _ [p]) = do
  (ExprPath path) <- cExpr s p
  let (ExprPath outPath) = exprPath cfg True e []
  outPath %> \_ -> aGlobFiles cfg outPath path
  return (ExprPath outPath)
cGlobFiles _ _ = error "bad arguments to cGlobFiles"

aGlobFiles :: CutConfig -> FilePath -> FilePath -> Action ()
aGlobFiles cfg outPath path = do
  ptn   <- fmap strip $ debugReadFile cfg path
  -- liftIO $ putStrLn $ "ptn: " ++ show ptn
  paths <- liftIO $ mapM absolutize =<< glob ptn
  -- toShortCutSetStr cfg str (ExprPath outPath) paths
  debugWriteLines cfg outPath paths
