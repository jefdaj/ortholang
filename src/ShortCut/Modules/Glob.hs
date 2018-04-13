module ShortCut.Modules.Glob where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic        (rExpr, defaultTypeCheck)
import ShortCut.Core.Actions (readLit, writeLits, debugA)
import ShortCut.Core.Paths (exprPath, CutPath, toCutPath, fromCutPath)
import Data.String.Utils          (strip)

import System.FilePath.Glob       (glob)
import System.Directory (makeRelativeToCurrentDirectory)
-- import ShortCut.Core.Debug        (debugA)

import ShortCut.Modules.SeqIO (fna, loadFnaEach)
-- import ShortCut.Core.Compile.Apply (rApply1)

cutModule :: CutModule
cutModule = CutModule
  { mName = "glob"
  , mFunctions = [globFiles, globFna]
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
  paths  <- liftIO $ glob ptn
  paths' <- liftIO $ mapM makeRelativeToCurrentDirectory paths
  -- toShortCutListStr cfg str (ExprPath outPath) paths
  writeLits cfg ref out'' paths'
  where
    out'  = fromCutPath cfg outPath
    path' = fromCutPath cfg path
    out'' = debugA cfg "aGlobFiles" out' [out', path']

--------------
-- glob_fna --
--------------

{- This is a test of the rApply1 function,
 - to see if I can compose glob_files with load_fna_each
 -}

-- globFna :: CutFunction
-- globFna = CutFunction
--   { fName      = name
--   , fTypeCheck = defaultTypeCheck [str] (ListOf fna)
--   , fTypeDesc  = mkTypeDesc name  [str] (ListOf fna)
--   , fFixity    = Prefix
--   , fRules     = rApply1 globFiles loadFnaEach
--   }
--   where
--     name = "glob_fna"

compose1 :: String -> CutFunction -> CutFunction -> CutType -> TypeChecker -> String -> CutFunction
compose1 name fn1 fn2 type1 typeChecker typeDesc = CutFunction
  { fName = name
  , fTypeCheck = typeChecker
  , fTypeDesc  = typeDesc
  , fFixity    = Infix
  -- , fRules     = rCompose (fRules fn1) (fRules fn2)
  , fRules     = rCompose1 fn1 fn2 type1
  }

-- rCompose :: RulesFn -> RulesFn -> RulesFn
-- rCompose rules1 rules2 st expr = rules2 st expr'
--   where
--     expr' = CutRules (CompiledExpr expr $ rules1 st expr)

rCompose1 :: CutFunction -> CutFunction -> CutType -> RulesFn
rCompose1 fn1 fn2 type1 st (CutFun rtn salt deps name args) = (fRules fn2) st expr2
  where
    expr1'  = CutFun type1 salt deps (fName fn1) args
    expr1'' = CutRules $ CompiledExpr expr1' $ (fRules fn1) st expr1'
    expr2   = CutFun rtn salt deps name [expr1'']
rCompose1 _ _ _ _ _ = error "bad argument to rCompose1"

globFna :: CutFunction
globFna =
  let name = "glob_fna"
  in compose1 name globFiles loadFnaEach (ListOf str)
       (defaultTypeCheck [str] (ListOf fna))
       (mkTypeDesc name  [str] (ListOf fna))
