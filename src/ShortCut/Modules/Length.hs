module ShortCut.Modules.Length where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions  (readPaths, writeLit)
import ShortCut.Core.Debug    (debugAction)
import ShortCut.Core.Paths    (exprPath, fromCutPath,
                               toCutPath, CutPath)
import ShortCut.Core.Compile.Basic     (rExpr)
import ShortCut.Core.Compile.Each     (rEach)
import ShortCut.Modules.Blast  (bht)
import ShortCut.Modules.BlastCRB (crb)
import System.FilePath         (takeDirectory)
import System.Directory           (createDirectoryIfMissing)
import Data.Scientific (Scientific())
-- import Path (fromAbsFile, fromAbsDir)

cutModule :: CutModule
cutModule = CutModule {mName = "length", mFunctions = [len, lenEach]}

-- can't name it length because that's a standard Haskell function
len :: CutFunction
len = CutFunction
  { fName      = "length"
  , fTypeCheck = tLen
  , fFixity    = Prefix
  , fRules  = rLen
  }

lenEach :: CutFunction
lenEach = CutFunction
  { fName      = "length_each"
  , fTypeCheck = tLenEach
  , fFixity    = Prefix
  , fRules  = rEach aLen
  }

tLen :: [CutType] -> Either String CutType
tLen [Empty ] = Right num
tLen [(ListOf _)] = Right num
tLen [x] | x == bht = Right num
tLen _ = Left $ "length requires a list"

rLen :: CutState -> CutExpr -> Rules ExprPath
rLen s@(_,cfg) e@(CutFun _ _ _ _ [l]) = do
  (ExprPath lPath) <- rExpr s l
  -- TODO once all modules are converted, add back phantom types!
  -- let relPath = makeRelative (cfgTmpDir cfg) lPath
  -- (ExprPath outPath) = exprPathExplicit cfg True num "length" [relPath]
  let outPath = exprPath s e
      out'    = fromCutPath cfg outPath
      lPath'  = toCutPath   cfg lPath
  out' %> \_ -> aLen cfg [outPath, lPath']
  return (ExprPath out')
rLen _ _ = error "bad arguments to rLen"

tLenEach :: [CutType] -> Either String CutType
tLenEach [ ListOf  Empty     ] = Right (ListOf num) -- specifically, []
tLenEach [(ListOf (ListOf _))] = Right (ListOf num)
tLenEach [ListOf  x] | x `elem` [bht, crb] = Right (ListOf num)
tLenEach _ = Left $ "length_each requires a list of things with lengths"

-- TODO if given a list with empty lists, should return zeros!
aLen :: CutConfig -> [CutPath] -> Action ()
aLen cfg [out, lst] = do
  let count ls = read (show $ length ls) :: Scientific
  n <- fmap count $ readPaths cfg lst'
  liftIO $ createDirectoryIfMissing True $ takeDirectory out'
  -- liftIO $ putStrLn $ "length of " ++ lst' ++ " is " ++ show n
  writeLit cfg out'' $ show n
  where
    out'  = fromCutPath cfg out
    lst'  = fromCutPath cfg lst
    out'' = debugAction cfg "aLen" out' [out', lst']
aLen _ args = error $ "bad arguments to aLen: " ++ show args
