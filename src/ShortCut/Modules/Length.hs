module ShortCut.Modules.Length where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Debug     (debugReadLines, debugWriteFile, debugAction)
import ShortCut.Core.Compile.Paths2    (cacheDir2, tmpToExpr)
import ShortCut.Core.Compile.Rules     (rExpr, rMapLastTmp)
import ShortCut.Modules.Blast  (bht)
import System.FilePath         (takeDirectory, (</>))
import System.Directory           (createDirectoryIfMissing)
import Data.Scientific (Scientific())
import Path (fromAbsFile, fromAbsDir)

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
  , fRules  = rMapLastTmp aLen "length_each" num
  }

tLen :: [CutType] -> Either String CutType
tLen [EmptyList ] = Right num
tLen [(ListOf _)] = Right num
tLen [x] | x == bht = Right num
tLen _ = Left $ "length requires a list"

rLen :: CutState -> CutExpr -> Rules ExprPath
rLen s@(_,cfg) e@(CutFun _ _ _ _ [l]) = do
  (ExprPath lPath) <- rExpr s l
  -- TODO once all modules are converted, add back phantom types!
  -- let relPath = makeRelative (cfgTmpDir cfg) lPath
  -- (ExprPath outPath) = exprPathExplicit cfg True num "length" [relPath]
  let cDir    = fromAbsDir  $ cacheDir2 cfg "length"
      outPath = fromAbsFile $ tmpToExpr s e
      out = cfgTmpDir cfg </> outPath
  out %> \_ -> aLen cfg (CacheDir cDir) [ExprPath out, ExprPath lPath]
  return (ExprPath out)
rLen _ _ = error "bad arguments to rLen"

tLenEach :: [CutType] -> Either String CutType
tLenEach [EmptyList          ] = Right (ListOf num)
tLenEach [(ListOf (ListOf _))] = Right (ListOf num)
tLenEach [ListOf x] | x == bht = Right (ListOf num) -- TODO also crb?
tLenEach _ = Left $ "length_each requires a list of lists"

aLen :: CutConfig -> CacheDir -> [ExprPath] -> Action ()
aLen cfg _ [ExprPath out, ExprPath lst] = do
  n <- fmap (\n -> read n :: Scientific)
     $ fmap (show . length)
     $ debugReadLines cfg lst
  let out' = debugAction cfg "aLen" out [out, lst]
  liftIO $ createDirectoryIfMissing True $ takeDirectory out
  debugWriteFile cfg out' (show n ++ "\n") -- TODO auto-add the \n?
aLen _ _ args = error $ "bad arguments to aLen: " ++ show args
