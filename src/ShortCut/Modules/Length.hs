module ShortCut.Modules.Length where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Debug     (debugReadLines, debugWriteFile)
import ShortCut.Core.Compile.Paths     (cacheDir, exprPathExplicit)
import ShortCut.Core.Compile.Rules     (rExpr, rMapLastTmp)
import ShortCut.Modules.Blast  (bht)
import System.FilePath         (makeRelative)

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
  , fRules  = rMapLastTmp aLen "length_each" (SetOf num)
  }

tLen :: [CutType] -> Either String CutType
tLen [EmptySet ] = Right num
tLen [(SetOf _)] = Right num
tLen [x] | x == bht = Right num
tLen _ = Left $ "length requires a list"

rLen :: CutState -> CutExpr -> Rules ExprPath
rLen s@(_,cfg) (CutFun _ _ _ _ [l]) = do
  (ExprPath lPath) <- rExpr s l
  let relPath = makeRelative (cfgTmpDir cfg) lPath
      cDir = cacheDir cfg "length"
      (ExprPath outPath) = exprPathExplicit cfg True (typeOf l) "length" [relPath]
  outPath %> \_ -> aLen cfg cDir [ExprPath outPath, ExprPath lPath]
  return (ExprPath outPath)
rLen _ _ = error "bad arguments to rLen"

tLenEach :: [CutType] -> Either String CutType
tLenEach [EmptySet          ] = Right (SetOf num)
tLenEach [(SetOf (SetOf _))] = Right (SetOf num)
tLenEach [SetOf x] | x == bht = Right (SetOf num) -- TODO also crb?
tLenEach _ = Left $ "length_each requires a list of lists"

aLen :: CutConfig -> CacheDir -> [ExprPath] -> Action ()
aLen cfg _ [ExprPath out, ExprPath lst] = do
  n <- fmap length $ debugReadLines cfg lst
  debugWriteFile cfg out (show n ++ "\n") -- TODO auto-add the \n?
aLen _ _ args = error $ "bad arguments to aLen: " ++ show args
