module ShortCut.Modules.Length where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile (cExpr)
import ShortCut.Core.Debug   (debugReadLines, debugWriteFile)
import ShortCut.Core.Paths   (exprPath)
import ShortCut.Core.ModuleAPI (rMapLastTmp)

cutModule :: CutModule
cutModule = CutModule {mName = "length", mFunctions = [len, lenEach]}

-- can't name it length because that's a standard Haskell function
len :: CutFunction
len = CutFunction
  { fName      = "length"
  , fTypeCheck = tLen
  , fFixity    = Prefix
  , fCompiler  = cLen
  }

lenEach :: CutFunction
lenEach = CutFunction
  { fName      = "length_each"
  , fTypeCheck = tLenEach
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmp aLen "length_each" (ListOf num)
  }

tLen :: [CutType] -> Either String CutType
tLen [EmptyList ] = Right num
tLen [(ListOf _)] = Right num
tLen _ = Left $ "length requires a list"

cLen :: CutState -> CutExpr -> Rules ExprPath
cLen s@(_,cfg) e@(CutFun _ _ _ _ [l]) = do
  lp@(ExprPath lPath) <- cExpr s l
  let (ExprPath outPath) = exprPath cfg e [lp]
  outPath %> \_ -> do
    n <- fmap length $ debugReadLines cfg lPath
    debugWriteFile cfg outPath (show n ++ "\n") -- TODO auto-add the \n?
  return (ExprPath outPath)
cLen _ _ = error "bad arguments to cLen"

tLenEach :: [CutType] -> Either String CutType
tLenEach [EmptyList ] = Right (ListOf num)
tLenEach [(ListOf (ListOf _))] = Right (ListOf num)
tLenEach _ = Left $ "length_each requires a list of lists"

aLen :: CutConfig -> CacheDir -> [ExprPath] -> Action ()
aLen cfg _ [ExprPath out, ExprPath lst] = do
  n <- fmap length $ debugReadLines cfg lst
  debugWriteFile cfg out (show n ++ "\n") -- TODO auto-add the \n?
aLen _ _ args = error $ "bad arguments to aLen: " ++ show args
