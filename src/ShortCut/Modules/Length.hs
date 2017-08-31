module ShortCut.Modules.Length where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile (cExpr)
import ShortCut.Core.Debug   (debugReadLines, debugWriteFile)
import ShortCut.Core.Paths   (exprPath)

cutModule :: CutModule
cutModule = CutModule {mName = "length", mFunctions = [len]}

-- can't name it length because that's a standard Haskell function
len :: CutFunction
len = CutFunction
  { fName      = "length"
  , fTypeCheck = tLen
  , fFixity    = Prefix
  , fCompiler  = cLen
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