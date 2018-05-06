module ShortCut.Modules.Sample where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic  (rExpr)
import ShortCut.Core.Paths (exprPath, toCutPath, fromCutPath)
import ShortCut.Core.Actions (readLit, readStrings, writeStrings)
import Data.Scientific

cutModule :: CutModule
cutModule = CutModule
  { mName = "sample"
  , mFunctions = [sample]
  }

sample :: CutFunction
sample = CutFunction
  { fName      = name 
  , fFixity    = Prefix
  , fTypeCheck = tSample
  , fTypeDesc  = name ++ " : <whatever>.list -> <whatever>.list"
  , fRules     = rSample
  }
  where
    name = "sample"

tSample :: [CutType] -> Either String CutType
tSample [n, ListOf x] | n == num = Right $ ListOf x
tSample _ = Left "sample requires a num and a list"

rSample :: RulesFn
rSample st@(_, cfg, ref) expr@(CutFun _ salt _ _ [n, lst]) = do
  (ExprPath nPath' ) <- rExpr st n
  (ExprPath inPath') <- rExpr st lst
  let nPath    = toCutPath cfg nPath'
      inPath   = toCutPath cfg inPath'
      outPath  = exprPath st expr
      outPath' = fromCutPath cfg outPath
      (ListOf t) = typeOf lst
  outPath' %> \_ -> aSample (randomSample salt) t cfg ref outPath nPath inPath
  return $ ExprPath outPath'
rSample _ _ = error "bad argument to rSample"

aSample :: (Int -> [String] -> [String]) -> CutType -> Action2
aSample fn t cfg ref outPath nPath lstPath = do
  let nPath'   = fromCutPath cfg nPath
      lstPath' = fromCutPath cfg lstPath
      outPath' = fromCutPath cfg outPath
  nStr <- readLit cfg ref nPath'
  lst  <- readStrings t cfg ref lstPath'
  let n         = read $ formatScientific Fixed (Just 0) $ read nStr
      elements' = fn n lst
  writeStrings t cfg ref outPath' elements'

-- TODO actually sample lol
randomSample :: Int -> Int -> [String] -> [String]
randomSample seed n lst = take n lst
