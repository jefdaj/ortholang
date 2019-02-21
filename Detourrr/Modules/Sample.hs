module Detourrr.Modules.Sample where

-- TODO single sample works, but why doesn't the salt change when repeating??

import Development.Shake
import Detourrr.Core.Types
import Detourrr.Core.Compile.Basic  (rExpr)
import Detourrr.Core.Paths (exprPath, toRrrPath, fromRrrPath)
import Detourrr.Core.Actions (readLit, readStrings, writeStrings, debugL)
import Data.Scientific
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "Sample"
  , mDesc = "Random (but reproducable) sampling of list elements.\n\n\
            \WARNING: Because of the way Detourrr caches tempfiles, calling these\n\
            \more than once will give the same sublist each time! For different\n\
            \sublists, use in combination with the 'repeat' function"
  , mTypes = []
  , mFunctions = [sample]
  }

sample :: RrrFunction
sample = RrrFunction
  { fName      = name 
  , fFixity    = Prefix
  , fTypeCheck = tSample
  , fRules     = rSample
  , fTypeDesc  = name ++ " : num X.list -> X.list"
  , fDesc = Just "Take a random sample from a list. Can be used to test your\n\
                 \algorithm on a smaller set of genes/genomes, or as the 'permute'\n\
                 \step in the permute, repeat, summarize (PRS) pattern."
  }
  where
    name = "sample"

tSample :: [RrrType] -> Either String RrrType
tSample [n, ListOf x] | n == num = Right $ ListOf x
tSample _ = Left "sample requires a num and a list"

rSample :: RulesFn
rSample st@(_, cfg, ref, ids) expr@(RrrFun _ salt _ _ [n, lst]) = do
  (ExprPath nPath' ) <- rExpr st n
  (ExprPath inPath') <- rExpr st lst
  let nPath    = toRrrPath cfg nPath'
      inPath   = toRrrPath cfg inPath'
      outPath  = exprPath st expr
      outPath' = fromRrrPath cfg outPath
      (ListOf t) = typeOf lst
  outPath' %> \_ -> aSample salt t cfg ref ids outPath nPath inPath
  return $ ExprPath outPath'
rSample _ _ = error "bad argument to rSample"

aSample :: Int -> RrrType -> Action2
aSample salt t cfg ref _ outPath nPath lstPath = do
  let nPath'   = fromRrrPath cfg nPath
      lstPath' = fromRrrPath cfg lstPath
      outPath' = fromRrrPath cfg outPath
  nStr <- readLit cfg ref nPath'
  lst  <- readStrings t cfg ref lstPath'
  debugL cfg $ "aSample salt: " ++ show salt
  let n         = read $ formatScientific Fixed (Just 0) $ read nStr
      elements' = randomSample salt n lst
  writeStrings t cfg ref outPath' elements'

randomSample :: Int -> Int -> [String] -> [String]
randomSample seed n lst = take n $ shuffle lst gen
  where
    shuffle xs = shuffle' xs $ length xs
    gen = mkStdGen seed