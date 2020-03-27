module OrthoLang.Modules.Sample where

-- TODO single sample works, but why doesn't the salt change when repeating??

import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Compile  (rExpr)
import OrthoLang.Core.Paths (exprPath, toPath, fromPath)
import OrthoLang.Core.Actions (readLit, readStrings, writeStrings, debugA)
import Data.Scientific
import System.Random (StdGen)
import System.Random.Shuffle (shuffle')

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "Sample"
  , mDesc = "Random (but reproducable) sampling of list elements.\n\n\
            \WARNING: Because of the way OrthoLang caches tempfiles, calling these\n\
            \more than once will give the same sublist each time! For different\n\
            \sublists, use in combination with the 'repeat' function"
  , mTypes = []
  , mFunctions = [sample]
  }

sample :: Function
sample = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = tSample
  , fNewRules = Nothing, fOldRules = rSample
  , fTypeDesc  = name ++ " : num X.list -> X.list"
  }
  where
    name = "sample"

tSample :: [Type] -> Either String Type
tSample [n, ListOf x] | n == num = Right $ ListOf x
tSample _ = Left "sample requires a num and a list"

rSample :: RulesFn
rSample st@(_, cfg, ref, ids) expr@(Fun _ salt _ _ [n, lst]) = do
  (ExprPath nPath' ) <- rExpr st n
  (ExprPath inPath') <- rExpr st lst
  let nPath    = toPath cfg nPath'
      inPath   = toPath cfg inPath'
      outPath  = exprPath st expr
      outPath' = fromPath cfg outPath
      (ListOf t) = typeOf lst
  outPath' %> \_ -> aSample salt t cfg ref ids outPath nPath inPath
  return $ ExprPath outPath'
rSample _ _ = fail "bad argument to rSample"

aSample :: Salt -> Type -> Action2
aSample salt t cfg ref _ outPath nPath lstPath = do
  let nPath'   = fromPath cfg nPath
      lstPath' = fromPath cfg lstPath
      outPath' = fromPath cfg outPath
  nStr <- readLit cfg ref nPath'
  lst  <- readStrings t cfg ref lstPath'
  debugA ("ortholang.modules.sample.aSample") ("salt: " ++ show salt)
  let n         = read $ formatScientific Fixed (Just 0) $ read nStr
      elements' = randomSample salt n lst
  writeStrings t cfg ref outPath' elements'

randomSample :: Salt -> Int -> [String] -> [String]
randomSample (Salt s) n lst = take n $ shuffle lst randGen
  where
    shuffle xs = shuffle' xs $ length xs
    -- according to the docs, and string is OK as a random seed
    randGen = read (show s) :: StdGen
