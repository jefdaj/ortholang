module OrthoLang.Modules.Sample where

-- TODO single sample works, but why doesn't the salt change when repeating??

import Development.Shake
import OrthoLang.Core

import Data.Scientific       (formatScientific, FPFormat(..))
import System.Random         (StdGen)
import System.Random.Shuffle (shuffle')
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "Sample"
  , mDesc = "Random (but reproducable) sampling of list elements.\n\n\
            \WARNING: Because of the way OrthoLang caches tempfiles, calling these\n\
            \more than once will give the same sublist each time! For different\n\
            \sublists, use in combination with the 'repeat' function"
  , mTypes = []
  , mGroups = []
  , mFunctions = [sample]
  }

sample :: Function
sample = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  -- , fTypeCheck = tSample
  -- , fTypeDesc  = name ++ " : num X.list -> X.list"
  , fInputs = [Exactly num, ListSigs (AnyType "any type")]
  , fOutput =  ListSigs (AnyType "any type")
  , fOldRules = rSample
  , fNewRules = NewNotImplemented
  }
  where
    name = "sample"

-- (num, ListOf (Some ot "any type")) (ListOf ot "any type")
-- shown as "num t.list -> t.list, where t is any type"
tSample :: [Type] -> Either String Type
tSample [n, ListOf x] | n == num = Right $ ListOf x
tSample _ = Left "sample requires a num and a list"

rSample :: RulesFn
rSample scr expr@(Fun _ salt _ _ [n, lst]) = do
  (ExprPath nPath' ) <- rExpr scr n
  (ExprPath inPath') <- rExpr scr lst
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let nPath    = toPath cfg nPath'
      inPath   = toPath cfg inPath'
      outPath  = exprPath cfg dRef scr expr
      outPath' = fromPath cfg outPath
      (ListOf t) = typeOf lst
  outPath' %> \_ -> aSample salt t outPath nPath inPath
  return $ ExprPath outPath'
rSample _ _ = fail "bad argument to rSample"

aSample :: Salt -> Type -> Action2
aSample salt t outPath nPath lstPath = do
  cfg <- fmap fromJust getShakeExtra
  let nPath'   = fromPath cfg nPath
      lstPath' = fromPath cfg lstPath
      outPath' = fromPath cfg outPath
  nStr <- readLit nPath'
  lst  <- readStrings t lstPath'
  debugA ("ortholang.modules.sample.aSample") ("salt: " ++ show salt)
  let n         = read $ formatScientific Fixed (Just 0) $ read nStr
      elements' = randomSample salt n lst
  writeStrings t outPath' elements'

randomSample :: Salt -> Int -> [String] -> [String]
randomSample (Salt s) n lst = take n $ shuffle lst randGen
  where
    shuffle xs = shuffle' xs $ length xs
    -- according to the docs, and string is OK as a random seed
    randGen = read (show s) :: StdGen
