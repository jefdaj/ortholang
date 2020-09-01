module OrthoLang.Modules.Sample where

-- TODO single sample works, but why doesn't the seed change when repeating??

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Interpreter.Paths (getExprPathSeed)

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
  , mEncodings = []
  , mFunctions = [sample]
  }

sample :: Function
sample = newFnA2 "sample" (Exactly num, la) la aSample [Nondeterministic] -- TODO is that really the right word?
  where
    la = ListSigs $ AnyType "type of the thing to sample"

aSample :: NewAction2
aSample (ExprPath outPath') nPath lstPath = do
  let loc  = "ortholang.modules.sample.aSample"
      seed = fromJust $ getExprPathSeed outPath'
  debugA loc ("seed: " ++ show seed)
  nStr <- readLit loc nPath
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  -- TODO do we need to `need` the path before decoding its type? seems like it would be 'no'
  (ListOf t) <- liftIO $ decodeNewRulesType cfg dRef (ExprPath lstPath) -- TODO convenience fn for this as Action
  -- liftIO $ putStrLn $ "t: " ++ show t
  -- let (ListOf t') = t
  lst  <- readStrings loc t lstPath
  let n         = read $ formatScientific Fixed (Just 0) $ read nStr
      elements' = randomSample seed n lst
  writeStrings loc t outPath' elements'

randomSample :: Seed -> Int -> [String] -> [String]
randomSample (Seed s) n lst = take n $ shuffle lst randGen
  where
    shuffle xs = shuffle' xs $ length xs
    -- according to the docs, and string is OK as a random seed
    randGen = read (show s) :: StdGen
