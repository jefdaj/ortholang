module OrthoLang.Modules.Math where

-- TODO aha! the bug is that the list of [n1, n2] is never needed by the bop somehow

import OrthoLang.Core.Types

import Data.Scientific        (Scientific, toRealFloat)
import OrthoLang.Core.Actions (readLits, writeLit)
import OrthoLang.Core.Compile (mkNewFn1)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "Math"
  , mDesc = "Basic math"
  , mTypes = [num]
  , mFunctions =
    [ mkMathFn '+' "add"      (+)
    , mkMathFn '-' "subtract" (-)
    , mkMathFn '*' "multiply" (*)
    , mkMathFn '/' "divide"   (divDouble)
    ]
  }

-- for some reason division is a lot harder than I expected!
-- TODO is there a more elegant way without converting to string?
divDouble :: Scientific -> Scientific -> Scientific
divDouble n1 n2 = read $ show (answer :: Double)
  where
    answer = toRealFloat n1 / toRealFloat n2

mkMathFn :: Char -> String -> (Scientific -> Scientific -> Scientific) -> OrthoLangFunction
mkMathFn opChar name fn = mkNewFn1 name (Just opChar) num [ListOf num] $ aMath fn

aMath :: (Scientific -> Scientific -> Scientific) -> NewAction1
aMath fn cfg lRef _ (ExprPath out) a1 = do
  inputs <- readLits cfg lRef a1
  let result = foldl1 fn $ map (\n -> read n :: Scientific) inputs
  writeLit cfg lRef out $ show result
