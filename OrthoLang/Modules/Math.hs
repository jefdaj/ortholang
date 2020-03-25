module OrthoLang.Modules.Math where

-- TODO is math ever actually needed in a cut script?

import OrthoLang.Core.Types
import Development.Shake

import Data.Scientific             (Scientific, toRealFloat)
import Data.String.Utils           (strip)
import OrthoLang.Core.Compile (defaultTypeCheck, rExpr, mkNewFn1)
-- import OrthoLang.Core.Debug         (traceA)
import OrthoLang.Core.Actions       (readLits, writeLit, traceA, need')
import OrthoLang.Core.Paths (exprPath, fromOrthoLangPath)

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
--   { fName = name
--   , fOpChar    = Just opChar
--   , fTypeCheck = defaultTypeCheck [ListOf num] num
--   , fTypeDesc  = mkTypeDesc name  [ListOf num] num
--   , fTags = []
--   , fNewRules = Just $ aMath fn
--   , fOldRules = undefined --rMath fn
--   }

-- apply a math operation to two numbers
-- TODO can a lot of this be moved back into compile while leaving something?
-- rMath :: (Scientific -> Scientific -> Scientific) -- in this module
--       -> RulesFn    -- in Compile module
-- rMath fn s@(_, cfg, ref, _) e@(OrthoLangFun _ _ _ _ [ns]) = do -- TODO this needs to be a list too
--   -- liftIO $ putStrLn "entering rMath"
--   -- (ExprPath p1, ExprPath p2, ExprPath p3) <- rBop s e (e1, e2)
--   (ExprPath nsPath) <- rExpr s ns
--   -- (ExprPath p2) <- rExpr s e2
--   let out' = fromOrthoLangPath cfg $ exprPath s e
--   out' %> aMath cfg ref fn nsPath
--   return (ExprPath p3)
-- rMath _ _ _ = fail "bad argument to rMath"

-- TODO this has to work on a list of scientifics now
aMath :: (Scientific -> Scientific -> Scientific) -> NewAction1
aMath fn cfg lRef iRef (ExprPath out) a1 = do
  -- need' cfg ref "ortholang.modules.math.aMath" [p1, p2]
  nums <- readLits cfg lRef a1
  -- num1 <- fmap strip $ readLit cfg ref p1
  -- num2 <- fmap strip $ readLit cfg ref p2
  -- putQuiet $ unwords [fnName, p1, p2, p3]
  let res  = foldl1 fn $ map (\n -> read n :: Scientific) nums
  -- let out' = traceA "aMath" out [p1, p2, out]
  writeLit cfg lRef out $ show res
