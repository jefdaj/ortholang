module OrthoLang.Modules.Math where

-- TODO is math ever actually needed in a cut script?

import OrthoLang.Core.Types
import Development.Shake

import Data.Scientific             (Scientific, toRealFloat)
import Data.String.Utils           (strip)
import OrthoLang.Core.Compile.Basic (rBop, defaultTypeCheck)
-- import OrthoLang.Core.Debug         (traceA)
import OrthoLang.Core.Actions       (readLit, writeLit, traceA, need')

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "Math"
  , mDesc = "Basic math"
  , mTypes = [] -- TODO include num?
  , mFunctions =
    [ mkMathFn '+' "plus"   (+)
    , mkMathFn '-' "minus"  (-)
    , mkMathFn '*' "times"  (*)
    , mkMathFn '/' "divide" (divDouble)
    ]
  }

-- for some reason division is a lot harder than I expected!
-- TODO is there a more elegant way without converting to string?
divDouble :: Scientific -> Scientific -> Scientific
divDouble n1 n2 = read $ show (answer :: Double)
  where
    answer = toRealFloat n1 / toRealFloat n2

mkMathFn :: Char -> String -> (Scientific -> Scientific -> Scientific) -> OrthoLangFunction
mkMathFn opChar opName fn = OrthoLangFunction
  { fNames     = [[opChar], opName]
  , fTypeCheck = defaultTypeCheck    [num, num] num
  , fTypeDesc  = mkTypeDesc [opChar] [num, num] num
  , fFixity    = Infix
  , fRules     = rMath fn
  }

-- apply a math operation to two numbers
-- TODO can a lot of this be moved back into compile while leaving something?
rMath :: (Scientific -> Scientific -> Scientific) -- in this module
      -> OrthoLangState -> OrthoLangExpr -> Rules ExprPath    -- in Compile module
rMath fn s@(_, cfg, ref, _) e@(OrthoLangBop _ _ _ _ n1 n2) = do
  -- liftIO $ putStrLn "entering rMath"
  (ExprPath p1, ExprPath p2, ExprPath p3) <- rBop s e (n1, n2)
  p3 %> aMath cfg ref fn p1 p2
  return (ExprPath p3)
rMath _ _ _ = fail "bad argument to rMath"

aMath :: OrthoLangConfig -> Locks -> (Scientific -> Scientific -> Scientific)
      -> FilePath -> FilePath -> FilePath -> Action ()
aMath cfg ref fn p1 p2 out = do
    need' cfg ref "ortholang.modules.math.aMath" [p1, p2]
    num1 <- fmap strip $ readLit cfg ref p1
    num2 <- fmap strip $ readLit cfg ref p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    let out' = traceA "aMath" out [p1, p2, out]
    writeLit cfg ref out' $ show num3
