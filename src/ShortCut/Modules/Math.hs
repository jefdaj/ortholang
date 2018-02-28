module ShortCut.Modules.Math where

-- TODO is math ever actually needed in a cut script?

import ShortCut.Core.Types
import Development.Shake

import Data.Scientific             (Scientific, toRealFloat)
import Data.String.Utils           (strip)
import ShortCut.Core.Compile.Basic (rBop, defaultTypeCheck)
import ShortCut.Core.Debug         (debugAction)
import ShortCut.Core.Actions       (readLit, writeLit)

cutModule :: CutModule
cutModule = CutModule
  { mName = "math"
  , mFunctions =
    [ mkMathFn "+" (+)
    , mkMathFn "-" (-)
    , mkMathFn "*" (*)
    , mkMathFn "/" (divDouble)
    ]
  }

-- for some reason division is a lot harder than I expected!
-- TODO is there a more elegant way without converting to string?
divDouble :: Scientific -> Scientific -> Scientific
divDouble n1 n2 = read $ show (answer :: Double)
  where
    answer = toRealFloat n1 / toRealFloat n2

mkMathFn :: String -> (Scientific -> Scientific -> Scientific) -> CutFunction
mkMathFn name fn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, num] num
  , fFixity    = Infix
  , fRules     = rMath fn
  }

-- apply a math operation to two numbers
-- TODO can a lot of this be moved back into compile while leaving something?
rMath :: (Scientific -> Scientific -> Scientific) -- in this module
      -> CutState -> CutExpr -> Rules ExprPath    -- in Compile module
rMath fn s@(_,cfg,_) e@(CutBop _ _ _ _ n1 n2) = do
  -- liftIO $ putStrLn "entering rMath"
  (ExprPath p1, ExprPath p2, ExprPath p3) <- rBop s e (n1, n2)
  p3 %> aMath cfg fn p1 p2
  return (ExprPath p3)
rMath _ _ _ = error "bad argument to rMath"

aMath :: CutConfig -> (Scientific -> Scientific -> Scientific)
      -> FilePath -> FilePath -> FilePath -> Action ()
aMath cfg fn p1 p2 out = do
    need [p1, p2]
    num1 <- fmap strip $ readLit cfg p1
    num2 <- fmap strip $ readLit cfg p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    let out' = debugAction cfg "aMath" out [p1, p2, out]
    writeLit cfg out' $ show num3
