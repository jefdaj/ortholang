module ShortCut.Modules.Math where

-- TODO is math ever actually needed in a cut script?

import Data.Scientific       (Scientific)
import Data.String.Utils     (strip)
import Development.Shake
import ShortCut.Core.Compile (cBop)
import ShortCut.Core.ModuleAPI (defaultTypeCheck)
import ShortCut.Core.Types

cutModule :: CutModule
cutModule = CutModule
  { mName = "math"
  , mFunctions =
    [ mkMathFn "+" (+)
    , mkMathFn "-" (-)
    , mkMathFn "*" (*)
    , mkMathFn "/" (/)
    ]
  }

mkMathFn :: String -> (Scientific -> Scientific -> Scientific) -> CutFunction
mkMathFn name fn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, num] num
  , fFixity    = Infix
  , fCompiler  = cMath fn
  }

-- apply a math operation to two numbers
-- TODO can a lot of this be moved back into compile while leaving something?
cMath :: (Scientific -> Scientific -> Scientific) -- in this module
      -> CutState -> CutExpr -> Rules ExprPath    -- in Compile module
cMath fn s e@(CutBop extn _ _ _ n1 n2) = do
  -- liftIO $ putStrLn "entering cMath"
  (ExprPath p1, ExprPath p2, ExprPath p3) <- cBop s extn e (n1, n2)
  p3 %> \out -> do
    num1 <- fmap strip $ readFile' p1
    num2 <- fmap strip $ readFile' p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    writeFileChanged out $ show num3 ++ "\n"
  return (ExprPath p3)
cMath _ _ _ = error "bad argument to cMath"
