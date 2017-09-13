module ShortCut.Modules.Math where

-- TODO is math ever actually needed in a cut script?

import ShortCut.Core.Types
import Development.Shake

import Data.Scientific       (Scientific)
import Data.String.Utils     (strip)
import ShortCut.Core.Compile.Rules (rBop, defaultTypeCheck)

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
  , fRules  = rMath fn
  }

-- apply a math operation to two numbers
-- TODO can a lot of this be moved back into compile while leaving something?
rMath :: (Scientific -> Scientific -> Scientific) -- in this module
      -> CutState -> CutExpr -> Rules ExprPath    -- in Compile module
rMath fn s e@(CutBop extn _ _ _ n1 n2) = do
  -- liftIO $ putStrLn "entering rMath"
  (ExprPath p1, ExprPath p2, ExprPath p3) <- rBop s extn e (n1, n2)
  p3 %> aMath fn p1 p2
  return (ExprPath p3)
rMath _ _ _ = error "bad argument to rMath"

aMath :: (Scientific -> Scientific -> Scientific)
      -> FilePath -> FilePath -> FilePath -> Action ()
aMath fn p1 p2 out = do
    need [p1, p2]
    num1 <- fmap strip $ readFile' p1
    num2 <- fmap strip $ readFile' p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    writeFileChanged out $ show num3 ++ "\n"
