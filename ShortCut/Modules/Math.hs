module ShortCut.Modules.Math where

import Development.Shake
import ShortCut.Core.Types
import Data.Scientific       (Scientific)
import Data.String.Utils     (strip)
import ShortCut.Core.Compile (cBop)

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
  , fSignature = \_ -> (num, [num, num])
  , fFixity    = Infix
  , fCompiler  = cMath fn
  }

-- apply a math operation to two numbers
-- TODO can a lot of this be moved back into compile while leaving something?
cMath :: (Scientific -> Scientific -> Scientific) -- in this module
      -> CutConfig -> CutExpr -> Rules FilePath   -- in Compile module
cMath fn cfg e@(CutBop extn _ n1 n2) = do
  -- liftIO $ putStrLn "entering cMath"
  (p1, p2, p3) <- cBop cfg extn e (n1, n2)
  p3 %> \out -> do
    num1 <- fmap strip $ readFile' p1
    num2 <- fmap strip $ readFile' p2
    -- putQuiet $ unwords [fnName, p1, p2, p3]
    let num3 = fn (read num1 :: Scientific) (read num2 :: Scientific)
    writeFileChanged out $ show num3 ++ "\n"
  return p3
cMath _ _ _ = error "bad argument to cMath"
