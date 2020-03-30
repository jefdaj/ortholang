module OrthoLang.Modules.Math where

import Prelude hiding (log)
import OrthoLang.Core.Types
import qualified OrthoLang.Core.Util as U

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans    (lift)
import Data.Scientific        (Scientific, toRealFloat)
import OrthoLang.Core.Actions (readLits, writeLit)
import OrthoLang.Core.Compile (mkNewFn1)

orthoLangModule :: Module
orthoLangModule = Module
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

divDouble :: Scientific -> Scientific -> Scientific
divDouble n1 n2 = read $ show (answer :: Double)
  where
    answer = toRealFloat n1 / toRealFloat n2

mkMathFn :: Char -> String -> (Scientific -> Scientific -> Scientific) -> Function
mkMathFn opChar name fn = mkNewFn1 name (Just opChar) num [ListOf num] $ aMath fn

log :: Show a => a -> ActionR ()
log = lift . liftIO . U.debug "modules.math.amath" . show

aMath :: (Scientific -> Scientific -> Scientific) -> ActionR1
aMath mathFn (ExprPath outPath) nsPath = do
  log nsPath
  cfg  <- askConfig
  lRef <- askLocks
  ns <- lift $ readLits cfg lRef nsPath
  log ns
  let n = foldl1 mathFn $ map (read :: String -> Scientific) ns
  lift $ writeLit cfg lRef outPath $ show n
  log n
