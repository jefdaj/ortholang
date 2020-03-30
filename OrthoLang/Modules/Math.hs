module OrthoLang.Modules.Math where

import Prelude hiding (log)
import OrthoLang.Core.Types
import qualified OrthoLang.Core.Util as U

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans    (lift)
import Data.Scientific        (Scientific, toRealFloat)
import OrthoLang.Core.Actions (readLits, writeLit)
import OrthoLang.Core.Compile (mkNewBop)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "Math"
  , mDesc = "Basic math"
  , mTypes = [num]
  , mFunctions =
    [ mkMathBop "add"      '+' (+)
    , mkMathBop "subtract" '-' (-)
    , mkMathBop "multiply" '*' (*)
    , mkMathBop "divide"   '/' (divDouble)
    ]
  }

divDouble :: Scientific -> Scientific -> Scientific
divDouble n1 n2 = read $ show (answer :: Double)
  where
    answer = toRealFloat n1 / toRealFloat n2

mkMathBop :: String -> Char -> (Scientific -> Scientific -> Scientific) -> Function
mkMathBop name opChar fn = mkNewBop name opChar num num $ aMathBop fn

log :: Show a => a -> ActionR ()
log = lift . liftIO . U.debug "modules.math.amath" . show

aMathBop :: (Scientific -> Scientific -> Scientific) -> ActionR1
aMathBop mathFn (ExprPath outPath) nsPath = do
  log nsPath
  cfg  <- askConfig
  lRef <- askLocks
  ns <- lift $ readLits cfg lRef nsPath
  log ns
  let n = foldl1 mathFn $ map (read :: String -> Scientific) ns
  lift $ writeLit cfg lRef outPath $ show n
  log n
