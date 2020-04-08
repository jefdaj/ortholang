module OrthoLang.Modules.Math where

import Development.Shake
import Prelude hiding (log)
import OrthoLang.Debug
import OrthoLang.Core
import qualified OrthoLang.Util as U

import Control.Monad.IO.Class (liftIO)
import Data.Scientific        (Scientific, toRealFloat)

olModule :: Module
olModule = Module
  { mName = "Math"
  , mDesc = "Basic math"
  , mTypes = [num]
  , mGroups = []
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
mkMathBop name opChar fn = newBop name opChar num num $ aMathBop fn

log' :: Show a => a -> Action ()
log' = liftIO . debug "modules.math.amath" . show

aMathBop :: (Scientific -> Scientific -> Scientific) -> NewAction1
aMathBop mathFn (ExprPath outPath) nsPath = do
  log' nsPath
  ns <- readLits nsPath
  log' ns
  let n = foldl1 mathFn $ map (read :: String -> Scientific) ns
  writeLit outPath $ show n
  log' n
