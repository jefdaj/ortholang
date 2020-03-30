module OrthoLang.Modules.Math where

import OrthoLang.Core.Types
import qualified OrthoLang.Core.Util as U

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans        (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Scientific            (Scientific, toRealFloat)
import OrthoLang.Core.Actions     (readLits, writeLit, need')
import OrthoLang.Core.Compile     (mkNewFn1)

debugShow :: Show a => a -> ActionR ()
debugShow thing = lift $ liftIO $ U.debug "modules.math.amath" $ show thing

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

-- for some reason division is a lot harder than I expected!
-- TODO is there a more elegant way without converting to string?
divDouble :: Scientific -> Scientific -> Scientific
divDouble n1 n2 = read $ show (answer :: Double)
  where
    answer = toRealFloat n1 / toRealFloat n2

mkMathFn :: Char -> String -> (Scientific -> Scientific -> Scientific) -> Function
mkMathFn opChar name fn = mkNewFn1 name (Just opChar) num [ListOf num] $ aMath fn

aMath :: (Scientific -> Scientific -> Scientific) -> ActionR1
aMath fn (ExprPath out) a1 = do
  debugShow a1
  (cfg, lRef, _, _) <- ask
  -- lift $ need' cfg lRef "test" [a1]
  inputs <- lift $ readLits cfg lRef a1
  debugShow inputs
  let result = foldl1 fn $ map (\n -> read n :: Scientific) inputs
  lift $ writeLit cfg lRef out $ show result
  debugShow result
