module OrthoLang.Modules.Math where

-- simpler problem? maybe not everything is even getting compiled through rExpr. no it is
-- looks like lits (at least nums) arent being added to the hExprs map
--   need to have the list-needing function (multiply) need the list (works)
--   then the list has to need its elements (not working), which have to have been compiled (not working)

-- simple idea: what if the compilation has to be evaluated strictly to fill IDs?
--   would make sense, but difficult to test without NFData instances
--   if that's it, maybe you should rethink the design and keep the digests in the state directly!
-- also create a NewRulesTest case for this

-- TODO aha! the bug is that the list of [n1, n2] is never needed by the bop somehow
--      it also happens when calling multiply [2, 3.5] directly
--      which means:
--        - it's nothing to do with bop parsing
--        - the bug must be in Compile/NewRules.hs?
--      [2, 3.5] has path: TMPDIR/exprs/list/bd6c542918/0/result digest: 1eb7dcd853
--      multiply [2, 3.5] wants digest: 1eb7dcd853
--      so is multiply just not needing it, or what?
--      hmm, it could be the applyDeps thing not needing the overall list first?
--      aha! no it's the opposite:
--        aNewRules does need' it, but there's no matching pattern
--        why would that be though? it should match rListLits
--      clue: the explicit list hashing uses (ListOf rtn) as type for some reason
--      clue: return type for multiply looks like it's wrongly num.list?
--        nevermind that's the one dep, which is right other than having the wrong hash

import OrthoLang.Core.Types

import Data.Scientific        (Scientific, toRealFloat)
import OrthoLang.Core.Actions (readLits, writeLit, need')
import OrthoLang.Core.Compile (mkNewFn1)
import OrthoLang.Core.Paths   (toPath, fromPath)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans (lift)

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

-- oh this never runs, because shake can't find the input list
aMath :: (Scientific -> Scientific -> Scientific) -> ActionR1
aMath fn (ExprPath out) a1 = do
  liftIO $ putStrLn $ "aMath a1: " ++ show a1
  -- paths <- readPaths cfg lRef a1
  -- liftIO $ putStrLn $ "aMath paths: " ++ show paths
  -- inputs <- mapM (readLit cfg lRef) $ map (fromPath cfg) paths
  (cfg, lRef, _, _) <- ask
  lift $ need' cfg lRef "test" [a1]
  inputs <- lift $ readLits cfg lRef a1
  liftIO $ putStrLn $ "aMath inputs: " ++ show inputs
  let result = foldl1 fn $ map (\n -> read n :: Scientific) inputs
  liftIO $ putStrLn $ "aMath result: " ++ show result
  lift $ writeLit cfg lRef out $ show result
