{-# LANGUAGE QuasiQuotes #-}

-- TODO oh man, does the parser need to work with negative numbers for this?
--      could probably get around it with _subtract, _divide etc. but that's ugly

module ShortCut.Modules.Range
  where

import Development.Shake
import ShortCut.Core.Types
import Text.RawString.QQ

import System.FilePath ((<.>))
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rExpr)
import ShortCut.Core.Actions (readLit, CmdDesc(..), runCmd)
import ShortCut.Core.Paths (exprPath, fromCutPath)
import System.Exit (ExitCode(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "Range"
  , mDesc = "Generate ranges of numbers"
  , mTypes = [num]
  , mFunctions =
    [ rangeAdd
    , rangeExponent
    , rangeIntegers
    , rangeLength
    , rangeMultiply
    ]
  }

rangeAdd = mkRangeFn "range_add" 3 [r|Arguments: start, stop, step
A range made by repeatedly adding a constant step value.

Examples:
range_add 1 10 2 -> [1, 3, 5, 7, 9]|]

rangeExponent = mkRangeFn "range_exponent" 4 [r|Arguments: base, exp_start, exp_stop, exp_step
Think of it like base ^ (range_add start stop step).
That is, you have a base number and apply a range_add of exponents to it.
Very useful for specifying a range of e-value cutoffs!

Examples:
range_exponent 1e-1 0 50 10 -> [1, 1.0e-10, 1.0e-20, 1.0e-30, 1.0e-40, 1.0e-50]
range_exponent  1.5 1 10  4 -> [1.5, 7.59375, 38.443359375]|]

rangeIntegers = mkRangeFn "range_integers" 2
  "" -- TODO write desc with examples

rangeLength = mkRangeFn "range_length" 3
  "" -- TODO write desc with examples

rangeMultiply = mkRangeFn "range_multiply" 3
  "" -- TODO write desc with examples

mkRangeFn :: String -> Int -> String -> CutFunction
mkRangeFn name nArgs desc =  CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck (take nArgs $ repeat num) (ListOf num)
  , fTypeDesc  = mkTypeDesc name  (take nArgs $ repeat num) (ListOf num)
  , fDesc      = Just desc
  , fFixity    = Prefix
  , fRules     = rRange
  }

-- TODO put somewhere as the standard way to construct an rSimpleScript that takes numbers?
rRange :: RulesFn
rRange st@(_, cfg, ref, _) e@(CutFun _ _ _ name args) = do
  let out = exprPath st e
      out' = fromCutPath cfg out
  argPaths <- fmap (map (\(ExprPath p) -> p)) $ mapM (rExpr st) args
  out' %> \_ -> do
    args <- mapM (readLit cfg ref) argPaths
    runCmd cfg ref $ CmdDesc
      { cmdBinary = name <.> "R"
      , cmdArguments = out':args
      , cmdFixEmpties = False
      , cmdParallel = False
      , cmdOptions = []
      , cmdInPatterns = args
      , cmdOutPath = out'
      , cmdExtraOutPaths = []
      , cmdSanitizePaths = []
      , cmdExitCode = ExitSuccess
      , cmdRmPatterns = [out']
      }
  return $ ExprPath out'
rRange _ e = error $ "bad argument to rRange: " ++ show e
