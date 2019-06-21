module ShortCut.Modules.Range
  where

import Development.Shake
import ShortCut.Core.Types

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

-- TODO add examples of these!

rangeAdd = mkRangeFn "range_add" 3
  "Arguments: start, stop, step\n\
  \A range made by repeatedly adding a constant step value"

rangeExponent = mkRangeFn "range_exponent" 4
  "Arguments: base, exp_start, exp_stop, exp_step\n\
  \A range of exponents applied to a common base"

rangeIntegers = mkRangeFn "range_integers" 2
  ""

rangeLength = mkRangeFn "range_length" 3
  ""

rangeMultiply = mkRangeFn "range_multiply" 3
  ""

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
