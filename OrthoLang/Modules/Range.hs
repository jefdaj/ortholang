{-# LANGUAGE QuasiQuotes #-}

module OrthoLang.Modules.Range
  where

import Development.Shake
import OrthoLang.Core

import System.Exit     (ExitCode(..))
import System.FilePath ((<.>))
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "Range"
  , mDesc = "Generate ranges of numbers"
  , mTypes = [num]
  , mGroups = []
  , mEncodings = []
  , mFunctions =
    [ mkRangeFn "range_add"      3
    , mkRangeFn "range_exponent" 4 -- TODO for this, need functions that take 4 args?
    , mkRangeFn "range_integers" 2
    , mkRangeFn "range_length"   3
    , mkRangeFn "range_multiply" 3
    ]
  }

mkRangeFn :: String -> Int -> Function
mkRangeFn name nArgs =  Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name (take nArgs $ repeat num) (ListOf num)
  -- , fTypeDesc  = mkTypeDesc name  (take nArgs $ repeat num) (ListOf num)
  , fInputs = take nArgs $ repeat (Exactly num)
  , fOutput = ListSigs (Exactly num)
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rRange
  }

-- TODO put somewhere as the standard way to construct an rSimpleScript that takes numbers?
rRange :: RulesFn
rRange scr e@(Fun _ _ _ name args) = do
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let out = exprPath cfg dRef scr e
      out' = fromPath loc cfg out
      loc = "modules.range.rRange"
  argPaths <- fmap (map (\(ExprPath p) -> p)) $ mapM (rExpr scr) args
  out' %> \_ -> do
    as <- mapM (readLit loc) argPaths
    runCmd $ CmdDesc
      { cmdBinary = name <.> "R"
      , cmdArguments = out':as
      , cmdFixEmpties = False
      , cmdParallel = False
      , cmdOptions = []
      , cmdInPatterns = as
      , cmdOutPath = out'
      , cmdExtraOutPaths = []
      , cmdSanitizePaths = []
      , cmdExitCode = ExitSuccess
      , cmdRmPatterns = [out']
      }
  return $ ExprPath out'
rRange _ e = error $ "bad argument to rRange: " ++ show e
