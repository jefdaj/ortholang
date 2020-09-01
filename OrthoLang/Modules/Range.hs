{-# LANGUAGE QuasiQuotes #-}

module OrthoLang.Modules.Range
  where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

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
    [ rangeIntegers
    , rangeAdd
    , rangeLength
    , rangeMultiply
    -- , mkRangeFn "range_exponent" 4 -- TODO for this, need functions that take 4 args?
    ]
  }

-- mkRangeFn :: String -> Int -> Function
-- mkRangeFn name nArgs =  Function
--   { fOpChar = Nothing, fName = name
--   , fInputs = take nArgs $ repeat (Exactly num)
--   , fOutput = ListSigs (Exactly num)
--   , fTags = []
--   , fNewRules = NewNotImplemented
--   , fOldRules = rRange
--   }

-- TODO put somewhere as the standard way to construct an rSimpleScript that takes numbers?
-- rRange :: RulesFn
-- rRange scr e@(Fun _ _ _ name args) = do
--   cfg  <- fmap fromJust getShakeExtraRules
--   dRef <- fmap fromJust getShakeExtraRules
--   let out = exprPath cfg dRef scr e
--       out' = fromPath loc cfg out
--       loc = "modules.range.rRange"
--   argPaths <- fmap (map (\(ExprPath p) -> p)) $ mapM (rExpr scr) args
--   out' %> \_ -> do
--     as <- mapM (readLit loc) argPaths
--     runCmd $ CmdDesc
--       { cmdBinary = name <.> "R"
--       , cmdArguments = out':as
--       , cmdFixEmpties = False
--       , cmdParallel = False
--       , cmdOptions = []
--       , cmdInPatterns = as
--       , cmdNoNeedDirs = []
--       , cmdOutPath = out'
--       , cmdExtraOutPaths = []
--       , cmdSanitizePaths = []
--       , cmdExitCode = ExitSuccess
--       , cmdRmPatterns = [out']
--       }
--   return $ ExprPath out'
-- rRange _ e = error $ "bad argument to rRange: " ++ show e

rangeIntegers :: Function
rangeIntegers = newFnS2
  "range_integers"
  (Exactly num, Exactly num)
  (ListSigs $ Exactly num)
  "range_integers.R"
  []
  id

rangeAdd :: Function
rangeAdd = newFnS3
  "range_add"
  (Exactly num, Exactly num, Exactly num)
  (ListSigs $ Exactly num)
  "range_add.R"
  []
  id

rangeLength :: Function
rangeLength = newFnS3
  "range_length"
  (Exactly num, Exactly num, Exactly num)
  (ListSigs $ Exactly num)
  "range_length.R"
  []
  id

rangeMultiply :: Function
rangeMultiply = newFnS3
  "range_multiply"
  (Exactly num, Exactly num, Exactly num)
  (ListSigs $ Exactly num)
  "range_multiply.R"
  []
  id
