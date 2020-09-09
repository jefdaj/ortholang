module OrthoLang.Modules.Summarize where

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

-- import OrthoLang.Types (traceA)
import Development.Shake.FilePath ((</>))
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "Summarize"
  , mDesc = "Collapse a list of results into a single summary"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mRules = return ()
  , mFunctions =
    [
    ]
  }

-- TODO remove once a couple others are finished
-- commonElements :: Function
-- commonElements = Function
--   { fNames     = "common_elements" -- TODO rename to `all`?
--   ,fTags = []
--   , fTypeCheck = summaryTypeCheck
--   , fDesc = Nothing, fTypeDesc  = undefined
--   , fNewRules = NewNotImplemented, fOldRules = rSummary (foldr1 intersect)
--   }

summaryTypeCheck :: [Type] -> Either String Type
summaryTypeCheck [(ListOf (ListOf t))] = Right $ ListOf t
summaryTypeCheck _ = Left "type error in summary!"

-- takes a list of lists and summarizes (flattens?) it to a single list
-- using the given summaryFn
-- TODO are paths hashes unique now??
--      (if it turns out to be re-running stuff unneccesarily)
rSummary :: ([[FilePath]] -> [FilePath]) -> RulesFn
rSummary summaryFn scr expr@(Fun _ _ _ _ [iList]) = do
  (ExprPath iPath) <- rExpr scr iList
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  -- let (ListOf (ListOf eType)) = typeOf iList
      -- (ExprPath oPath) = unsafeExprPathExplicit cfg True (ListOf eType) fnName 
                                          -- [show expr, iPath]
  let loc = "modules.summarize.rSummary"
      oPath = fromPath loc cfg $ exprPath cfg dRef scr expr
  oPath %> aSummary summaryFn iPath
  return (ExprPath oPath)
rSummary _ _ _ = fail "bad argument to rSummary"

aSummary :: ([[String]] -> [String]) -> FilePath -> FilePath -> Action ()
aSummary summaryFn iPath out = do
  let loc = "ortholang.modules.summary.aSummary"
  need' loc [iPath]
  iLists <- readLits loc iPath
  cfg <- fmap fromJust getShakeExtra
  iElems <- mapM (readLits loc . (\p -> tmpdir cfg </> p)) iLists
  let oElems = summaryFn iElems
      out' = traceA loc out [out, iPath]
  writeLits loc out' oElems
