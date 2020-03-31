module OrthoLang.Modules.Summarize where

import Development.Shake
import OrthoLang.Core

import OrthoLang.Core (exprPath, fromPath)
import OrthoLang.Core      (rExpr)
import OrthoLang.Core (readLits, writeLits, traceA, need')
-- import OrthoLang.Core (traceA)
import Development.Shake.FilePath ((</>))

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "Summarize"
  , mDesc = "Collapse a list of results into a single summary"
  , mTypes = []
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
--   , fNewRules = Nothing, fOldRules = rSummary (foldr1 intersect)
--   }

summaryTypeCheck :: [Type] -> Either String Type
summaryTypeCheck [(ListOf (ListOf t))] = Right $ ListOf t
summaryTypeCheck _ = Left "type error in summary!"

-- takes a list of lists and summarizes (flattens?) it to a single list
-- using the given summaryFn
-- TODO are paths hashes unique now??
--      (if it turns out to be re-running stuff unneccesarily)
rSummary :: ([[FilePath]] -> [FilePath]) -> RulesFn
rSummary summaryFn s@(scr, cfg, ref, _, _) expr@(Fun _ _ _ _ [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  -- let (ListOf (ListOf eType)) = typeOf iList
      -- (ExprPath oPath) = exprPathExplicit cfg True (ListOf eType) fnName 
                                          -- [show expr, iPath]
  let oPath = fromPath cfg $ exprPath cfg scr expr
  oPath %> aSummary cfg ref summaryFn iPath
  return (ExprPath oPath)
rSummary _ _ _ = fail "bad argument to rSummary"

aSummary :: Config -> LocksRef -> ([[String]] -> [String])
         -> FilePath -> FilePath -> Action ()
aSummary cfg ref summaryFn iPath out = do
  need' cfg ref "ortholang.modules.summary.aSummary" [iPath]
  iLists <- readLits cfg ref iPath
  iElems <- mapM (readLits cfg ref . (\p -> cfgTmpDir cfg </> p)) iLists
  let oElems = summaryFn iElems
      out' = traceA "aSummary" out [out, iPath]
  writeLits cfg ref out' oElems
