module OrthoLang.Modules.Summarize where

import Development.Shake
import OrthoLang.Core.Types

import OrthoLang.Core.Paths (exprPath, fromOrthoLangPath)
import OrthoLang.Core.Compile.Basic      (rExpr)
import OrthoLang.Core.Actions (readLits, writeLits, traceA, need')
-- import OrthoLang.Core.Debug (traceA)
import Development.Shake.FilePath ((</>))

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "Summarize"
  , mDesc = "Collapse a list of results into a single summary"
  , mTypes = []
  , mFunctions =
    [
    ]
  }

-- TODO remove once a couple others are finished
-- commonElements :: OrthoLangFunction
-- commonElements = OrthoLangFunction
--   { fNames     = "common_elements" -- TODO rename to `all`?
--   , fFixity    = Prefix, fTags = []
--   , fTypeCheck = summaryTypeCheck
--   , fDesc = Nothing, fTypeDesc  = undefined
--   , fRules  = rSummary (foldr1 intersect)
--   }

summaryTypeCheck :: [OrthoLangType] -> Either String OrthoLangType
summaryTypeCheck [(ListOf (ListOf t))] = Right $ ListOf t
summaryTypeCheck _ = Left "type error in summary!"

-- takes a list of lists and summarizes (flattens?) it to a single list
-- using the given summaryFn
-- TODO are paths hashes unique now??
--      (if it turns out to be re-running stuff unneccesarily)
rSummary :: ([[FilePath]] -> [FilePath]) -> OrthoLangState -> OrthoLangExpr -> Rules ExprPath
rSummary summaryFn s@(_, cfg, ref, _) expr@(OrthoLangFun _ _ _ _ [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  -- let (ListOf (ListOf eType)) = typeOf iList
      -- (ExprPath oPath) = exprPathExplicit cfg True (ListOf eType) fnName 
                                          -- [show expr, iPath]
  let oPath = fromOrthoLangPath cfg $ exprPath s expr
  oPath %> aSummary cfg ref summaryFn iPath
  return (ExprPath oPath)
rSummary _ _ _ = fail "bad argument to rSummary"

aSummary :: OrthoLangConfig -> Locks -> ([[String]] -> [String])
         -> FilePath -> FilePath -> Action ()
aSummary cfg ref summaryFn iPath out = do
  need' cfg ref "ortholang.modules.summary.aSummary" [iPath]
  iLists <- readLits cfg ref iPath
  iElems <- mapM (readLits cfg ref . (\p -> cfgTmpDir cfg </> p)) iLists
  let oElems = summaryFn iElems
      out' = traceA "aSummary" out [out, iPath]
  writeLits cfg ref out' oElems
