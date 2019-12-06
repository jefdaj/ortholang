module ShortCut.Modules.Summarize where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Paths (exprPath, fromCutPath)
import ShortCut.Core.Compile.Basic      (rExpr)
import ShortCut.Core.Actions (readLits, writeLits, traceA, need')
-- import ShortCut.Core.Debug (traceA)
import Development.Shake.FilePath ((</>))

cutModule :: CutModule
cutModule = CutModule
  { mName = "Summarize"
  , mDesc = "Collapse a list of results into a single summary"
  , mTypes = []
  , mFunctions =
    [
    ]
  }

-- TODO remove once a couple others are finished
-- commonElements :: CutFunction
-- commonElements = CutFunction
--   { fName      = "common_elements" -- TODO rename to `all`?
--   , fFixity    = Prefix
--   , fTypeCheck = summaryTypeCheck
--   , fDesc = Nothing, fTypeDesc  = undefined
--   , fRules  = rSummary (foldr1 intersect)
--   }

summaryTypeCheck :: [CutType] -> Either String CutType
summaryTypeCheck [(ListOf (ListOf t))] = Right $ ListOf t
summaryTypeCheck _ = Left "type error in summary!"

-- takes a list of lists and summarizes (flattens?) it to a single list
-- using the given summaryFn
-- TODO are paths hashes unique now??
--      (if it turns out to be re-running stuff unneccesarily)
rSummary :: ([[FilePath]] -> [FilePath]) -> CutState -> CutExpr -> Rules ExprPath
rSummary summaryFn s@(_, cfg, ref, _) expr@(CutFun _ _ _ _ [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  -- let (ListOf (ListOf eType)) = typeOf iList
      -- (ExprPath oPath) = exprPathExplicit cfg True (ListOf eType) fnName 
                                          -- [show expr, iPath]
  let oPath = fromCutPath cfg $ exprPath s expr
  oPath %> aSummary cfg ref summaryFn iPath
  return (ExprPath oPath)
rSummary _ _ _ = fail "bad argument to rSummary"

aSummary :: CutConfig -> Locks -> ([[String]] -> [String])
         -> FilePath -> FilePath -> Action ()
aSummary cfg ref summaryFn iPath out = do
  need' cfg ref "shortcut.modules.summary.aSummary" [iPath]
  iLists <- readLits cfg ref iPath
  iElems <- mapM (readLits cfg ref . (\p -> cfgTmpDir cfg </> p)) iLists
  let oElems = summaryFn iElems
      out' = traceA "aSummary" out [out, iPath]
  writeLits cfg ref out' oElems
