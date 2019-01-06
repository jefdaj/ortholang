module Detourrr.Modules.Summarize where

import Development.Shake
import Detourrr.Core.Types

import Detourrr.Core.Paths (exprPath, fromRrrPath)
import Detourrr.Core.Compile.Basic      (rExpr)
import Detourrr.Core.Actions (readLits, writeLits, debugA, debugNeed)
-- import Detourrr.Core.Debug (debugA)
import Development.Shake.FilePath ((</>))

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "Summarize"
  , mDesc = "Collapse a list of results into a single summary"
  , mTypes = []
  , mFunctions =
    [
    ]
  }

-- TODO remove once a couple others are finished
-- commonElements :: RrrFunction
-- commonElements = RrrFunction
--   { fName      = "common_elements" -- TODO rename to `all`?
--   , fFixity    = Prefix
--   , fTypeCheck = summaryTypeCheck
--   , fDesc = Nothing, fTypeDesc  = undefined
--   , fRules  = rSummary (foldr1 intersect)
--   }

summaryTypeCheck :: [RrrType] -> Either String RrrType
summaryTypeCheck [(ListOf (ListOf t))] = Right $ ListOf t
summaryTypeCheck _ = Left "type error in summary!"

-- takes a list of lists and summarizes (flattens?) it to a single list
-- using the given summaryFn
-- TODO are paths hashes unique now??
--      (if it turns out to be re-running stuff unneccesarily)
rSummary :: ([[FilePath]] -> [FilePath]) -> RrrState -> RrrExpr -> Rules ExprPath
rSummary summaryFn s@(_, cfg, ref, _) expr@(RrrFun _ _ _ _ [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  -- let (ListOf (ListOf eType)) = typeOf iList
      -- (ExprPath oPath) = exprPathExplicit cfg True (ListOf eType) fnName 
                                          -- [show expr, iPath]
  let oPath = fromRrrPath cfg $ exprPath s expr
  oPath %> aSummary cfg ref summaryFn iPath
  return (ExprPath oPath)
rSummary _ _ _ = error "bad argument to rSummary"

aSummary :: RrrConfig -> Locks -> ([[String]] -> [String])
         -> FilePath -> FilePath -> Action ()
aSummary cfg ref summaryFn iPath out = do
  debugNeed cfg "aSummary" [iPath]
  iLists <- readLits cfg ref iPath
  iElems <- mapM (readLits cfg ref . (\p -> cfgTmpDir cfg </> p)) iLists
  let oElems = summaryFn iElems
      out' = debugA cfg "aSummary" out [out, iPath]
  writeLits cfg ref out' oElems
