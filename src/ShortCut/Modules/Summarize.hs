module ShortCut.Modules.Summarize where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Paths (exprPath, fromCutPath)
import ShortCut.Core.Compile.Basic      (rExpr)
import ShortCut.Core.Actions (readLits, writeLits)
import ShortCut.Core.Debug (debugAction)
import Development.Shake.FilePath ((</>))

cutModule :: CutModule
cutModule = CutModule
  { mName = "summarize"
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
--   , fRules  = rSummary (foldr1 intersect)
--   }

summaryTypeCheck :: [CutType] -> Either String CutType
summaryTypeCheck [(ListOf (ListOf t))] = Right $ ListOf t
summaryTypeCheck _ = Left "type error in summary!"

-- takes a list of lists and summarizes (flattens?) it to a single list
-- using the given summaryFn
-- TODO are paths hashes unique now??
-- TODO use writeFileChanged instead of writeFileLines?
--      (if it turns out to be re-running stuff unneccesarily)
rSummary :: ([[FilePath]] -> [FilePath]) -> CutState -> CutExpr -> Rules ExprPath
rSummary summaryFn s@(_,cfg,_) expr@(CutFun _ _ _ _ [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  -- let (ListOf (ListOf eType)) = typeOf iList
      -- (ExprPath oPath) = exprPathExplicit cfg True (ListOf eType) fnName 
                                          -- [show expr, iPath]
  let oPath = fromCutPath cfg $ exprPath s expr
  oPath %> aSummary cfg summaryFn iPath
  return (ExprPath oPath)
rSummary _ _ _ = error "bad argument to rSummary"

aSummary :: CutConfig -> ([[String]] -> [String])
         -> FilePath -> FilePath -> Action ()
aSummary cfg summaryFn iPath out = do
  need [iPath]
  iLists <- readLits cfg iPath
  iElems <- mapM (readLits cfg . (\p -> cfgTmpDir cfg </> p)) iLists
  let oElems = summaryFn iElems
      out' = debugAction cfg "aSummary" out [out, iPath]
  writeLits cfg out' oElems
