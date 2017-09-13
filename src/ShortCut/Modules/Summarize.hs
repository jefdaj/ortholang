module ShortCut.Modules.Summarize where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Paths        (exprPathExplicit)
import ShortCut.Core.Rules      (rExpr)
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
summaryTypeCheck [(SetOf (SetOf t))] = Right $ SetOf t
summaryTypeCheck _ = Left "type error in summary!"

-- takes a list of lists and summarizes (flattens?) it to a single list
-- using the given summaryFn
-- TODO are paths hashes unique now??
-- TODO use writeFileChanged instead of writeFileLines?
--      (if it turns out to be re-running stuff unneccesarily)
rSummary :: ([[FilePath]] -> [FilePath]) -> CutState -> CutExpr -> Rules ExprPath
rSummary summaryFn s@(_,cfg) expr@(CutFun _ _ _ fnName [iList]) = do
  (ExprPath iPath) <- rExpr s iList
  let (SetOf (SetOf eType)) = typeOf iList
      (ExprPath oPath) = exprPathExplicit cfg True (SetOf eType) fnName 
                                          [show expr, iPath]
  oPath %> aSummary cfg summaryFn iPath
  return (ExprPath oPath)
rSummary _ _ _ = error "bad argument to rSummary"

aSummary :: CutConfig -> ([[String]] -> [String])
         -> FilePath -> FilePath -> Action ()
aSummary cfg summaryFn iPath out = do
  need [iPath]
  iLists <- fmap lines $ readFile' iPath
  iElems <- mapM (fmap lines . readFile' . (\p -> cfgTmpDir cfg </> p)) iLists
  let oElems = summaryFn iElems
  writeFileLines out oElems
