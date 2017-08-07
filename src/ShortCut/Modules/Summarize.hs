module ShortCut.Modules.Summarize where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Paths        (exprPathExplicit)
import ShortCut.Core.Compile      (cExpr)
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
--   , fCompiler  = cSummary (foldr1 intersect)
--   }

summaryTypeCheck :: [CutType] -> Either String CutType
summaryTypeCheck [(ListOf (ListOf t))] = Right $ ListOf t
summaryTypeCheck _ = Left "type error in summary!"

-- takes a list of lists and summarizes (flattens?) it to a single list
-- using the given summaryFn
-- TODO are paths hashes unique now??
-- TODO use writeFileChanged instead of writeFileLines?
--      (if it turns out to be re-running stuff unneccesarily)
cSummary :: ([[FilePath]] -> [FilePath]) -> CutState -> CutExpr -> Rules ExprPath
cSummary summaryFn s@(_,cfg) expr@(CutFun _ _ _ fnName [iList]) = do
  (ExprPath iPath) <- cExpr s iList
  let (ListOf (ListOf eType)) = typeOf iList
      (ExprPath oPath) = exprPathExplicit cfg (ListOf eType) expr fnName [ExprPath iPath]
  oPath %> \out -> do
    need [iPath]
    iLists <- fmap lines $ readFile' iPath
    iElems <- mapM (fmap lines . readFile' . (\p -> cfgTmpDir cfg </> p)) iLists
    let oElems = summaryFn iElems
    writeFileLines out oElems
  return (ExprPath oPath)
cSummary _ _ _ = error "bad argument to cSummary"
