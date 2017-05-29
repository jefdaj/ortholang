module ShortCut.Modules.Summarize where

import Development.Shake
import ShortCut.Core.Types

import Data.List                  (intersect)
import ShortCut.Core.Compile      (cExpr, hashedTmp')
import Development.Shake.FilePath ((</>))

cutModule :: CutModule
cutModule = CutModule
  { mName = "summarize"
  , mFunctions =
    [ commonElements
    ]
  }

commonElements :: CutFunction
commonElements = CutFunction
  { fName      = "common_elements"
  , fFixity    = Prefix
  , fTypeCheck = summaryTypeCheck
  , fCompiler  = cSummary (foldr1 intersect)
  }

summaryTypeCheck :: [CutType] -> Either String CutType
summaryTypeCheck [(ListOf (ListOf t))] = Right $ ListOf t
summaryTypeCheck _ = Left "type error in summary!"

-- takes a list of lists and summarizes (flattens?) it to a single list
-- using the given summaryFn
-- TODO are paths hashes unique now??
-- TODO use writeFileChanged instead of writeFileLines?
--      (if it turns out to be re-running stuff unneccesarily)
cSummary :: ([[FilePath]] -> [FilePath]) -> CutState -> CutExpr -> Rules FilePath
cSummary summaryFn s@(_,cfg) expr@(CutFun _ _ fnName [iList]) = do
  iPath <- cExpr s iList
  let (ListOf (ListOf eType)) = typeOf iList
      oPath = hashedTmp' cfg (ListOf eType) expr [iPath, fnName]
  oPath %> \out -> do
    need [iPath]
    iLists <- fmap lines $ readFile' iPath
    iElems <- mapM (fmap lines . readFile' . (\p -> cfgTmpDir cfg </> p)) iLists
    let oElems = summaryFn iElems
    writeFileLines out oElems
  return oPath
cSummary _ _ _ = error "bad argument to cSummary"
