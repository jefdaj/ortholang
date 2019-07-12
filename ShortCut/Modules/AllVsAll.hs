module ShortCut.Modules.AllVsAll
  where

-- import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rExpr)
import ShortCut.Modules.SeqIO         (faa)

cutModule :: CutModule
cutModule = CutModule
  { mName = "All-Vs-All"
  , mDesc = "Creates all-vs-all hit tables from any BLAST-like search for use in ortholog finding algorithms"
  , mTypes = [ava]
  , mFunctions = [] -- TODO put the functions here, or in their respective modules?
  }

ava :: CutType
ava = CutType
  { tExt  = "ava"
  , tDesc = "all-vs-all hit table listing"
  , tShow = defaultShow
  }

-- TODO is anything besides the name needed?
-- it seems like it has to be name : num faa.list -> bht basically
-- oh, except the result table type might be different?
-- TODO remove the other hit table types? check if they're needed at all
mkAva :: String -> CutFunction
mkAva name = let name' = name ++ "_ava" in CutFunction
  { fName      = name'
  , fTypeDesc  = mkTypeDesc  name' [num, ListOf faa] ava
  , fTypeCheck = defaultTypeCheck  [num, ListOf faa] ava
  , fFixity    = Prefix
  , fRules     = rMkAva
  }

-- TODO any reason to take the name as a separate arg here?
rMkAva :: RulesFn
rMkAva st (CutFun rtn salt deps name [e, faas]) = do
  (ExprPath faasPath) <- rExpr st faas
  return undefined
rMkAva _ e = error $ "bad argument to rMkAva: " ++ show e

-- construct a BLAST-like search expression from compiled paths
mkSearchExpr rtn salt deps name  evalueExpr  queryFaPath subjFaPath
  =   CutFun rtn salt deps name [evalueExpr, queryExpr,  subjExpr]
  where
    queryExpr = CutRules $ CompiledExpr faa queryFaPath (return queryFaPath)
    subjExpr  = CutRules $ CompiledExpr faa subjFaPath  (return subjFaPath)
