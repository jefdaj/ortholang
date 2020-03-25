module OrthoLang.Modules.AllVsAll
  where

-- TODO this should be easily doable using the extractExprs trick for lists but not fn calls,
--      but should you bother since it also might not be needed for the greencut algorithm?

-- import Development.Shake
import OrthoLang.Core.Types
import OrthoLang.Core.Compile (defaultTypeCheck, rExpr)
import OrthoLang.Modules.SeqIO         (faa)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "All-Vs-All"
  , mDesc = "Creates all-vs-all hit tables from any BLAST-like search for use in ortholog finding algorithms"
  , mTypes = [ava]
  , mFunctions = [] -- TODO put the functions here, or in their respective modules?
  }

ava :: OrthoLangType
ava = OrthoLangType
  { tExt  = "ava"
  , tDesc = "all-vs-all hit table listing"
  , tShow = defaultShow
  }

-- TODO is anything besides the name needed?
-- it seems like it has to be name : num faa.list -> bht basically
-- oh, except the result table type might be different?
-- TODO remove the other hit table types? check if they're needed at all
mkAva :: String -> OrthoLangFunction
mkAva name = let name' = name ++ "_ava" in OrthoLangFunction
  { fNames     = [name']
  , fTypeDesc  = mkTypeDesc  name' [num, ListOf faa] ava
  , fTypeCheck = defaultTypeCheck  [num, ListOf faa] ava
  , fFixity    = Prefix, fTags = []
  , fNewRules = Nothing, fOldRules = rMkAva
  }

-- TODO any reason to take the name as a separate arg here?
rMkAva :: RulesFn
rMkAva st (OrthoLangFun _ _ _ _ [_, faas]) = do
  (ExprPath _) <- rExpr st faas
  return undefined
rMkAva _ e = error $ "bad argument to rMkAva: " ++ show e

-- construct a BLAST-like search expression from compiled paths
mkSearchExpr :: OrthoLangType -> RepeatSalt -> [OrthoLangVar] -> String -> OrthoLangExpr -> ExprPath -> ExprPath -> OrthoLangExpr
mkSearchExpr rtn salt deps name  evalueExpr  queryFaPath subjFaPath
  =   OrthoLangFun rtn salt deps name [evalueExpr, queryExpr,  subjExpr]
  where
    queryExpr = OrthoLangRules $ CompiledExpr faa queryFaPath (return queryFaPath)
    subjExpr  = OrthoLangRules $ CompiledExpr faa subjFaPath  (return subjFaPath)
