module OrthoLang.Modules.AllVsAll
  where

-- TODO this should be easily doable using the extractExprs trick for lists but not fn calls,
--      but should you bother since it also might not be needed for the greencut algorithm?

-- import Development.Shake
import OrthoLang.Core
import OrthoLang.Modules.SeqIO (faa)

olModule :: Module
olModule = Module
  { mName = "All-Vs-All"
  , mDesc = "Creates all-vs-all hit tables from any BLAST-like search for use in ortholog finding algorithms"
  , mTypes = [ava]
  , mGroups = []
  , mEncodings = []
  , mFunctions = [] -- TODO put the functions here, or in their respective modules?
  }

ava :: Type
ava = Type
  { tExt  = "ava"
  , tDesc = "all-vs-all hit table listing"
  , tShow = defaultShow
  }

-- TODO is anything besides the name needed?
-- it seems like it has to be name : num faa.list -> bht basically
-- oh, except the result table type might be different?
-- TODO remove the other hit table types? check if they're needed at all
mkAva :: String -> Function
mkAva name = let name' = name ++ "_ava" in Function
  { fOpChar = Nothing, fName = name'
  -- , fTypeDesc  = mkTypeDesc  name' [num, ListOf faa] ava
  -- , fTypeCheck = defaultTypeCheck name' [num, ListOf faa] ava
  , fInputs = [Exactly num, Exactly (ListOf faa)]
  , fOutput = Exactly ava
  , fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMkAva
  }

-- TODO any reason to take the name as a separate arg here?
rMkAva :: RulesFn
rMkAva st (Fun _ _ _ _ [_, faas]) = do
  (ExprPath _) <- rExpr st faas
  return undefined
rMkAva _ e = error $ "bad argument to rMkAva: " ++ show e

-- construct a BLAST-like search expression from compiled paths
mkSearchExpr :: Type -> Maybe Salt -> [Var] -> String -> Expr -> ExprPath -> ExprPath -> Expr
mkSearchExpr rtn _ deps name  evalueExpr  queryFaPath subjFaPath
  =   Fun rtn Nothing deps name [evalueExpr, queryExpr,  subjExpr]
  where
    queryExpr = Com $ CompiledExpr faa queryFaPath (return queryFaPath)
    subjExpr  = Com $ CompiledExpr faa subjFaPath  (return subjFaPath)
