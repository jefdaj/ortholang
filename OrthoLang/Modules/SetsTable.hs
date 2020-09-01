module OrthoLang.Modules.SetsTable
  where

import OrthoLang.Interpreter
import OrthoLang.Types
import OrthoLang.Modules.Plots (listVarNames)

olModule :: Module
olModule = Module
  { mName = "SetsTable"
  , mDesc = "Generate set membership tables (spreadsheets) for easier list comparison"
  , mTypes = [tsv]
  , mGroups = [lit]
  , mEncodings = []
  , mFunctions = [setsTable, setsTableExplicit]
  }

-- TODO should this be more specific?
tsv :: Type
tsv = Type
  { tExt  = "tsv"
  , tDesc = "set membership table (spreadsheet)"
  , tShow = defaultShow
  }

setsTableExplicit :: Function
setsTableExplicit = newFnS2
  "sets_table_explicit"
  (ListSigs (Exactly str), ListSigs (ListSigs (Some lit "a literal")))
  (Exactly tsv)
  "sets_table.R"
  [Hidden]
  id

-- | User-facing version that auto-loads the script and captures any varnames in the untyped list.
setsTable :: Function
setsTable = newExprExpansion
  "sets_table"
  [ListSigs (ListSigs (Some lit "a literal"))]
  (Exactly tsv)
  mSetsTable
  []

-- | Macro that adds the label strs
mSetsTable :: ExprExpansion
mSetsTable mods scr (Fun r ms ds n [(Ref _ _ _ (Var _ name))]) = case lookupExpr name (sAssigns scr) of
  Nothing -> error "modules.plots.mSetsTable" $ "no such var: " ++ name
  Just e -> mSetsTable mods scr (Fun r ms ds n [e]) -- TODO is this the right way to handle it?
mSetsTable _ scr (Fun r ms ds _ [e@(Lst _ _ _ es)]) =
  let names = listVarNames "list" scr es
  in Fun r ms ds "sets_table_explicit" [names, e]
mSetsTable _ _ e = error "modules.plots.mSetsTable" $ "bad argument: " ++ show e
