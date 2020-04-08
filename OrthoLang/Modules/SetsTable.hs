module OrthoLang.Modules.SetsTable
  where

import OrthoLang.Core
import OrthoLang.Core (defaultTypeCheck)
import OrthoLang.Modules.Plots (rPlotListOfLists)

olModule :: Module
olModule = Module
  { mName = "SetsTable"
  , mDesc = "Generate set membership tables (spreadsheets) for easier list comparison"
  , mTypes = [tsv]
  , mGroups = [lit]
  , mFunctions = [setsTable]
  }

-- TODO move to Types.hs
-- TODO and either use it in the core compilers or remove it
lit :: TypeGroup
lit = TypeGroup
  { tgExt = "lit"
  , tgDesc = "basic literal (str or num)"
  , tgTypes = [str, num]
  }

-- TODO should this be more specific?
tsv :: Type
tsv = Type
  { tExt  = "tsv"
  , tDesc = "set membership table (spreadsheet)"
  , tShow = defaultShow
  }

setsTable :: Function
setsTable = let name = "sets_table" in Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [ListOf (ListOf lit)] tsv
  , fTypeDesc  = mkTypeDesc  name [ListOf (ListOf lit)] tsv
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rPlotListOfLists "sets_table.R"
  }
