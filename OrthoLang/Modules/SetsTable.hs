module OrthoLang.Modules.SetsTable
  where

import OrthoLang.Core.Types
import OrthoLang.Core.Compile (defaultTypeCheck)
import OrthoLang.Modules.Plots (rPlotListOfLists)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "SetsTable"
  , mDesc = "Generate set membership tables (spreadsheets) for easier list comparison"
  , mTypes = [tsv]
  , mFunctions = [setsTable]
  }

-- TODO move to Types.hs?
lit :: Type
lit = TypeGroup
  { tgExt = "lit"
  , tgDesc = "basic literal (str or num)"
  , tgMember = \t -> t `elem` [str, num]
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
  , fTypeCheck = defaultTypeCheck [ListOf (ListOf lit)] tsv
  , fTypeDesc  = mkTypeDesc  name [ListOf (ListOf lit)] tsv
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rPlotListOfLists "sets_table.R"
  }
