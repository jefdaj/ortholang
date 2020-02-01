module OrthoLang.Modules.SetsTable
  where

import OrthoLang.Core.Types
import OrthoLang.Core.Compile.Basic (defaultTypeCheck)
import OrthoLang.Modules.Plots (rPlotListOfLists)

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
  { mName = "SetsTable"
  , mDesc = "Generate set membership tables (spreadsheets) for easier list comparison"
  , mTypes = [tsv]
  , mFunctions = [setsTable]
  }

-- TODO move to Types.hs?
lit :: OrthoLangType
lit = OrthoLangTypeGroup
  { tgExt = "lit"
  , tgDesc = "basic literal (str or num)"
  , tgMember = \t -> t `elem` [str, num]
  }

-- TODO should this be more specific?
tsv :: OrthoLangType
tsv = OrthoLangType
  { tExt  = "tsv"
  , tDesc = "set membership table (spreadsheet)"
  , tShow = defaultShow
  }

setsTable :: OrthoLangFunction
setsTable = let name = "sets_table" in OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [ListOf (ListOf lit)] tsv
  , fTypeDesc  = mkTypeDesc  name [ListOf (ListOf lit)] tsv
  , fFixity    = Prefix
  , fRules     = rPlotListOfLists "sets_table.R"
  }
