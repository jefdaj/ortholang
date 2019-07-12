module ShortCut.Modules.SetsTable
  where

import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic (defaultTypeCheck)
import ShortCut.Modules.Plots (rPlotListOfLists)

cutModule :: CutModule
cutModule = CutModule
  { mName = "SetsTable"
  , mDesc = "Generate set membership tables (spreadsheets) for easier list comparison"
  , mTypes = [tsv]
  , mFunctions = [setsTable]
  }

-- TODO move to Types.hs?
lit :: CutType
lit = CutTypeGroup
  { tgExt = "lit"
  , tgDesc = "basic literal (str or num)"
  , tgMember = \t -> t `elem` [str, num]
  }

-- TODO should this be more specific?
tsv :: CutType
tsv = CutType
  { tExt  = "tsv"
  , tDesc = "set membership table (spreadsheet)"
  , tShow = defaultShow
  }

setsTable :: CutFunction
setsTable = let name = "sets_table" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf (ListOf lit)] tsv
  , fTypeDesc  = mkTypeDesc  name [ListOf (ListOf lit)] tsv
  , fFixity    = Prefix
  , fRules     = rPlotListOfLists "sets_table.R"
  }
