module OrthoLang.Modules.SetsTable
  where

import OrthoLang.Core
import OrthoLang.Modules.Plots (rPlotListOfLists)

olModule :: Module
olModule = Module
  { mName = "SetsTable"
  , mDesc = "Generate set membership tables (spreadsheets) for easier list comparison"
  , mTypes = [tsv]
  , mGroups = [lit]
  , mEncodings = []
  , mFunctions = [setsTable]
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
  -- , fTypeCheck = defaultTypeCheck name [ListOf (ListOf (Some lit "some lit"))] tsv
  -- , fTypeDesc  = mkTypeDesc       name [ListOf (ListOf (Some lit "some lit"))] tsv
  , fInputs = [ListSigs (ListSigs (Some lit "some lit"))] -- TODO would any type work, not just lits?
  , fOutput = Exactly tsv -- TODO would it help to make this EncodedAs tsv ...?
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rPlotListOfLists "sets_table.R"
  }
