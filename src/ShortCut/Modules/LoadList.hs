module ShortCut.Modules.LoadList where

-- TODO call it load_strs? more accurate but sounds weird
-- TODO move the other load_* and mkLoad* functions here too
-- TODO rename the module to Load

import ShortCut.Core.Types
import ShortCut.Core.Compile.Basic (mkLoad)

cutModule :: CutModule
cutModule = CutModule
  { mName = "loadList"
  , mFunctions = [loadList]
  }

loadList :: CutFunction
loadList = mkLoad "load_list" (ListOf str)
