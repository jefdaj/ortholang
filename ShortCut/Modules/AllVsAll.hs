module ShortCut.Modules.AllVsAll
  where

-- import Development.Shake
import ShortCut.Core.Types

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
