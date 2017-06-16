-- TODO go for simple: take and return faa only at first

module ShortCut.Modules.Shmlast where

import ShortCut.Core.Types
import ShortCut.Core.Parse    (defaultTypeCheck)
import ShortCut.Modules.Blast (faa, gom)

---------------
-- interface --
---------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "shmlast"
  , mFunctions = [ rbh, crbh ]
  }

bbh :: CutType
bbh = CutType
  { tExt  = "bbh"
  , tDesc = "tsv table of blast best hits"
  , tCat  = undefined
  }

rbh :: CutFunction
rbh = CutFunction
  { fName = "blast_rbh"
  , fTypeCheck = defaultTypeCheck [faa, ListOf gom, num] faa
  , fFixity  = Prefix
  , fCompiler = cRbh
  }

crbh :: CutFunction
crbh = CutFunction
  { fName = "blast_crbh"
  , fTypeCheck = defaultTypeCheck [faa, ListOf gom] faa
  , fFixity  = Prefix
  , fCompiler = cCrbh
  }

--------------------
-- implementation --
--------------------

cRbh = undefined
cCrbh = undefined
