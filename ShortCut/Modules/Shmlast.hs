-- TODO go for simple: take and return faa only at first
-- TODO before working on this, get it to work on its own!

module ShortCut.Modules.Shmlast where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Parse    (defaultTypeCheck)
import ShortCut.Modules.Fasta (faa)

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
  { fName      = "blast_rbh"
  , fTypeCheck = defaultTypeCheck [faa, ListOf faa, num] faa
  , fFixity    = Prefix
  , fCompiler  = cRbh
  }

crbh :: CutFunction
crbh = CutFunction
  { fName      = "blast_crbh"
  , fTypeCheck = defaultTypeCheck [faa, ListOf faa] faa
  , fFixity    = Prefix
  , fCompiler  = cCrbh
  }

--------------------
-- implementation --
--------------------

cRbh :: CutState -> CutExpr -> Rules FilePath
cRbh = undefined

cCrbh :: CutState -> CutExpr -> Rules FilePath
cCrbh = undefined
