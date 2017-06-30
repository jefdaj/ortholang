module ShortCut.Modules.GenBank where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Compile   (cExpr, hashedTmp)
import ShortCut.Core.ModuleAPI (defaultTypeCheck, cOneArgScript, mkLoad)
import ShortCut.Modules.Fasta  (faa, fna)

cutModule :: CutModule
cutModule = CutModule
  { mName = "genbank"
  , mFunctions = [loadGbk, gbkToFaa, gbkToFna]
  }

gbk :: CutType
gbk = CutType
  { tExt  = "gbk"
  , tDesc = "genbank file"
  , tCat  = defaultCat
  }

loadGbk :: CutFunction
loadGbk = mkLoad "load_gbk" gbk

gbkToFaa :: CutFunction
gbkToFaa = CutFunction
  { fName      = "gbk_to_faa"
  , fTypeCheck = defaultTypeCheck [gbk] faa
  , fFixity    = Prefix
  , fCompiler  = cOneArgScript "genbank" "gbk_to_faa.py"
  }

gbkToFna :: CutFunction
gbkToFna = CutFunction
  { fName      = "gbk_to_fna"
  , fTypeCheck = defaultTypeCheck [gbk] fna
  , fFixity    = Prefix
  , fCompiler  = cOneArgScript "genbank" "gbk_to_fna.py"
  }
