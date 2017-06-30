module ShortCut.Modules.GenBank where

import ShortCut.Core.Types
import Development.Shake
import ShortCut.Core.Compile (cExpr, hashedTmp)
import ShortCut.Modules.Fasta (faa, fna)
import ShortCut.Core.ModuleAPI (defaultTypeCheck, cOneArgScript, mkLoad)

cutModule :: CutModule
cutModule = CutModule
  { mName = "genbank"
  , mFunctions = 
    [ mkLoad "load_gbk" gbk
    , gbkToFaa
    ]
  }

gbk :: CutType
gbk = CutType
  { tExt  = "gbk"
  , tDesc = "genbank file"
  , tCat  = defaultCat
  }

-- TODO fna version, which is what i really wanted...
gbkToFaa :: CutFunction
gbkToFaa = CutFunction
  { fName      = "gbk_to_faa"
  , fTypeCheck = defaultTypeCheck [gbk] faa
  , fFixity    = Prefix
  , fCompiler  = cOneArgScript "genbank" "gbk_to_faa.py"
  }

-- cGenbankToFaa :: CutState -> CutExpr -> Rules FilePath
-- cGenbankToFaa s@(_,cfg) e@(CutFun _ _ _ [gbk]) = do
--   gbkPath <- cExpr s gbk
--   let oPath = hashedTmp cfg e []
--   oPath %> \_ -> undefined
--   return oPath
-- cGenbankToFaa _ _ = error "bad argument to cGenbankToFaa"
