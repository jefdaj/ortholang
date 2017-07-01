module ShortCut.Modules.SeqIO where

import Development.Shake
import ShortCut.Core.Types

import Development.Shake.FilePath ((</>))
import ShortCut.Core.Compile      (cacheDir, cExpr, hashedTmp, toShortCutList,
                                   fromShortCutList, scriptTmpFile)
import ShortCut.Core.Debug        (debugTrackWrite)
import ShortCut.Core.ModuleAPI    (mkLoad, mkLoadList, defaultTypeCheck,
                                   typeError, cOneArgScript, cOneArgListScript)

cutModule :: CutModule
cutModule = CutModule
  { mName = "seqio"
  , mFunctions =
    [ mkLoad "load_gbk" gbk
    , mkLoad "load_faa" faa
    , mkLoad "load_fna" fna
    , gbkToFaa
    , gbkToFna
    , extractSeqs
    , extractSeqIDs
    , translate
    ]
  }

gbk :: CutType
gbk = CutType
  { tExt  = "gbk"
  , tDesc = "genbank file"
  , tCat  = defaultCat
  }

faa :: CutType
faa = CutType
  { tExt  = "faa"
  , tDesc = "FASTA (amino acid)"
  , tCat  = defaultCat
  }

fna :: CutType
fna = CutType
  { tExt  = "fna"
  , tDesc = "FASTA (nucleic acid)"
  , tCat  = defaultCat
  }

gbkToFaa :: CutFunction
gbkToFaa = CutFunction
  { fName      = "gbk_to_faa"
  , fTypeCheck = defaultTypeCheck [gbk] faa
  , fFixity    = Prefix
  , fCompiler  = cOneArgScript "seqio" "gbk_to_faa.py"
  }

gbkToFna :: CutFunction
gbkToFna = CutFunction
  { fName      = "gbk_to_fna"
  , fTypeCheck = defaultTypeCheck [gbk] fna
  , fFixity    = Prefix
  , fCompiler  = cOneArgScript "seqio" "gbk_to_fna.py"
  }

-------------------------------------------
-- extract sequence IDs from FASTA files --
-------------------------------------------

-- TODO this needs to do relative paths again, not absolute!
-- TODO also extract them from genbank files

extractSeqIDs :: CutFunction
extractSeqIDs = CutFunction
  { fName      = "extract_seq_ids"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqIDs
  , fCompiler  = cExtractSeqIDs
  }

tExtractSeqIDs [x] | elem x [faa, fna] = Right (ListOf str)
tExtractSeqIDs _ = Left "expected a fasta file"

cExtractSeqIDs = cOneArgListScript "fasta" "extract-seq-ids.py"

----------------------------------------------
-- extract sequences from FASTA files by ID --
----------------------------------------------

-- TODO also extract them from genbank files

extractSeqs :: CutFunction
extractSeqs = CutFunction
  { fName      = "extract_seqs_by_id"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqs
  , fCompiler  = cExtractSeqs
  }

-- TODO does ListOf str match on the value or just the constructor?
tExtractSeqs [x, ListOf str] | elem x [faa, fna] = Right x
tExtractSeqs _ = Left "expected a list of strings and a fasta file"

-- TODO can this be replaced with cOneArgListScript?
cExtractSeqs :: CutState -> CutExpr -> Rules FilePath
cExtractSeqs s@(_,cfg) e@(CutFun _ _ _ [fa, ids]) = do
  faPath  <- cExpr s fa
  idsPath <- cExpr s ids
  -- liftIO . putStrLn $ "extracting sequences from " ++ faPath
  let faTmp   = cacheDir cfg </> "fasta"
      outPath = hashedTmp cfg e []
      tmpList = scriptTmpFile cfg faTmp e "txt"
  tmpList %> \_ -> fromShortCutList cfg faTmp idsPath tmpList
  outPath %> \_ -> do
    need [faPath, tmpList]
    quietly $ cmd "extract-seqs-by-id.py" faTmp outPath faPath tmpList
  return outPath
mExtractSeqs _ _ = error "bad argument to extractSeqs"

-------------------------------------
-- convert between DNA and protein --
-------------------------------------

-- TODO name something else like fna_to_faa?
translate :: CutFunction
translate = CutFunction
  { fName      = "translate"
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [fna] faa
  , fCompiler  = cConvert "translate.py"
  }

-- TODO remove as biologically invalid?
-- back_transcribe :: CutFunction
-- back_transcribe = CutFunction
--   { fName      = "back_transcribe"
--   , fFixity    = Prefix
--   , fTypeCheck = defaultTypeCheck [faa] fna
--   , fCompiler  = cConvert "back_transcribe.py"
--   }

-- TODO can this use cOneArgScript?
cConvert :: FilePath -> CutState -> CutExpr -> Rules FilePath
cConvert script s@(_,cfg) e@(CutFun _ _ _ [fa]) = do
  faPath <- cExpr s fa
  let oPath = hashedTmp cfg e []
  oPath %> \_ -> need [faPath] >> unit (cmd script oPath faPath)
    -- debugTrackWrite cfg [oPath] TODO is this implied?
  return oPath
cConvert _ _ _ = error "bad argument to cConvert"
