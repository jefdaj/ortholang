module ShortCut.Modules.Fasta where

-- TODO convert genbank to fasta in this module?

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Debug (debugTrackWrite)

import Development.Shake.FilePath     ((</>))
import ShortCut.Core.Compile          (cacheDir, cExpr, hashedTmp, toShortCutList, fromShortCutList, scriptTmpFile)
import ShortCut.Core.ModuleAPI        (mkLoad, mkLoadList, defaultTypeCheck, typeError)
import Text.PrettyPrint.HughesPJClass (text)

-----------------------
-- module definition --
-----------------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "fasta"
  , mFunctions =
    [ mkLoad "load_faa" faa
    , mkLoad "load_fna" fna
    , extractSeqs
    , extractSeqIDs
    , translate -- TODO make a "convert" module
    -- , back_transcribe
    -- , mkLoadList "load_csvs" csv -- TODO remove once list loading works
    ]
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

-------------------------------------------
-- extract sequence IDs from FASTA files --
-------------------------------------------

-- TODO this needs to do relative paths again, not absolute!

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

-- TODO move to API
cOneArgListScript :: FilePath -> FilePath -> CutState -> CutExpr -> Rules FilePath
cOneArgListScript tmpName script s@(_,cfg) expr@(CutFun _ _ _ [fa]) = do
  faPath <- cExpr s fa
  let tmpDir = cacheDir cfg </> tmpName
      tmpOut = scriptTmpFile cfg tmpDir expr "txt"
      actOut = hashedTmp cfg expr []
  tmpOut %> \out -> do
    need [faPath]
    quietly $ cmd script tmpDir out faPath
    -- trackWrite [out]
  actOut %> \_ -> toShortCutList cfg str tmpOut actOut
  return actOut
cOneArgListScript _ _ _ _ = error "bad argument to cOneArgListScript"

----------------------------------------------
-- extract sequences from FASTA files by ID --
----------------------------------------------

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

cConvert :: FilePath -> CutState -> CutExpr -> Rules FilePath
cConvert script s@(_,cfg) e@(CutFun _ _ _ [fa]) = do
  faPath <- cExpr s fa
  let oPath = hashedTmp cfg e []
  oPath %> \_ -> need [faPath] >> unit (cmd script oPath faPath)
    -- debugTrackWrite cfg [oPath] TODO is this implied?
  return oPath
cConvert _ _ _ = error "bad argument to cConvert"
