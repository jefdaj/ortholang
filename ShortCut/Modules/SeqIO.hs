-- TODO rename something more general like SeqUtils

module ShortCut.Modules.SeqIO where

import Development.Shake
import ShortCut.Core.Types

import Data.List                  (intercalate)
import Development.Shake.FilePath ((</>))
import ShortCut.Core.Paths        (hashedTmp, scriptTmpFile)
import ShortCut.Core.Compile      (cExpr, toShortCutList, fromShortCutList)
import ShortCut.Core.Debug        (debug, debugTrackWrite, debugReadLines,
                                   debugReadFile, debugWriteFile)
import ShortCut.Core.ModuleAPI    (mkLoad, mkLoadList, defaultTypeCheck,
                                   typeError, cOneArgScript, cOneArgListScript)
import System.Directory           (createDirectoryIfMissing)

cutModule :: CutModule
cutModule = CutModule
  { mName = "seqio"
  , mFunctions =
    [ loadGbk
    , loadGbkAll
    , loadFaa
    , loadFaaAll
    , loadFna
    , loadFaaAll
    , gbkToFaa
    , gbkToFna
    , extractSeqs
    , extractSeqIDs
    , translate
    , concatFastas
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

--------------------
-- load sequences --
--------------------

loadFaa :: CutFunction
loadFaa = mkLoad "load_faa" faa

loadFaaAll :: CutFunction
loadFaaAll = mkLoadList "load_faa_all" faa

loadFna :: CutFunction
loadFna = mkLoad "load_fna" fna

loadFnaAll :: CutFunction
loadFnaAll = mkLoadList "load_fna_all" fna

loadGbk :: CutFunction
loadGbk = mkLoad "load_gbk" gbk

loadGbkAll :: CutFunction
loadGbkAll = mkLoadList "load_gbk_all" gbk

-------------------------------------------
-- extract sequence IDs from FASTA files --
-------------------------------------------

-- TODO this needs to do relative paths again, not absolute!
-- TODO also extract them from genbank files

extractSeqIDs :: CutFunction
extractSeqIDs = CutFunction
  { fName      = "extract_ids"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqIDs
  , fCompiler  = cExtractSeqIDs
  }

tExtractSeqIDs [x] | elem x [faa, fna] = Right (ListOf str)
tExtractSeqIDs _ = Left "expected a fasta file"

cExtractSeqIDs = cOneArgListScript "seqio" "extract_ids.py"

----------------------------------------------
-- extract sequences from FASTA files by ID --
----------------------------------------------

-- TODO also extract them from genbank files

extractSeqs :: CutFunction
extractSeqs = CutFunction
  { fName      = "extract_seqs"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqs
  , fCompiler  = cExtractSeqs
  }

-- TODO does ListOf str match on the value or just the constructor?
tExtractSeqs [x, ListOf str] | elem x [faa, fna] = Right x
tExtractSeqs _ = Left "expected a list of strings and a fasta file"

-- TODO can this be replaced with cOneArgListScript?
cExtractSeqs :: CutState -> CutExpr -> Rules ExprPath
cExtractSeqs s@(_,cfg) e@(CutFun _ _ _ _ [fa, ids]) = do
  (ExprPath faPath)  <- cExpr s fa
  idsPath <- cExpr s ids
  -- liftIO . putStrLn $ "extracting sequences from " ++ faPath
  let tmpDir  = cfgTmpDir cfg </> "cache" </> "seqio"
      (ExprPath outPath) = hashedTmp cfg e []
      tmpList = scriptTmpFile cfg tmpDir e "txt"
  tmpList %> \_ -> do
    liftIO $ createDirectoryIfMissing True tmpDir
    fromShortCutList cfg idsPath (ExprPath tmpList)
  outPath %> \_ -> do
    need [faPath, tmpList]
    liftIO $ createDirectoryIfMissing True tmpDir
    quietly $ cmd "extract_seqs.py" tmpDir outPath faPath tmpList
  return (ExprPath outPath)
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
cConvert :: FilePath -> CutState -> CutExpr -> Rules ExprPath
cConvert script s@(_,cfg) e@(CutFun _ _ _ _ [fa]) = do
  (ExprPath faPath) <- cExpr s fa
  let (ExprPath oPath) = hashedTmp cfg e []
  oPath %> \_ -> need [faPath] >> unit (cmd script oPath faPath)
    -- debugTrackWrite cfg [oPath] TODO is this implied?
  return (ExprPath oPath)
cConvert _ _ _ = error "bad argument to cConvert"

------------------------
-- concat fasta files --
------------------------

concatFastas :: CutFunction
concatFastas = CutFunction
  { fName      = "concat_fastas"
  , fFixity    = Prefix
  , fTypeCheck = tConcatFastas
  , fCompiler  = cConcat
  }

tConcatFastas :: [CutType] -> Either String CutType
tConcatFastas [ListOf x] | elem x [faa, fna] = Right x
tConcatFastas _ = Left "expected a list of fasta files (of the same type)"

-- TODO why is this writing the file paths instead of their contents?
cConcat :: CutState -> CutExpr -> Rules ExprPath
cConcat s@(_,cfg) e@(CutFun _ _ _ _ [fs]) = do
  (ExprPath fsPath) <- cExpr s fs
  let (ExprPath oPath) = hashedTmp cfg e []
  oPath %> \_ -> do
    -- need (debug cfg ("faPaths: " ++ show faPaths) faPaths)
    -- let catCmd = intercalate " " $ ["cat"] ++ faPaths ++ [">", oPath]
    -- unit $ quietly $ cmd Shell (debug cfg ("catCmd: " ++ catCmd) catCmd)
    -- debugTrackWrite cfg [oPath]
    fPaths <- debugReadLines cfg (debug cfg ("fsPath: " ++ fsPath) fsPath)
    need fPaths -- TODO shouldn't the next line handle this?
    txt <- fmap concat $ mapM (debugReadFile cfg) (debug cfg ("fPaths: " ++ show fPaths) fPaths)
    debugWriteFile cfg oPath txt
  return (ExprPath oPath)
cConcat _ _ = error "bad argument to cConcat"
