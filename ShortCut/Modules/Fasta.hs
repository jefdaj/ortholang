module ShortCut.Modules.Fasta where

-- TODO convert genbank to fasta in this module?

import Development.Shake
import ShortCut.Core.Types

import Data.String.Utils              (strip)
import Development.Shake.FilePath     ((</>))
import ShortCut.Core.Compile          (cacheDir, cExpr, hashedTmp, toShortCutList, fromShortCutList, scriptTmpFile)
import ShortCut.Core.Parse            (typeError)
import ShortCut.Modules.Load          (mkLoad, mkLoadList)
import Text.PrettyPrint.HughesPJClass (text)
import System.FilePath            (makeRelative)

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

cExtractSeqIDs :: CutState -> CutExpr -> Rules FilePath
cExtractSeqIDs s@(_,cfg) expr@(CutFun _ _ _ [fa]) = do
  faPath <- cExpr s fa
  let faTmp  = cacheDir cfg </> "fasta"
      tmpOut = scriptTmpFile faTmp expr "txt"
      actOut = hashedTmp cfg expr []
  tmpOut %> \out -> do
    need [faPath]
    quietly $ cmd "extract-seq-ids.py" faTmp out faPath
    -- trackWrite [out]
  actOut %> \out -> toShortCutList s str tmpOut out
  return actOut
cExtractSeqIDs _ _ = error "bad argument to cExtractSeqIDs"

----------------------------------------------
-- extract sequences from FASTA files by ID --
----------------------------------------------

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

cExtractSeqs :: CutState -> CutExpr -> Rules FilePath
cExtractSeqs s@(_,cfg) e@(CutFun _ _ _ [fa, ids]) = do
  faPath  <- cExpr s fa
  liftIO . putStrLn $ "extracting sequences from " ++ faPath
  idsPath <- cExpr s ids
  let faTmp   = cacheDir cfg </> "fasta"
      outPath = hashedTmp cfg e []
      tmpList = scriptTmpFile faTmp e "txt"
  tmpList %> \out -> fromShortCutList cfg faTmp idsPath out
  outPath %> \out -> do
    need [faPath, tmpList]
    quietly $ cmd "extract-seqs-by-id.py" faTmp out faPath tmpList
  return outPath
mExtractSeqs _ _ = error "bad argument to extractSeqs"
