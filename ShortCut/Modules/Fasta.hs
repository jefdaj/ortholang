module ShortCut.Modules.Fasta where

-- TODO convert genbank to fasta in this module?

import Development.Shake
import ShortCut.Core.Types

import Data.String.Utils              (strip)
import Development.Shake.FilePath     ((</>))
import ShortCut.Core.Compile          (cacheDir, cExpr, hashedTmp, cProperList)
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

-- Usage: extract-seqs-by-id <tmpdir> <outfasta> <infasta> <idlist>
cExtractSeqs :: CutState -> CutExpr -> Rules FilePath
cExtractSeqs s@(_,cfg) e@(CutFun _ _ _ [fa, ids]) = do
  faPath  <- cExpr s fa
  idsPath <- cExpr s ids
  let estmp   = cacheDir cfg </> "fasta" -- TODO is this needed?
      outPath = hashedTmp cfg e []
  outPath %> \out -> do
    need [faPath, idsPath]
    cmd "extract-seqs-by-id.py" estmp out faPath idsPath
  return outPath
cExtractSeqs _ _ = error "bad argument to extractSeqs"

-------------------------------------------
-- extract sequence IDs from FASTA files --
-------------------------------------------

extractSeqIDs :: CutFunction
extractSeqIDs = CutFunction
  { fName      = "extract_seq_ids"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqIDs
  , fCompiler  = cExtractSeqIDs
  }

tExtractSeqIDs [x] | elem x [faa, fna] = Right (ListOf str)
tExtractSeqIDs _ = Left "expected a fasta file"

-- TODO is there a reason for duplicating cLoad, or was I just being lazy?
--      even if there was it could be refactored in terms of mkLoader right?
-- TODO rewrite this with mkLoader from Compile
cExtractSeqIDs :: CutState -> CutExpr -> Rules FilePath
cExtractSeqIDs s@(_,cfg) expr@(CutFun _ _ _ [f]) = do
  path <- cExpr s f
  let fstmp  = cacheDir cfg </> "fasta" -- not actually used
      tmpIDs = hashedTmp cfg expr ["tmpids"]
      outIDs = hashedTmp cfg expr []
  tmpIDs %> \out -> do
    path' <- fmap strip $ readFile' path
    cmd "extract-seq-ids.py" fstmp out path'
  outIDs %> \out -> cProperList cfg str tmpIDs out
  return outIDs
cExtractSeqIDs _ _ = error "bad argument to cExtractSeqIDs"
