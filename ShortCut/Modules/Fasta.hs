module ShortCut.Modules.Fasta where

-- TODO convert genbank to fasta in this module?

import ShortCut.Core.Types
import ShortCut.Core.Parse (typeError)
import Development.Shake
import ShortCut.Modules.Load (mkLoad, mkLoadList)
import Text.PrettyPrint.HughesPJClass (text)
import ShortCut.Core.Compile (cacheDir, cExpr, hashedTmp)
import Development.Shake.FilePath ((</>))

cutModule :: CutModule
cutModule = CutModule
  { mName = "fasta"
  , mFunctions =
    [ mkLoad "load_faa" faa
    , mkLoad "load_fna" fna
    , mkLoad "load_genes"    (ListOf gen) -- TODO replace with cExtractFastaSeqIDs?
    , mkLoad "load_genomes"  (ListOf gom)
    , mkLoadList "load_csvs" csv -- TODO remove once list loading works
    , extractSeqs
    , extractSeqIDs
    ]
  }

-- TODO remove
gen :: CutType
gen = CutType
  { tExt  = "gene"
  , tDesc = "gene"
  , tCat  = undefined
  }

-- TODO remove
gom :: CutType
gom = CutType
  { tExt  = "genome"
  , tDesc = "genome"
  , tCat  = undefined
  }

-- TODO use tsv instead
-- TODO obviously, better printing that says # rows and stuff
-- TODO remove in favor of specific types per tsv format
csv :: CutType
csv = CutType
  { tExt  = "csv"
  , tDesc = "spreadsheet"
  , tCat  = return . text . unlines . (++ ["..."]) . take 5 . lines
  }

faa :: CutType
faa = CutType
  { tExt  = "faa"
  , tDesc = "fasta amino acid"
  , tCat  = undefined
  }

fna :: CutType
fna = CutType
  { tExt  = "fna"
  , tDesc = "fasta nucleic acid"
  , tCat  = undefined
  }

extractSeqs :: CutFunction
extractSeqs = CutFunction
  { fName      = "extract_fasta_seqs"
  , fFixity    = Prefix
  , fTypeCheck = tExtractFastaSeqs
  , fCompiler  = cExtractFastaSeqs
  }

tExtractFastaSeqs [ListOf str, faa] = Right faa
tExtractFastaSeqs [ListOf str, fna] = Right fna
tExtractFastaSeqs _ = Left "expected a list of strings and a fasta file"

cExtractFastaSeqs :: CutState -> CutExpr -> Rules FilePath
cExtractFastaSeqs s@(_,cfg) e@(CutFun _ _ _ [fa, ids]) = do
  faPath  <- cExpr s fa
  idsPath <- cExpr s ids
  let estmp   = cacheDir cfg </> "fasta" -- TODO is this needed?
      outPath = hashedTmp cfg e []
  outPath %> \out -> do
    need [faPath, idsPath]
    -- TODO needs to take faPath as an argument
    cmd "extract-seqs-by-id.py" estmp faPath idsPath
  return outPath
cExtractFastaSeqs _ _ = error "bad argument to extractSeqs"

extractSeqIDs :: CutFunction
extractSeqIDs = CutFunction
  { fName      = "extract_fasta_seq_ids"
  , fFixity    = Prefix
  , fTypeCheck = tExtractFastaSeqIDs
  , fCompiler  = cExtractFastaSeqIDs
  }

tExtractFastaSeqIDs [x] | elem x [faa, fna] = Right (ListOf str)
tExtractFastaSeqIDs _ = Left "expected a fasta file"

-- TODO is there a reason for duplicating cLoad, or was I just being lazy?
--      even if there was it could be refactored in terms of mkLoader right?
-- TODO rewrite this with mkLoader from Compile
cExtractFastaSeqIDs :: CutState -> CutExpr -> Rules FilePath
cExtractFastaSeqIDs s@(_,cfg) expr@(CutFun _ _ _ [f]) = do
  -- liftIO $ putStrLn "entering cExtractFastaSeqIDs"
  -- TODO this shouldn't be needed because this fn will be starting from a gom,
  --      not a str; mkLoader or cLoad or whatever handles str -> gom
  path <- cExpr s f
  let fstmp = cacheDir cfg </> "fasta" -- not actually used
      genes = hashedTmp cfg expr []
  genes %> \out -> do
    need [path]
    path' <- readFile' path
    quietly $ cmd "extract-seq-ids.py" fstmp out path'
  return genes
cExtractFastaSeqIDs _ _ = error "bad argument to cExtractFastaSeqIDs"
