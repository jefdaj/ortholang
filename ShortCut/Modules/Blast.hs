module ShortCut.Modules.Blast where

import Development.Shake
import Development.Shake.FilePath ((<.>), (</>))
import ShortCut.Core.Types
import ShortCut.Core.Compile
-- import ShortCut.Modules.Files (faa, fna, gen, gom)

gen = CutType "gene"   "gene"   -- TODO deprecate
gom = CutType "genome" "genome" -- TODO deprecate
faa = CutType "faa"    "fasta amino acid"
fna = CutType "fna"    "fasta nucleic acid"
csv = CutType "csv" "spreadsheet"

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions =
    [ loadFastaAA
    , loadFastaNA
    , loadGenes
    , loadGenomes
    , filterGenes
    , filterGenomes
    , worstBestEvalue
    ]
  }

-- cutModule :: CutModule
-- cutModule = CutModule
  -- { mName = "files"
  -- }

loadFastaAA :: CutFunction
loadFastaAA = CutFunction
  { fName = "load_fasta_aa"
  , fAccepts = [str]
  , fReturns = faa
  , fFixity  = Prefix
  , fCompiler = cLoad
  }

loadFastaNA :: CutFunction
loadFastaNA = CutFunction
  { fName = "load_fasta_na"
  , fAccepts = [str]
  , fReturns = fna
  , fFixity  = Prefix
  , fCompiler = cLoad
  }

loadGenes :: CutFunction
loadGenes = CutFunction
  { fName = "load_genes"
  , fAccepts = [str]
  , fReturns = gen
  , fFixity  = Prefix
  , fCompiler = cLoad
  }

loadGenomes :: CutFunction
loadGenomes = CutFunction
  { fName = "load_genomes"
  , fAccepts = [str]
  , fReturns = gom
  , fFixity  = Prefix
  , fCompiler = cLoad
  }

filterGenes :: CutFunction
filterGenes = CutFunction
  { fName = "filter_genes"
  , fAccepts = [SetOf gen, SetOf gom, num]
  , fReturns = SetOf gen
  , fFixity  = Prefix
  , fCompiler = cFilterGenes
  }

filterGenomes :: CutFunction
filterGenomes = CutFunction
  { fName = "filter_genomes"
  , fAccepts = [SetOf gom, SetOf gen, num]
  , fReturns = SetOf gom
  , fFixity  = Prefix
  , fCompiler = cFilterGenomes
  }

worstBestEvalue :: CutFunction
worstBestEvalue = CutFunction
  { fName = "worst_best_evalue"
  , fAccepts = [SetOf gen, SetOf gom]
  , fReturns = num
  , fFixity  = Prefix
  , fCompiler = cWorstBest
  }

-- TODO is there a reason for duplicating cLoad, or was I just being lazy?
--      even if there was it could be refactored in terms of mkLoader right?
--
-- Ah, it's two functions maybe?
-- 1. Loads a genome file (not genes)
-- 2. extracts genes from it
-- Actually write the cut scripts that way for clarity!
-- You can make a convenience function doing both later if it helps.
--
-- Don't be too afraid to change one or two little script things
-- But also don't go overboard! Gotta be sure can still work!
--
-- TODO rewrite this with mkLoader from Compile
-- TODO should what you've been calling load_genes actually be load_fna/faa?
-- TODO adapt to work with multiple files?
cLoadGenes :: CutConfig -> CutExpr -> Rules FilePath
cLoadGenes cfg expr@(CutFun _ _ [f]) = do
  -- liftIO $ putStrLn "entering cLoadGenes"
  -- TODO this shouldn't be needed because this fn will be starting from a gom,
  --      not a str; mkLoader or cLoad or whatever handles str -> gom
  path <- cExpr cfg f
  let fstmp = cacheDir cfg </> "loadgenes" -- not actually used
      genes = hashedTmp cfg expr []
  genes %> \out -> do
    need [path]
    path' <- readFile' path
    quietly $ cmd "extract-seq-ids.py" fstmp out path'
  return genes
cLoadGenes _ _ = error "bad argument to cLoadGenes"

-- TODO does this need to distinguish FNA from FAA?
extractSeqs :: CmdResult b => CutConfig -> FilePath -> FilePath -> Action b
extractSeqs cfg genes out = do
  -- liftIO $ putStrLn "entering extractseqs"
  let estmp = cacheDir cfg </> "extractseqs"
  need [genes]
  quietly $ cmd "extract-seqs-by-id.py" estmp out genes

bblast :: CmdResult b => CutConfig -> FilePath -> FilePath -> FilePath -> Action b
bblast cfg genes genomes out = do
  -- liftIO $ putStrLn "entering bblast"
  let bbtmp = cacheDir cfg </> "bblast"
  need [genes, genomes]
  -- TODO fix bblast so order doesn't matter here
  -- TODO take a verbosity flag and pass the value on to bblast
  quietly $ cmd "bblast" "-o" out "-d" genomes "-f" genes "-c" "tblastn" "-t" bbtmp

-- TODO factor out bblast!
cFilterGenes :: CutConfig -> CutExpr -> Rules FilePath
cFilterGenes cfg e@(CutFun _ _ [gens, goms, sci]) = do
  -- liftIO $ putStrLn "entering cFilterGenes"
  genes   <- cExpr cfg gens
  genomes <- cExpr cfg goms
  evalue  <- cExpr cfg sci
  let hits   = hashedTmp' cfg csv e [genes, genomes]
      faa'   = hashedTmp' cfg faa e [genes, "extractseqs"]
      genes' = hashedTmp  cfg e [hits, evalue]
      fgtmp  = cacheDir cfg </> "fgtmp" -- TODO remove? not actually used
  -- TODO extract-seqs-by-id first, and pass that to filter_genes.R
  faa' %> extractSeqs cfg genes
  hits %> bblast cfg faa' genomes
  genes' %> \out -> do
    need [genomes, hits, evalue]
    quietly $ cmd "filter_genes.R" [fgtmp, out, genomes, hits, evalue]
  return genes'
cFilterGenes _ _ = error "bad argument to cFilterGenes"

-- TODO factor out bblast!
cFilterGenomes :: CutConfig -> CutExpr -> Rules FilePath
cFilterGenomes cfg e@(CutFun _ _ [goms, gens, sci]) = do
  -- liftIO $ putStrLn "entering cFilterGenomes"
  genomes <- cExpr cfg goms
  genes   <- cExpr cfg gens
  evalue  <- cExpr cfg sci
  let faa'     = hashedTmp' cfg faa e [genes, "extractseqs"]
      hits     = hashedTmp' cfg csv e [genomes, genes]
      genomes' = hashedTmp  cfg e [hits, evalue]
      fgtmp = cacheDir cfg </> "fgtmp" -- TODO remove? not actually used
  faa' %> extractSeqs cfg genes
  hits %> bblast cfg faa' genomes
  genomes' %> \out -> do
    need [genes, hits, evalue]
    quietly $ cmd "filter_genomes.R" [fgtmp, out, genes, hits, evalue]
  return genomes'
cFilterGenomes _ _ = error "bad argument to cFilterGenomes"

cWorstBest :: CutConfig -> CutExpr -> Rules FilePath
cWorstBest cfg e@(CutFun _ _ [gens, goms]) = do
  -- liftIO $ putStrLn "entering cWorstBest"
  genes   <- cExpr cfg gens
  genomes <- cExpr cfg goms
  let faa'   = hashedTmp' cfg faa e [genes, "extractseqs"]
      hits   = hashedTmp' cfg csv e [genomes, genes]
      evalue = hashedTmp  cfg e [genes, genomes]
      wbtmp  = cacheDir cfg </> "wbtmp" -- TODO remove? not actually used
  faa' %> extractSeqs cfg genes
  hits %> bblast cfg faa' genomes
  evalue %> \out -> do
    need [hits, genes]
    quietly $ cmd "worst_best_evalue.R" [wbtmp, out, hits, genes]
  return evalue
cWorstBest _ _ = error "bad argument to cWorstBest"
