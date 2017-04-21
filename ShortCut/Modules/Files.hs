module ShortCut.Modules.Files where

import ShortCut.Core.Types

-- TODO break this into core (set loading) + fns in each other module?

gen = CutType "gene"   "gene"   -- TODO deprecate
gom = CutType "genome" "genome" -- TODO deprecate
faa = CutType "faa"    "fasta amino acid"
fna = CutType "fna"    "fasta nucleic acid"

cutModule :: CutModule
cutModule = CutModule
  { mName = "files"
  , mFunctions = [loadFastaAA, loadFastaNA, loadGenes, loadGenomes]
  }

loadFastaAA :: CutFunction
loadFastaAA = CutFunction
  { fName = "load_fasta_aa"
  , fAccepts = [str]
  , fReturns = faa
  }

loadFastaNA :: CutFunction
loadFastaNA = CutFunction
  { fName = "load_fasta_na"
  , fAccepts = [str]
  , fReturns = fna
  }

loadGenes :: CutFunction
loadGenes = CutFunction
  { fName = "load_genes"
  , fAccepts = [str]
  , fReturns = gen
  }

loadGenomes :: CutFunction
loadGenomes = CutFunction
  { fName = "load_genomes"
  , fAccepts = [str]
  , fReturns = gom
  }
