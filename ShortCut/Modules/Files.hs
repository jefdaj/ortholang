module ShortCut.Modules.Files where

import ShortCut.Core.Types (CutModule(..), CutFunction(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "files"
  , mFunctions = [loadFastaAA, loadFastaNA, loadGenes, loadGenomes]
  }

loadFastaAA :: CutFunction
loadFastaAA = CutFunction
  { fName = "load_fasta_aa"
  , fAccepts = undefined
  , fReturns = undefined
  }

loadFastaNA :: CutFunction
loadFastaNA = CutFunction
  { fName = "load_fasta_na"
  , fAccepts = undefined
  , fReturns = undefined
  }

loadGenes :: CutFunction
loadGenes = CutFunction
  { fName = "load_genes"
  , fAccepts = undefined
  , fReturns = undefined
  }

loadGenomes :: CutFunction
loadGenomes = CutFunction
  { fName = "load_genomes"
  , fAccepts = undefined
  , fReturns = undefined
  }
