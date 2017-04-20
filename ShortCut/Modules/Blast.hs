module ShortCut.Modules.Blast where

import ShortCut.Core.Types (CutModule(..), CutFunction(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions = [filterGenes, filterGenomes, worstBestEvalue]
  }

filterGenes :: CutFunction
filterGenes = CutFunction
  { fName = "filter_genes"
  , fAccepts = undefined
  , fReturns = undefined
  }

filterGenomes :: CutFunction
filterGenomes = CutFunction
  { fName = "filter_genomes"
  , fAccepts = undefined
  , fReturns = undefined
  }

worstBestEvalue :: CutFunction
worstBestEvalue = CutFunction
  { fName = "worst_best_evalue"
  , fAccepts = undefined
  , fReturns = undefined
  }
