module ShortCut.Modules.Blast where

import ShortCut.Core.Types
import ShortCut.Modules.Files (faa, fna, gen, gom)

csv = CutType "csv" "spreadsheet"

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions = [filterGenes, filterGenomes, worstBestEvalue]
  }

filterGenes :: CutFunction
filterGenes = CutFunction
  { fName = "filter_genes"
  , fAccepts = [SetOf gen, SetOf gom, num]
  , fReturns = SetOf gen
  }

filterGenomes :: CutFunction
filterGenomes = CutFunction
  { fName = "filter_genomes"
  , fAccepts = [SetOf gom, SetOf gen, num]
  , fReturns = SetOf gom
  }

worstBestEvalue :: CutFunction
worstBestEvalue = CutFunction
  { fName = "worst_best_evalue"
  , fAccepts = [SetOf gen, SetOf gom]
  , fReturns = num
  }
