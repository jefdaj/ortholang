module ShortCut.Modules.BlastCRB where

import ShortCut.Core.Types
import Development.Shake
import ShortCut.Core.Parse    (defaultTypeCheck)
import ShortCut.Core.Compile  (cExpr)
import ShortCut.Modules.Fasta (faa, fna)

---------------
-- interface --
---------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "blastcrb"
  , mFunctions = [blastCRB]
  }

crb :: CutType
crb = CutType
  { tExt  = "crb"
  , tDesc = "tab-separated table of conditional reciprocal blast best hits"
  , tCat  = undefined
  }

blastCRB :: CutFunction
blastCRB = CutFunction
  { fName      = "blast_crb"
  , fTypeCheck = defaultTypeCheck [faa, ListOf fna] crb
  , fFixity    = Prefix
  , fCompiler  = cBlastCRB
  }

--------------------
-- implementation --
--------------------

-- an old blast function for reference:
-- cFilterGenes :: CutState -> CutExpr -> Rules FilePath
-- cFilterGenes s@(_,cfg) e@(CutFun _ _ _ [gens, goms, sci]) = do
--   -- liftIO $ putStrLn "entering cFilterGenes"
--   genes   <- cExpr s gens
--   genomes <- cExpr s goms
--   evalue  <- cExpr s sci
--   let hits   = hashedTmp' cfg csv e [genes, genomes]
--       faa'   = hashedTmp' cfg faa e [genes, "extractseqs"]
--       genes' = hashedTmp  cfg e [hits, evalue]
--       fgtmp  = cacheDir cfg </> "fgtmp" -- TODO remove? not actually used
--   -- TODO extract-seqs-by-id first, and pass that to filter_genes.R
--   faa' %> extractFastaSeqs cfg genes
--   hits %> bblast cfg faa' genomes
--   genes' %> \out -> do
--     need [genomes, hits, evalue]
--     quietly $ cmd "filter_genes.R" [fgtmp, out, genomes, hits, evalue]
--   return genes'
-- cFilterGenes _ _ = error "bad argument to cFilterGenes"

-- output columns:
-- query - the name of the transcript from the 'query' fasta file
-- target - the name of the transcript from the 'target' fasta file
-- id - the percent sequence identity
-- alnlen - the alignment length
-- evalue - the blast evalue
-- bitscore - the blast bitscore
-- qstart..qend - the coordinates of the alignment in the query from start to end
-- tstart..tend - the coordinates of the alignment in the target from start to end 
-- qlen - the length of the query transcript
-- tlen - the length of the target transcript

-- TODO finish this
cBlastCRB :: CutState -> CutExpr -> Rules FilePath
cBlastCRB s@(script,config) e@(CutFun _ _ _ [query, targets]) = do
  qPath  <- cExpr s query
  tPaths <- cExpr s targets
  let res = undefined
  res %> \out -> do
    need [qPath, tPaths]
    quietly $ cmd "crb-blast" ["-q", qPath, "-t", "-o"]
  return undefined

-- TODO version with e-value cutoff?
-- TODO versions with other query and target types (or should the one function be variable?)
