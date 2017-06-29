module ShortCut.Modules.Blast where

-- import Development.Shake
-- import Development.Shake.FilePath ((</>))
-- import ShortCut.Core.Compile
-- import ShortCut.Core.Parse (defaultTypeCheck)

import ShortCut.Core.Types

import ShortCut.Core.ModuleAPI (defaultTypeCheck)
import ShortCut.Modules.Fasta  (faa, fna)

cutModule :: CutModule
cutModule = CutModule
  { mName = "blast"
  , mFunctions =
    [ mkBlastFn  "blastn" fna fna
    , mkBlastFn  "blastp" faa faa
    , mkBlastFn  "blastx" fna faa
    , mkBlastFn "tblastn" faa fna
    , mkBlastFn "tblastx" fna fna
    -- TODO vectorized versions
    -- TODO psiblast, dbiblast, deltablast, rpsblast, rpsblastn?
    -- TODO extract_queries, extract_targets
    ]
  }

bht :: CutType
bht = CutType
  { tExt  = "bht"
  , tDesc = "tab-separated table of reciprocal blast hits"
  , tCat  = defaultCat
  }

-- TODO mkBlast function that deduplicates these

mkBlastFn :: String -> CutType -> CutType -> CutFunction
mkBlastFn name qType tType = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [qType, tType, num] bht
  , fFixity    = Prefix
  , fCompiler  = undefined
  }

-- filterGenes :: CutFunction
-- filterGenes = CutFunction
--   { fName = "filter_genes"
--   , fTypeCheck = defaultTypeCheck [ListOf gen, ListOf gom, num] (ListOf gen)
--   , fFixity  = Prefix
--   , fCompiler = cFilterGenes
--   }
-- 
-- filterGenomes :: CutFunction
-- filterGenomes = CutFunction
--   { fName = "filter_genomes"
--   , fTypeCheck = defaultTypeCheck [ListOf gom, ListOf gen, num] (ListOf gom)
--   , fFixity  = Prefix
--   , fCompiler = cFilterGenomes
--   }
-- 
-- worstBestEvalue :: CutFunction
-- worstBestEvalue = CutFunction
--   { fName = "worst_best_evalue"
--   , fTypeCheck = defaultTypeCheck [ListOf gen, ListOf gom] num
--   , fFixity  = Prefix
--   , fCompiler = cWorstBest
--   }
-- 
-- bblast :: CmdResult b => CutConfig -> FilePath -> FilePath -> FilePath -> Action b
-- bblast cfg genes genomes out = do
--   -- liftIO $ putStrLn "entering bblast"
--   let bbtmp = cacheDir cfg </> "bblast"
--   need [genes, genomes]
--   -- TODO fix bblast so order doesn't matter here
--   -- TODO take a verbosity flag and pass the value on to bblast
--   quietly $ cmd "bblast" "-o" out "-d" genomes "-f" genes "-c" "tblastn" "-t" bbtmp
-- 
-- -- TODO factor out bblast!
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
--   -- faa' %> extractFastaSeqs cfg genes
--   faa' %> undefined -- so I can change extractFastaSeqs
--   hits %> bblast cfg faa' genomes
--   genes' %> \out -> do
--     need [genomes, hits, evalue]
--     quietly $ cmd "filter_genes.R" [fgtmp, out, genomes, hits, evalue]
--   return genes'
-- cFilterGenes _ _ = error "bad argument to cFilterGenes"
-- 
-- -- TODO factor out bblast!
-- cFilterGenomes :: CutState -> CutExpr -> Rules FilePath
-- cFilterGenomes s@(_,cfg) e@(CutFun _ _ _ [goms, gens, sci]) = do
--   -- liftIO $ putStrLn "entering cFilterGenomes"
--   genomes <- cExpr s goms
--   genes   <- cExpr s gens
--   evalue  <- cExpr s sci
--   let faa'     = hashedTmp' cfg faa e [genes, "extractseqs"]
--       hits     = hashedTmp' cfg csv e [genomes, genes]
--       genomes' = hashedTmp  cfg e [hits, evalue]
--       fgtmp = cacheDir cfg </> "fgtmp" -- TODO remove? not actually used
--   -- faa' %> extractFastaSeqs cfg genes
--   faa' %> undefined -- so I can change extractFastaSeqs
--   hits %> bblast cfg faa' genomes
--   genomes' %> \out -> do
--     need [genes, hits, evalue]
--     quietly $ cmd "filter_genomes.R" [fgtmp, out, genes, hits, evalue]
--   return genomes'
-- cFilterGenomes _ _ = error "bad argument to cFilterGenomes"
-- 
-- cWorstBest :: CutState -> CutExpr -> Rules FilePath
-- cWorstBest s@(_,cfg) e@(CutFun _ _ _ [gens, goms]) = do
--   -- liftIO $ putStrLn "entering cWorstBest"
--   genes   <- cExpr s gens
--   genomes <- cExpr s goms
--   let faa'   = hashedTmp' cfg faa e [genes, "extractseqs"]
--       hits   = hashedTmp' cfg csv e [genomes, genes]
--       evalue = hashedTmp  cfg e [genes, genomes]
--       wbtmp  = cacheDir cfg </> "wbtmp" -- TODO remove? not actually used
--   -- faa' %> extractFastaSeqs cfg genes
--   faa' %> undefined -- so I can change extractFastaSeqs
--   hits %> bblast cfg faa' genomes
--   evalue %> \out -> do
--     need [hits, genes]
--     quietly $ cmd "worst_best_evalue.R" [wbtmp, out, hits, genes]
--   return evalue
-- cWorstBest _ _ = error "bad argument to cWorstBest"
