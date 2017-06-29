module ShortCut.Modules.BlastCRB where

-- TODO any way to prevent it having to remake the mini6803 or whatever
--      query database each time? seems kind of inefficient! but can't put
--      everything in the same folder or it trips over itself in parallel

-- TODO what to do about the e-value cutoff?

import ShortCut.Core.Types
import Development.Shake
import ShortCut.Core.Parse    (defaultTypeCheck)
import ShortCut.Core.Compile  (cExpr, scriptTmp, hashedTmp', toShortCutList)
import ShortCut.Modules.Fasta (faa, fna)
import System.Directory       (createDirectoryIfMissing)
import Development.Shake.FilePath ((</>))
import ShortCut.Modules.Vectorize (vectorize)

---------------
-- interface --
---------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "blastcrb"
  , mFunctions =
    [ blastCRB
    , blastCRBAll
    , extractCrbQueries
    , extractCrbTargets
    ]
  }

crb :: CutType
crb = CutType
  { tExt  = "crb"
  , tDesc = "tab-separated table of conditional reciprocal blast best hits"
  , tCat  = defaultCat
  }

----------------------
-- basic crb search --
----------------------

blastCRB :: CutFunction
blastCRB = CutFunction
  { fName      = "crb_blast" -- TODO match the other no-underscore blast binaries?
  , fTypeCheck = defaultTypeCheck [faa, faa] crb
  , fFixity    = Prefix
  , fCompiler  = cBlastCRB
  }

blastCRBAll :: CutFunction
blastCRBAll = vectorize blastCRB "crb_blast_all"

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

cBlastCRB :: CutState -> CutExpr -> Rules FilePath
cBlastCRB s@(scr,cfg) e@(CutFun _ _ _ [query, target]) = do
  qPath <- cExpr s query
  tPath <- cExpr s target
  let crbTmp  = scriptTmp cfg (cfgTmpDir cfg </> "cache" </> "crbblast") e []
      outPath = hashedTmp' cfg crb e []
  outPath %> \out -> do
    need [qPath, tPath]
    liftIO $ createDirectoryIfMissing True crbTmp
    quietly $ cmd (Cwd crbTmp) "crb-blast"
      [ "--query"  , qPath
      , "--target" , tPath
      , "--output" , out
      , "--threads", "8" -- TODO how to pick this?
      , "--split"
      ]
  return outPath

-- TODO version with e-value cutoff?

-------------------------------
-- list query or target hits --
-------------------------------

extractCrbQueries :: CutFunction
extractCrbQueries = CutFunction
  { fName      = "extract_crb_queries"
  , fTypeCheck = defaultTypeCheck [crb] (ListOf str)
  , fFixity    = Prefix
  , fCompiler  = cExtractCrbColumn 1
  }

extractCrbTargets :: CutFunction
extractCrbTargets = CutFunction
  { fName      = "extract_crb_targets"
  , fTypeCheck = defaultTypeCheck [crb] (ListOf str)
  , fFixity    = Prefix
  , fCompiler  = cExtractCrbColumn 2
  }

cExtractCrbColumn :: Int -> CutState -> CutExpr -> Rules FilePath
cExtractCrbColumn n s@(_,cfg) e@(CutFun _ _ _ [hits]) = do
  hitsPath <- cExpr s hits
  let tmpPath = hashedTmp' cfg str e []
      outPath = hashedTmp' cfg (ListOf str) e []
  tmpPath %> \out -> do
    need [hitsPath]
    let awkCmd = "awk '{print $" ++ show n ++ "}'"
    Stdout strs <- quietly $ cmd Shell awkCmd hitsPath
    writeFile' out strs
  outPath %> \out -> toShortCutList s str tmpPath out
  return outPath
extractCrbColumn _ _ _ = error "bad argument to extractCrbColumn"
