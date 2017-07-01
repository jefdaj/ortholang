module ShortCut.Modules.BlastCRB where

-- TODO any way to prevent it having to remake the mini6803 or whatever
--      query database each time? seems kind of inefficient! but can't put
--      everything in the same folder or it trips over itself in parallel

-- TODO what to do about the e-value cutoff?

import ShortCut.Core.Types
import Development.Shake

import ShortCut.Core.ModuleAPI (aTsvColumn, rSimpleTmp, rMapLastTmp,
                                rMapLastTmps, defaultTypeCheck)
import ShortCut.Core.Compile   (exprDir, scriptTmpFile)
import ShortCut.Modules.SeqIO  (faa, fna)

---------------
-- interface --
---------------

cutModule :: CutModule
cutModule = CutModule
  { mName = "crb-blast"
  , mFunctions =
    [ blastCRB
    , blastCRBAll
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
  , fCompiler  = rSimpleTmp aBlastCRB "crbblast" crb
  }

blastCRBAll :: CutFunction
blastCRBAll = CutFunction
  { fName      = "crb_blast_all"
  , fTypeCheck = defaultTypeCheck [faa, ListOf faa] (ListOf crb)
  , fFixity    = Prefix
  , fCompiler  = rMapLastTmps aBlastCRB "crbblast" crb
  }

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

aBlastCRB :: CutConfig -> [FilePath] -> Action ()
aBlastCRB cfg args@[tmpDir, oPath, qPath, tPath] =
  quietly $ cmd (Cwd tmpDir) "crb-blast"
    [ "--query"  , qPath
    , "--target" , tPath
    , "--output" , oPath
    , "--threads", "8" -- TODO how to pick this?
    , "--split"
    ]
