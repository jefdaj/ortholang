module ShortCut.Modules.BlastCRB where

-- TODO expose the e-value cutoff, since it is an option? does it make a difference?

import ShortCut.Core.Types
import Development.Shake       (quietly, Action, CmdOption(..))
import ShortCut.Core.Config    (wrappedCmd)
import ShortCut.Core.Compile.Rules (rSimpleTmp, rMapLastTmps)
import ShortCut.Modules.SeqIO  (faa, fna)
import ShortCut.Core.Debug (debugAction, debugTrackWrite)

cutModule :: CutModule
cutModule = CutModule
  { mName = "crb-blast"
  , mFunctions =
    [ blastCRB
    , blastCRBEach
    ]
  }

-- crb columns:
--
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

crb :: CutType
crb = CutType
  { tExt  = "crb"
  , tDesc = "tab-separated table of conditional reciprocal blast best hits"
  , tShow  = defaultShow
  }

blastCRB :: CutFunction
blastCRB = CutFunction
  { fName      = "crb_blast" -- TODO match the other no-underscore blast binaries?
  , fTypeCheck = tCrbBlast
  , fFixity    = Prefix
  , fRules  = rSimpleTmp aBlastCRB "crbblast" crb
  }

blastCRBEach :: CutFunction
blastCRBEach = CutFunction
  { fName      = "crb_blast_each"
  , fTypeCheck = tCrbBlastEach
  , fFixity    = Prefix
  , fRules  = rMapLastTmps aBlastCRB "crbblast" (ListOf crb)
  }

tCrbBlast :: [CutType] -> Either String CutType
tCrbBlast [x, y] | x == fna && y `elem` [fna, faa] = Right crb
tCrbBlast _ = Left "crb_blast requires a fna query and fna or faa target"

tCrbBlastEach :: [CutType] -> Either String CutType
tCrbBlastEach [x, ListOf y] | x == fna && y `elem` [fna, faa] = Right (ListOf crb)
tCrbBlastEach _ = Left "crb_blast requires a fna query and a list of fna or faa targets"

aBlastCRB :: CutConfig -> CacheDir -> [ExprPath] -> Action ()
aBlastCRB cfg (CacheDir tmpDir) [(ExprPath o), (ExprPath q), (ExprPath t)] = do
  quietly $ wrappedCmd cfg [o] [Cwd tmpDir]
    "crb-blast" ["--query", q, "--target", t, "--output", o]
  let o' = debugAction cfg "aBlastCRB" o [tmpDir, o, q, t]
  debugTrackWrite cfg [o']
aBlastCRB _ _ args = error $ "bad argument to aBlastCRB: " ++ show args
