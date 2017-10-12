module ShortCut.Modules.BlastCRB where

-- TODO expose the e-value cutoff, since it is an option? does it make a difference?

import ShortCut.Core.Types
import Development.Shake       -- (quietly, Action, CmdOption(..), need, unit)
import Development.Shake.FilePath ((</>), (<.>), takeExtension)
import System.Directory (createDirectoryIfMissing)
import ShortCut.Core.Paths     (CutPath, fromCutPath, hashContent)
-- import ShortCut.Core.Util      (digest)
import ShortCut.Core.Config    (wrappedCmd)
import ShortCut.Core.Compile.Basic (rSimpleTmp)
import ShortCut.Core.Compile.Map (rMapTmp)
import ShortCut.Modules.SeqIO  (faa, fna)
import ShortCut.Core.Debug (debugAction, debugTrackWrite)

cutModule :: CutModule
cutModule = CutModule
  { mName = "crb-blast"
  , mFunctions =
    [ blastCRB
    , blastCRBEach -- TODO someting nicer than this!
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
  , fRules     = rSimpleTmp "crbblast" aBlastCRB
  }

-- TODO hey can you pass it the entire blastCRB fn instead so it also gets the name?
-- and then you can dispense with ll the rest of this stuff! it's just `mkEach blastCRB`
blastCRBEach :: CutFunction
blastCRBEach = CutFunction
  { fName      = "crb_blast_each"
  , fTypeCheck = tCrbBlastEach
  , fFixity    = Prefix
  , fRules     = rMapTmp aBlastCRB "crbblast"
  }

-- TODO split into two functions with different type signatures?
tCrbBlast :: [CutType] -> Either String CutType
tCrbBlast [x, y] | x == fna && y `elem` [fna, faa] = Right crb
tCrbBlast _ = Left "crb_blast requires a fna query and fna or faa target"

tCrbBlastEach :: [CutType] -> Either String CutType
tCrbBlastEach [x, ListOf y] | x == fna && y `elem` [fna, faa] = Right (ListOf crb)
tCrbBlastEach _ = Left "crb_blast_each requires a fna query and a list of fna or faa targets"

aBlastCRB :: CutConfig -> CutPath -> [CutPath] -> Action ()
aBlastCRB cfg tmpDir [o, q, t] = do
  -- CRB-BLAST has pretty bad file naming practices, so to prevent
  -- conflicts it needs to be run on unique filenames in a unique directory.
  -- TODO do we need to check for existence of the files first?
  need $ map (fromCutPath cfg) [q, t]
  qHash <- hashContent cfg q
  tHash <- hashContent cfg t
  let tDir  = fromCutPath cfg tmpDir </> qHash ++ "_" ++ tHash
      qLink = tDir </> qHash <.> takeExtension q'
      tLink = tDir </> tHash <.> takeExtension t'
      oPath = tDir </> "results.crb"
  liftIO $ putStrLn $ "tDir: " ++ tDir
  liftIO $ createDirectoryIfMissing True tDir
  unit $ quietly $ wrappedCmd cfg [q'] [] "ln" ["-fs", q', qLink]
  unit $ quietly $ wrappedCmd cfg [t'] [] "ln" ["-fs", t', tLink]
  debugTrackWrite cfg [qLink, tLink]
  -- Once that's set up we can finally run it.
  -- TODO relative path from actual out path to the hashed cached one?
  quietly $ wrappedCmd cfg [o'] [Cwd tDir]
    "crb-blast" ["--query", qLink, "--target", tLink, "--output", oPath]
  debugTrackWrite cfg [oPath]
  unit $ quietly $ wrappedCmd cfg [t'] [] "ln" ["-fs", oPath, o'']
  debugTrackWrite cfg [o'']
  where
    o'   = fromCutPath cfg o
    o''  = debugAction cfg "aBlastCRB" o' [fromCutPath cfg tmpDir, o', q', t']
    q'   = fromCutPath cfg q
    t'   = fromCutPath cfg t
aBlastCRB _ _ args = error $ "bad argument to aBlastCRB: " ++ show args
