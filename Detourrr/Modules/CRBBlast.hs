module Detourrr.Modules.BlastCRB where

-- TODO expose the e-value cutoff, since it is an option?
--      does it make a difference?

import Detourrr.Core.Types
import Development.Shake

import Development.Shake.FilePath  ((</>), takeFileName)
import Detourrr.Core.Actions       (wrappedCmdWrite, symlink, debugA, debugNeed)
import Detourrr.Core.Paths         (toDtrPath)
import Detourrr.Core.Compile.Basic (rSimpleTmp)
import Detourrr.Core.Compile.Map  (rMapTmps)
-- import Detourrr.Core.Debug         (debugA)
import Detourrr.Core.Paths         (DtrPath, fromDtrPath)
import Detourrr.Core.Util          (resolveSymlinks)
import Detourrr.Modules.SeqIO      (faa, fna)

dtrModule :: DtrModule
dtrModule = DtrModule
  { mName = "CRB-BLAST"
  , mDesc = "Conditional reciprocal BLAST best hits (Aubry et al. 2014)"
  , mTypes = [fna, faa, crb]
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

crb :: DtrType
crb = DtrType
  { tExt  = "crb"
  , tDesc = "tab-separated table of conditional reciprocal blast best hits"
  , tShow  = defaultShow
  }

blastCRB :: DtrFunction
blastCRB = DtrFunction
  { fName      = "crb_blast"
  , fTypeCheck = tCrbBlast
  , fDesc = Nothing, fTypeDesc  = "crb_blast : fa -> fa -> crb"
  , fFixity    = Prefix
  , fRules     = rSimpleTmp "crbblast" aBlastCRB
  }

-- TODO hey can you pass it the entire blastCRB fn instead so it also gets the name?
-- and then you can dispense with ll the rest of this stuff! it's just `mkEach blastCRB`
blastCRBEach :: DtrFunction
blastCRBEach = DtrFunction
  { fName      = "crb_blast_each"
  , fTypeCheck = tCrbBlastEach
  , fDesc = Nothing, fTypeDesc  = "crb_blast_each : fa -> fa.list -> crb.list"
  , fFixity    = Prefix
  , fRules     = rMapTmps 2 aBlastCRB "crbblast"
  }

-- TODO split into two functions with different type signatures?
-- TODO what types are actually allowed? (can subject be fna?)
tCrbBlast :: [DtrType] -> Either String DtrType
tCrbBlast [x, y] | x `elem` [fna, faa] && y `elem` [fna, faa] = Right crb
tCrbBlast _ = Left "crb_blast requires a fna query and fna or faa target"

tCrbBlastEach :: [DtrType] -> Either String DtrType
tCrbBlastEach [x, ListOf y] | x == fna && y `elem` [fna, faa] = Right (ListOf crb)
tCrbBlastEach _ = Left "crb_blast_each requires a fna query and a list of fna or faa targets"

{- CRB-BLAST has pretty bad file naming practices, so to prevent conflicts it
 - needs to be run on unique filenames in a unique directory. Also, it only
 - resolves one level of symlink, so we have to point directly to the input
 - files rather than to the canonical $TMPDIR/cache/load... paths.
 -}
aBlastCRB :: DtrConfig -> Locks -> HashedSeqIDsRef -> DtrPath -> [DtrPath] -> Action ()
aBlastCRB cfg ref _ tmpDir [o, q, t] = do
  debugNeed cfg "aBlastCRB" [q', t']
  -- get the hashes from the cacnonical path, but can't link to that
  qName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) q'
  tName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) t'
  -- instead, need to link to the actual input files
  qDst <- liftIO $ resolveSymlinks Nothing q' -- link directly to the file
  tDst <- liftIO $ resolveSymlinks Nothing t' -- link directly to the file
  let qSrc  = tmp' </> qName
      tSrc  = tmp' </> tName
      qSrc' = toDtrPath cfg qSrc
      qDst' = toDtrPath cfg qDst
      tSrc' = toDtrPath cfg tSrc
      tDst' = toDtrPath cfg tDst
      oPath = tmp' </> "results.crb"
      oPath' = toDtrPath cfg oPath
  debugNeed cfg "aBlastCRB" [qDst, tDst]
  symlink cfg ref qSrc' qDst'
  symlink cfg ref tSrc' tDst'
  wrappedCmdWrite True True cfg ref oPath [qSrc, tSrc] [] [Cwd tmp'] -- TODO is it parallel?
    "crb-blast" [ "-q", qSrc, "-t", tSrc, "-o", oPath]
  symlink cfg ref o'' oPath'
  where
    o'   = fromDtrPath cfg o
    o''  = debugA cfg "aBlastCRB" o [fromDtrPath cfg tmpDir, o', q', t']
    tmp' = fromDtrPath cfg tmpDir
    q'   = fromDtrPath cfg q
    t'   = fromDtrPath cfg t
aBlastCRB _ _ _ _ args = error $ "bad argument to aBlastCRB: " ++ show args
