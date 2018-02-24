module ShortCut.Modules.BlastCRB where

-- TODO expose the e-value cutoff, since it is an option?
--      does it make a difference?

import ShortCut.Core.Types
import Development.Shake

import Development.Shake.FilePath  ((</>), takeFileName)
import ShortCut.Core.Actions       (wrappedCmdWrite, symlink)
import ShortCut.Core.Paths         (toCutPath)
import ShortCut.Core.Compile.Basic (rSimpleTmp)
import ShortCut.Core.Compile.Each  (rEachTmps)
import ShortCut.Core.Debug         (debugAction)
import ShortCut.Core.Paths         (CutPath, fromCutPath)
import ShortCut.Core.Util          (resolveSymlinks)
import ShortCut.Modules.SeqIO      (faa, fna)
import System.Directory            (createDirectoryIfMissing)
-- import System.Posix.Files          (readSymbolicLink)

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
  { fName      = "crb_blast"
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
  , fRules     = rEachTmps aBlastCRB "crbblast"
  }

-- TODO split into two functions with different type signatures?
tCrbBlast :: [CutType] -> Either String CutType
tCrbBlast [x, y] | x == fna && y `elem` [fna, faa] = Right crb
tCrbBlast _ = Left "crb_blast requires a fna query and fna or faa target"

tCrbBlastEach :: [CutType] -> Either String CutType
tCrbBlastEach [x, ListOf y] | x == fna && y `elem` [fna, faa] = Right (ListOf crb)
tCrbBlastEach _ = Left "crb_blast_each requires a fna query and a list of fna or faa targets"

{- CRB-BLAST has pretty bad file naming practices, so to prevent conflicts it
 - needs to be run on unique filenames in a unique directory. Also, it only
 - resolves one level of symlink, so we have to point directly to the input
 - files rather than to the canonical $TMPDIR/cache/load... paths.
 -}
aBlastCRB :: CutConfig -> CutPath -> [CutPath] -> Action ()
aBlastCRB cfg tmpDir [o, q, t] = do
  need [q', t']
  -- get the hashes from the cacnonical path, but can't link to that
  qName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) q'
  tName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) t'
  -- instead, need to link to the actual input files
  qDst <- liftIO $ resolveSymlinks Nothing q' -- link directly to the file
  tDst <- liftIO $ resolveSymlinks Nothing t' -- link directly to the file
  let qSrc  = tmp' </> qName
      tSrc  = tmp' </> tName
      qSrc' = toCutPath cfg qSrc
      qDst' = toCutPath cfg qDst
      tSrc' = toCutPath cfg tSrc
      tDst' = toCutPath cfg tDst
      oPath = tmp' </> "results.crb" -- TODO what's `need`ing this? stop it! ahh, symlink
      oPath' = toCutPath cfg oPath
      -- oRel' = tmpLink cfg o' oPath
  liftIO $ createDirectoryIfMissing True tmp'
  -- These links aren't required, just helpful for a sane tmpfile tree.
  -- But if used, they have to have file extensions for some reason.
  -- Otherwise you get "Too many positional arguments".
  -- unit $ wrappedCmd cfg [q'] [] "ln" ["-fs", qDst, qSrc]
  -- unit $ wrappedCmd cfg [t'] [] "ln" ["-fs", tDst, tSrc]
  -- debugTrackWrite cfg [qSrc, tSrc]
  need [qDst, tDst]
  symlink cfg qSrc' qDst'
  symlink cfg tSrc' tDst'
  -- debugTrackWrite cfg [qSrc, tSrc] -- TODO why doesn't symlnk handle these??
  wrappedCmdWrite cfg oPath [qSrc, tSrc] [o'] [Cwd tmp'] "crb-blast" [ "-q", qSrc, "-t", tSrc, "-o", oPath]
  -- debugTrackWrite cfg [oPath]
  -- unit $ quietly $ wrappedCmd cfg [o''] [] "ln" ["-fs", oRel', o'']
  -- debugTrackWrite cfg [o'']
  symlink cfg o'' oPath'
  -- debugTrackWrite cfg [o'] --- TODO why doesn't symlink handle this??
  where
    o'   = fromCutPath cfg o
    o''  = debugAction cfg "aBlastCRB" o [fromCutPath cfg tmpDir, o', q', t']
    tmp' = fromCutPath cfg tmpDir
    q'   = fromCutPath cfg q
    t'   = fromCutPath cfg t
aBlastCRB _ _ args = error $ "bad argument to aBlastCRB: " ++ show args
