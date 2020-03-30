module OrthoLang.Modules.CRBBlast where

-- TODO expose the e-value cutoff, since it is an option?
--      does it make a difference?

import OrthoLang.Core
import Development.Shake

import Development.Shake.FilePath  ((</>), takeFileName)
import OrthoLang.Core       (runCmd, CmdDesc(..), symlink, traceA, need')
import OrthoLang.Core         (toPath)
import OrthoLang.Core (defaultTypeCheck)
import OrthoLang.Core (rSimpleTmp)
import OrthoLang.Core  (rMapTmps)
-- import OrthoLang.Core         (traceA)
import OrthoLang.Core         (Path, fromPath)
import OrthoLang.Core          (resolveSymlinks)
import OrthoLang.Modules.SeqIO      (faa, fna, fa)
import System.Exit (ExitCode(..))

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "CRB-BLAST"
  , mDesc = "Conditional reciprocal BLAST best hits (Aubry et al. 2014)"
  , mTypes = [fna, faa, fa, crb]
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

-- TODO remove if these are exactly blast hit tables
crb :: Type
crb = Type
  { tExt  = "crb"
  , tDesc = "tab-separated table of conditional reciprocal blast best hits"
  , tShow  = defaultShow
  }

blastCRB :: Function
blastCRB = Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc name  [fna, fa] crb
  , fTypeCheck = defaultTypeCheck [fna, fa] crb
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimpleTmp name aCRBBlast
  }
  where
    name = "crb_blast"

-- TODO hey can you pass it the entire blastCRB fn instead so it also gets the name?
-- and then you can dispense with ll the rest of this stuff! it's just `mkEach blastCRB`
blastCRBEach :: Function
blastCRBEach = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck [fna, ListOf fa] (ListOf crb)
  , fTypeDesc  = mkTypeDesc name  [fna, ListOf fa] (ListOf crb)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMapTmps 2 aCRBBlast "crb_blast"
  }
  where
    name = "crb_blast_each"

{- CRB-BLAST has pretty bad file naming practices, so to prevent conflicts it
 - needs to be run on unique filenames in a unique directory. Also, it only
 - resolves one level of symlink, so we have to point directly to the input
 - files rather than to the canonical $TMPDIR/cache/load... paths.
 -
 - TODO adjust cache paths to be deterministic!
 -}
aCRBBlast :: Config -> LocksRef -> IDsRef -> Path -> [Path] -> Action ()
aCRBBlast cfg ref _ tmpDir [o, q, t] = do
  need' cfg ref "ortholang.modules.crbblast.aCRBBlast" [q', t']
  -- get the hashes from the cacnonical path, but can't link to that
  qName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) q'
  tName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) t'
  -- instead, need to link to the actual input files
  qDst <- liftIO $ resolveSymlinks Nothing q' -- link directly to the file
  tDst <- liftIO $ resolveSymlinks Nothing t' -- link directly to the file
  let qSrc  = tmp' </> qName
      tSrc  = tmp' </> tName
      qSrc' = toPath cfg qSrc
      qDst' = toPath cfg qDst
      tSrc' = toPath cfg tSrc
      tDst' = toPath cfg tDst
      oPath = tmp' </> "results.crb"
      oPath' = toPath cfg oPath
  need' cfg ref "ortholang.core.modules.crbblast.aCRBBlast" [qDst, tDst]
  symlink cfg ref qSrc' qDst'
  symlink cfg ref tSrc' tDst'
  runCmd cfg ref $ CmdDesc
    { cmdParallel = False -- TODO true?
    , cmdFixEmpties = True
    , cmdOutPath = oPath
    , cmdInPatterns = [qSrc, tSrc]
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[Cwd tmp'] -- TODO remove?
    , cmdBinary = "crb-blast.sh"
    , cmdArguments = [oPath, tmp', qSrc, tSrc]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [o', tmp'] -- TODO the whole thing, right?
    }
  symlink cfg ref o'' oPath'
  where
    o'   = fromPath cfg o
    o''  = traceA "aCRBBlast" o [fromPath cfg tmpDir, o', q', t']
    tmp' = fromPath cfg tmpDir
    q'   = fromPath cfg q
    t'   = fromPath cfg t
aCRBBlast _ _ _ _ args = error $ "bad argument to aCRBBlast: " ++ show args
