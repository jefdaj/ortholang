module OrthoLang.Modules.CRBBlast where

-- TODO expose the e-value cutoff, since it is an option?
--      does it make a difference?

import OrthoLang.Core.Types
import Development.Shake

import Development.Shake.FilePath  ((</>), takeFileName)
import OrthoLang.Core.Actions       (runCmd, CmdDesc(..), symlink, traceA, need')
import OrthoLang.Core.Paths         (toOrthoLangPath)
import OrthoLang.Core.Compile.Basic (rSimpleTmp, defaultTypeCheck)
import OrthoLang.Core.Compile.Map  (rMapTmps)
-- import OrthoLang.Core.Debug         (traceA)
import OrthoLang.Core.Paths         (OrthoLangPath, fromOrthoLangPath)
import OrthoLang.Core.Util          (resolveSymlinks)
import OrthoLang.Modules.SeqIO      (faa, fna, fa)
import System.Exit (ExitCode(..))

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
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
crb :: OrthoLangType
crb = OrthoLangType
  { tExt  = "crb"
  , tDesc = "tab-separated table of conditional reciprocal blast best hits"
  , tShow  = defaultShow
  }

blastCRB :: OrthoLangFunction
blastCRB = OrthoLangFunction
  { fNames     = [name]
  , fTypeDesc  = mkTypeDesc name  [fna, fa] crb
  , fTypeCheck = defaultTypeCheck [fna, fa] crb
  , fFixity    = Prefix
  , fRules     = rSimpleTmp name aCRBBlast
  }
  where
    name = "crb_blast"

-- TODO hey can you pass it the entire blastCRB fn instead so it also gets the name?
-- and then you can dispense with ll the rest of this stuff! it's just `mkEach blastCRB`
blastCRBEach :: OrthoLangFunction
blastCRBEach = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [fna, ListOf fa] (ListOf crb)
  , fTypeDesc  = mkTypeDesc name  [fna, ListOf fa] (ListOf crb)
  , fFixity    = Prefix
  , fRules     = rMapTmps 2 aCRBBlast "crb_blast"
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
aCRBBlast :: OrthoLangConfig -> Locks -> HashedIDsRef -> OrthoLangPath -> [OrthoLangPath] -> Action ()
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
      qSrc' = toOrthoLangPath cfg qSrc
      qDst' = toOrthoLangPath cfg qDst
      tSrc' = toOrthoLangPath cfg tSrc
      tDst' = toOrthoLangPath cfg tDst
      oPath = tmp' </> "results.crb"
      oPath' = toOrthoLangPath cfg oPath
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
    o'   = fromOrthoLangPath cfg o
    o''  = traceA "aCRBBlast" o [fromOrthoLangPath cfg tmpDir, o', q', t']
    tmp' = fromOrthoLangPath cfg tmpDir
    q'   = fromOrthoLangPath cfg q
    t'   = fromOrthoLangPath cfg t
aCRBBlast _ _ _ _ args = error $ "bad argument to aCRBBlast: " ++ show args
