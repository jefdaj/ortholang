module OrthoLang.Modules.CRBBlast where

-- TODO expose the e-value cutoff, since it is an option?
--      does it make a difference?

import OrthoLang.Types
import OrthoLang.Interpreter
import Development.Shake

import Development.Shake.FilePath ((</>), takeFileName)
import OrthoLang.Modules.SeqIO    (faa, fna, fa)
import System.Exit                (ExitCode(..))
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "CRB-BLAST"
  , mDesc = "Conditional reciprocal BLAST best hits (Aubry et al. 2014)"
  , mTypes = [fna, faa, crb]
  , mGroups = [fa]
  , mEncodings = []
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
-- TODO or maybe there should be a separate "reciprocal best" type?
crb :: Type
crb = Type
  { tExt  = "crb"
  , tDesc = "tab-separated table of conditional reciprocal blast best hits"
  , tShow  = defaultShow
  }

blastCRB :: Function
blastCRB = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly fna, Some fa "any fasta file"]
  , fOutput = Exactly crb
  , fTags = [Stochastic]
  , fNewRules = NewNotImplemented
  , fOldRules = rSimpleTmp name aCRBBlast
  }
  where
    name = "crb_blast"

-- TODO hey can you pass it the entire blastCRB fn instead so it also gets the name?
-- and then you can dispense with ll the rest of this stuff! it's just `mkEach blastCRB`
blastCRBEach :: Function
blastCRBEach = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly fna, ListSigs (Some fa "any fasta file")]
  , fOutput = Exactly (ListOf crb)
  , fTags = [Stochastic]
  , fNewRules = NewNotImplemented, fOldRules = rMapTmps 2 aCRBBlast "crb_blast"
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
aCRBBlast :: Path -> [Path] -> Action ()
aCRBBlast tmpDir [o, q, t] = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.crbblast.aCRBBlast"
      o'   = fromPath loc cfg o
      o''  = traceA loc o [fromPath loc cfg tmpDir, o', q', t']
      tmp' = fromPath loc cfg tmpDir
      q'   = fromPath loc cfg q
      t'   = fromPath loc cfg t
  need' loc [q', t']
  -- get the hashes from the cacnonical path, but can't link to that
  qName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just $ tmpdir cfg) q'
  tName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just $ tmpdir cfg) t'
  -- liftIO $ putStrLn $ "qName: " ++ qName
  -- liftIO $ putStrLn $ "tName: " ++ tName
  -- instead, need to link to the actual input files
  qDst <- liftIO $ resolveSymlinks Nothing q' -- link directly to the file
  tDst <- liftIO $ resolveSymlinks Nothing t' -- link directly to the file
  -- liftIO $ putStrLn $ "qDst: " ++ qDst
  -- liftIO $ putStrLn $ "tDst: " ++ tDst
  let qSrc  = tmp' </> qDst
      tSrc  = tmp' </> tName
      qSrc' = toPath loc cfg qSrc
      qDst' = toPath loc cfg qDst
      tSrc' = toPath loc cfg tSrc
      tDst' = toPath loc cfg tDst
      oPath = tmp' </> "results.crb"
      oPath' = toPath loc cfg oPath
  need' loc [qDst, tDst]
  symlink qSrc' qDst'
  symlink tSrc' tDst'
  runCmd $ CmdDesc
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
  symlink o'' oPath'
aCRBBlast _ args = error $ "bad argument to aCRBBlast: " ++ show args
