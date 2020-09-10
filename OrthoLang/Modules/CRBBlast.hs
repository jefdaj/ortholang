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
  , mRules = []
  , mFunctions =
    [ crbBlast
    , crbBlastEach
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

crbBlast :: Function
crbBlast = newFnA2
  "crb_blast"
  (Exactly fna, Some fa "any fasta file")
  (Exactly crb)
  aCRBBlast
  [Nondeterministic]

crbBlastEach :: Function
crbBlastEach = newFnA2
  "crb_blast_each"
  (Exactly fna, ListSigs (Some fa "any fasta file"))
  (Exactly $ ListOf crb)
  (newMap2of2 "crb_blast")
  [Nondeterministic]

{- CRB-BLAST has pretty bad file naming practices, so to prevent conflicts it
 - needs to be run on unique filenames in a unique directory. Also, it only
 - resolves one level of symlink, so we have to point directly to the input
 - files rather than to the canonical $TMPDIR/cache/load... paths.
 -
 - TODO adjust cache paths to be deterministic!
 -}
aCRBBlast :: NewAction2
aCRBBlast (ExprPath out) qPath tPath = do
  cfg <- fmap fromJust getShakeExtra
  let tmpDir = cacheDir cfg "crb-blast"
  let loc = "modules.crbblast.aCRBBlast"
      tmp' = fromPath loc cfg tmpDir
      out' = traceA loc out [tmp', out, qPath, tPath]
  -- get the hashes from the cacnonical path, but can't link to that
  qName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just [tmpdir cfg]) qPath
  tName <- fmap takeFileName $ liftIO $ resolveSymlinks (Just [tmpdir cfg]) tPath
  -- liftIO $ putStrLn $ "qName: " ++ qName
  -- liftIO $ putStrLn $ "tName: " ++ tName
  -- instead, need to link to the actual input files
  qDst <- liftIO $ resolveSymlinks Nothing qPath -- link directly to the file
  tDst <- liftIO $ resolveSymlinks Nothing tPath -- link directly to the file
  -- liftIO $ putStrLn $ "qDst: " ++ qDst
  -- liftIO $ putStrLn $ "tDst: " ++ tDst
  let qSrc  = tmp' </> qDst
      tSrc  = tmp' </> tName
      qSrc' = toPath loc cfg qSrc
      qDstPath = toPath loc cfg qDst
      tSrc' = toPath loc cfg tSrc
      tDstPath = toPath loc cfg tDst
      oPath = tmp' </> "results.crb"
      oPath' = toPath loc cfg oPath
  need' loc [qDst, tDst]
  symlink qSrc' qDstPath
  symlink tSrc' tDstPath
  runCmd $ CmdDesc
    { cmdParallel = False -- TODO true?
    , cmdFixEmpties = True
    , cmdOutPath = oPath
    , cmdInPatterns = [qSrc, tSrc]
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions =[Cwd tmp'] -- TODO remove?
    , cmdBinary = "crb-blast.sh"
    , cmdArguments = [oPath, tmp', qSrc, tSrc]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out, tmp'] -- TODO the whole thing, right?
    }
  symlink (toPath loc cfg out) oPath'
