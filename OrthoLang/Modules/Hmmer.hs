module OrthoLang.Modules.Hmmer
  where

-- TODO add hht to ht group?
-- TODO then could make extract_targets work on it too!
-- TODO add to hit tables types in length, extract_hits etc.

-- TODO hmmfetch : str -> hmm
-- TODO hmmfetch_each : str.list -> hmm.list

import Development.Shake
import OrthoLang.Types
import OrthoLang.Interpreter

import Data.Scientific          (formatScientific, FPFormat(..))
import OrthoLang.Modules.Muscle (aln)
import OrthoLang.Modules.SeqIO  (faa)
import System.Directory         (createDirectoryIfMissing)
import System.Exit              (ExitCode(..))
import System.FilePath          (takeFileName, (</>))
import Data.Maybe (fromJust)
import Data.List (isInfixOf)

------------
-- module --
------------

olModule :: Module
olModule = Module
  { mName = "HMMER"
  , mDesc = "Search sequences with hidden Markov models"
  , mTypes = [faa, aln, hmm, hht]
  , mGroups = [], mRules = return ()
  , mEncodings = []
  , mFunctions = [hmmbuild, hmmbuildEach,
                  hmmsearch, hmmsearchEach,
                  extractHmmTargets, extractHmmTargetsEach]
  }

hmm :: Type
hmm = Type
  { tExt  = "hmm"
  , tDesc = "hidden markov model"
  -- , tShow = \_ _ f -> return $ "hidden markov model \"" ++ f ++ "\""
  , tShow = \c r p -> let rmDate = unlines . filter (\l -> not ("DATE" `isInfixOf` l)) . lines
                      in defaultShowN 13 c r p >>= return . rmDate
  }

hht :: Type
hht = Type
  { tExt  = "hht"
  , tDesc = "HMMER hits table"
  , tShow = defaultShow
  }

---------------
-- hmmbuild* --
---------------

hmmbuild :: Function
hmmbuild = newFnS1
  "hmmbuild"
  (Exactly aln)
  (Exactly hmm)
  "hmmbuild.sh"
  [] -- TODO nondeterministic?
  id

hmmbuildEach :: Function
hmmbuildEach = newFnA1
  "hmmbuild_each"
  (Exactly $ ListOf aln)
  (Exactly $ ListOf hmm)
  (newMap1of1 "hmmbuild")
  [] -- TODO nondeterministic?

----------------
-- hmmsearch* --
----------------

hmmsearch :: Function
hmmsearch = newFnA3
  "hmmsearch"
  (Exactly num, Exactly hmm, Exactly faa)
  (Exactly hht)
  aHmmsearch
  [] -- TODO nondeterministic?

hmmsearchEach :: Function
hmmsearchEach = newFnA3
  "hmmsearch_each"
  (Exactly num, Exactly (ListOf hmm), Exactly faa)
  (Exactly $ ListOf hht) -- TODO this should be a list right?
  (newMap2of3 "hmmsearch")
  [] -- TODO nondeterministic?

-- TODO make it parallel and mark as such if possible
aHmmsearch :: NewAction3
aHmmsearch (ExprPath out) ePath hmPath faPath = do
  cfg <- fmap fromJust getShakeExtra
  -- let out'  = fromPath loc cfg out
  let loc = "modules.hmmer.aHmmsearch"
      out'' = traceA loc out [out, faPath]
      -- e'    = fromPath loc cfg e
      -- hm'   = fromPath loc cfg hm
      -- fa'   = fromPath loc cfg fa
  eStr <- readLit loc ePath
  let eDec   = formatScientific Fixed Nothing (read eStr) -- format as decimal

      -- TODO warn users about this? hmmer fails on smaller values than ~1e-307 on my machine
      eMin   = formatScientific Fixed Nothing (read "1e-307")
      eDec'  = if eDec < eMin then eMin else eDec

      tmpDir = tmpdir cfg </> "cache" </> "hmmsearch"
      tmpOut = tmpDir </> takeFileName out
  liftIO $ createDirectoryIfMissing True tmpDir
  -- wrappedCmdWrite False True cfg ref out'' [ePath, hmPath, faPath] [tmpOut] []
  --   "hmmsearch.sh" [out'', eDec', tmpOut, hmPath, faPath]
  runCmd $ CmdDesc
    { cmdBinary = "hmmsearch.sh"
    , cmdArguments = [out'', eDec', tmpOut, hmPath, faPath]
    , cmdFixEmpties = True
    , cmdParallel = False
    , cmdOptions = []
    , cmdInPatterns = [ePath, hmPath, faPath]
    , cmdNoNeedDirs = []
    , cmdOutPath = out''
    , cmdExtraOutPaths = [tmpOut]
    , cmdSanitizePaths = [out'']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out'', tmpOut]
    }

--------------------------
-- extract_hmm_targets* --
--------------------------

extractHmmTargets :: Function
extractHmmTargets = newFnA1
  "extract_hmm_targets"
  (Exactly hht)
  (Exactly $ ListOf str)
  (aExtractHmm 1)
  []

extractHmmTargetsEach :: Function
extractHmmTargetsEach = newFnA1
  "extract_hmm_targets_each"
  (Exactly $ ListOf hht)
  (Exactly $ ListOf $ ListOf str)
  (newMap1of1 "extract_hmm_targets")
  []

-- TODO any good way to pass an arg without making a custom action fn?
aExtractHmm :: Int -> NewAction1
aExtractHmm n (ExprPath outPath) tsvPath = do
  -- let loc = "modules.hmmer.aExtractHmm"
  let outPath'' = traceA "aExtractHmm" outPath [show n, outPath, tsvPath]
  runCmd $ CmdDesc
    { cmdBinary = "extract-hmm.py"
    , cmdArguments = [outPath, tsvPath, show n]
    , cmdParallel = False
    , cmdFixEmpties = True
    , cmdOptions = []
    , cmdInPatterns = []
    , cmdNoNeedDirs = []
    , cmdOutPath = outPath''
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [] -- TODO sanitize outpath?
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath]
    }
