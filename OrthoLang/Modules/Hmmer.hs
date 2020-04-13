module OrthoLang.Modules.Hmmer
  where

import Development.Shake
import OrthoLang.Core

import Data.Scientific          (formatScientific, FPFormat(..))
import OrthoLang.Modules.Muscle (aln)
import OrthoLang.Modules.SeqIO  (faa)
import System.Directory         (createDirectoryIfMissing)
import System.Exit              (ExitCode(..))
import System.FilePath          (takeFileName, (</>))
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "HMMER"
  , mDesc = "Search sequences with hidden Markov models"
  , mTypes = [faa, aln, hmm, hht]
  , mGroups = []
  , mFunctions = [hmmbuild, hmmbuildEach,
                  hmmsearch, hmmsearchEach,
                  extractHmmTargets, extractHmmTargetsEach]
  }

hmm :: Type
hmm = Type
  { tExt  = "hmm"
  , tDesc = "hidden markov model"
  -- , tShow = \_ _ f -> return $ "hidden markov model \"" ++ f ++ "\""
  , tShow = defaultShow
  }

-- TODO add to hit tables types in length, extract_hits etc.
hht :: Type
hht = Type
  { tExt  = "hht"
  , tDesc = "HMMER hits table"
  , tShow = defaultShow
  }

-- TODO hmmfetch : str -> hmm
-- TODO hmmfetch_each : str.list -> hmm.list

hmmbuild :: Function
hmmbuild = let name = "hmmbuild" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [aln] hmm
  -- , fTypeDesc  = name ++ " : aln -> hmm" -- TODO generate
  , fInputs = [Exactly aln]
  , fOutput =  Exactly hmm
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rSimpleScript "hmmbuild.sh"
  }

hmmbuildEach :: Function
hmmbuildEach = let name = "hmmbuild_each" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [ListOf aln] (ListOf hmm)
  -- , fTypeDesc  = name ++ " : aln.list -> hmm.list" -- TODO generate
  , fInputs = [Exactly (ListOf aln)]
  , fOutput =  Exactly (ListOf hmm)
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rMapSimpleScript 1 "hmmbuild.sh"
  }

hmmsearch :: Function
hmmsearch = let name = "hmmsearch" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [num, hmm, faa] hht
  -- , fTypeDesc  = name ++ " : num hmm faa -> hht" -- TODO generate
  , fInputs = [Exactly num, Exactly hmm, Exactly faa] 
  , fOutput =  Exactly hht -- TODO add to ht group?
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple aHmmsearch
  }

-- TODO is this the right name for mapping over arg 2?
hmmsearchEach :: Function
hmmsearchEach = let name = "hmmsearch_each" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [num, ListOf hmm, faa] (ListOf hht)
  -- , fTypeDesc  = name ++ " : num hmm.list faa -> hht.list" -- TODO generate
  , fInputs = [Exactly num, Exactly (ListOf hmm), Exactly faa]
  , fOutput =  Exactly hht
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 2 aHmmsearch
  }

-- TODO better name, or is this actually the most descriptive way?
-- hmmsearchEachEach :: Function
-- hmmsearchEachEach = let name = "hmmsearch_each_each" in Function
--   { fOpChar = Nothing, fName = name
--   , fTypeCheck = defaultTypeCheck name [num, ListOf hmm, ListOf faa] (ListOf $ ListOf hht)
--   , fTypeDesc  = name ++ " : num hmm.list faa.list -> hht.list.list" -- TODO generate
--   ,fTags = []
--   , fNewRules = NewNotImplemented, fOldRules = rMap 2 aHmmsearch -- TODO this won't work right?
--   }

-- TODO is it parallel?
-- TODO reverse order? currently matches blast fns but not native hmmbuild args
-- TODO convert to rSimpleScript?
-- aHmmbuild :: [Path] -> Action ()
-- aHmmbuild cfg ref _ [out, fa] = do
--   wrappedCmdWrite False True cfg ref out'' [fa'] [] [] "hmmbuild" [out', fa']
--   where
--     out'  = fromPath loc cfg out
--     out'' = traceA "aHmmbuild" out' [out', fa']
--     fa'   = fromPath loc cfg fa
-- aHmmbuild _ _ _ args = error $ "bad argument to aHmmbuild: " ++ show args

-- TODO make it parallel and mark as such if possible
aHmmsearch :: [Path] -> Action ()
aHmmsearch [out, e, hm, fa] = do
  cfg <- fmap fromJust getShakeExtra
  let out'  = fromPath loc cfg out
      loc = "modules.hmmer.aHmmsearch"
      out'' = traceA loc out' [out', fa']
      e'    = fromPath loc cfg e
      hm'   = fromPath loc cfg hm
      fa'   = fromPath loc cfg fa
  eStr <- readLit loc e'
  let eDec   = formatScientific Fixed Nothing (read eStr) -- format as decimal

      -- TODO warn users about this? hmmer fails on smaller values than ~1e-307 on my machine
      eMin   = formatScientific Fixed Nothing (read "1e-307")
      eDec'  = if eDec < eMin then eMin else eDec

      tmpDir = cfgTmpDir cfg </> "cache" </> "hmmsearch"
      tmpOut = tmpDir </> takeFileName out'
  liftIO $ createDirectoryIfMissing True tmpDir
  -- wrappedCmdWrite False True cfg ref out'' [e', hm', fa'] [tmpOut] []
  --   "hmmsearch.sh" [out'', eDec', tmpOut, hm', fa']
  runCmd $ CmdDesc
    { cmdBinary = "hmmsearch.sh"
    , cmdArguments = [out'', eDec', tmpOut, hm', fa']
    , cmdFixEmpties = True
    , cmdParallel = False
    , cmdOptions = []
    , cmdInPatterns = [e', hm', fa']
    , cmdOutPath = out''
    , cmdExtraOutPaths = [tmpOut]
    , cmdSanitizePaths = [out'']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out'', tmpOut]
    }
aHmmsearch args = error $ "bad argument to aHmmsearch: " ++ show args

extractHmmTargets :: Function
extractHmmTargets = let name = "extract_hmm_targets" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [hht] (ListOf str)
  -- , fTypeDesc  = name ++ " : hht -> str.list"
  , fInputs = [Exactly hht]
  , fOutput = Exactly (ListOf str)
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aExtractHmm 1
  }

extractHmmTargetsEach :: Function
extractHmmTargetsEach = let name = "extract_hmm_targets_each" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [ListOf hht] (ListOf $ ListOf str)
  -- , fTypeDesc  = name ++ " : hht.list -> str.list.list"
  , fInputs = [Exactly (ListOf hht)]
  , fOutput = Exactly (ListOf (ListOf str))
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 1 $ aExtractHmm 1
  }

-- TODO clean this up! it's pretty ugly
-- TODO how to integrate the script since it needs the colnum?
aExtractHmm :: Int -> [Path] -> Action ()
aExtractHmm n [outPath, tsvPath] = do
  -- lits <- readLits tsvPath'
  -- let lits'   = filter (\l -> not $ "#" `isPrefixOf` l) lits
  --     lits''  = if uniq then sort $ nub lits' else lits'
  --     lits''' = map (\l -> (words l) !! (n - 1)) lits''
  -- writeLits outPath'' lits'''
  -- wrappedCmdWrite False True cfg ref outPath'' [outPath'] [] [] "extract-hmm.py" [outPath', tsvPath', show n]
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.hmmer.aExtractHmm"
      outPath'  = fromPath loc cfg outPath
      outPath'' = traceA "aExtractHmm" outPath' [show n, outPath', tsvPath']
      tsvPath'  = fromPath loc cfg tsvPath
  runCmd $ CmdDesc
    { cmdBinary = "extract-hmm.py"
    , cmdArguments = [outPath', tsvPath', show n]
    , cmdParallel = False
    , cmdFixEmpties = True
    , cmdOptions = []
    , cmdInPatterns = []
    , cmdOutPath = outPath''
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [] -- TODO sanitize outpath?
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath']
    }
aExtractHmm _ _ = fail "bad arguments to aExtractHmm"
