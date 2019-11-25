module ShortCut.Modules.Hmmer
  where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Modules.SeqIO (faa)
import ShortCut.Modules.Muscle (aln)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple, rSimpleScript)
import ShortCut.Core.Paths (CutPath, fromCutPath)
import ShortCut.Core.Actions (traceA, runCmd, CmdDesc(..), readLit)
import Data.Scientific (formatScientific, FPFormat(..))
-- import Data.List (isPrefixOf, nub, sort)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath             (takeFileName, (</>))
import ShortCut.Core.Compile.Map  (rMap, rMapSimpleScript)
import System.Exit (ExitCode(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "HMMER"
  , mDesc = "Search sequences with hidden Markov models"
  , mTypes = [faa, aln, hmm, hht]
  , mFunctions = [hmmbuild, hmmbuildEach,
                  hmmsearch, hmmsearchEach,
                  extractHmmTargets, extractHmmTargetsEach]
  }

hmm :: CutType
hmm = CutType
  { tExt  = "hmm"
  , tDesc = "hidden markov model"
  -- , tShow = \_ _ f -> return $ "hidden markov model '" ++ f ++ "'"
  , tShow = defaultShow
  }

-- TODO add to hit tables types in length, extract_hits etc.
hht :: CutType
hht = CutType
  { tExt  = "hht"
  , tDesc = "HMMER hits table"
  , tShow = defaultShow
  }

-- TODO hmmfetch : str -> hmm
-- TODO hmmfetch_each : str.list -> hmm.list

hmmbuild :: CutFunction
hmmbuild = let name = "hmmbuild" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [aln] hmm
  , fTypeDesc  = name ++ " : aln -> hmm" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rSimpleScript "hmmbuild.sh"
  }

hmmbuildEach :: CutFunction
hmmbuildEach = let name = "hmmbuild_each" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf aln] (ListOf hmm)
  , fTypeDesc  = name ++ " : aln.list -> hmm.list" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rMapSimpleScript 1 "hmmbuild.sh"
  }

hmmsearch :: CutFunction
hmmsearch = let name = "hmmsearch" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, hmm, faa] hht
  , fTypeDesc  = name ++ " : num hmm faa -> hht" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rSimple aHmmsearch
  }

-- TODO is this the right name for mapping over arg 2?
hmmsearchEach :: CutFunction
hmmsearchEach = let name = "hmmsearch_each" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf hmm, faa] (ListOf hht)
  , fTypeDesc  = name ++ " : num hmm.list faa -> hht.list" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rMap 2 aHmmsearch
  }

-- TODO better name, or is this actually the most descriptive way?
-- hmmsearchEachEach :: CutFunction
-- hmmsearchEachEach = let name = "hmmsearch_each_each" in CutFunction
--   { fName      = name
--   , fTypeCheck = defaultTypeCheck [num, ListOf hmm, ListOf faa] (ListOf $ ListOf hht)
--   , fTypeDesc  = name ++ " : num hmm.list faa.list -> hht.list.list" -- TODO generate
--   , fFixity    = Prefix
--   , fRules     = rMap 2 aHmmsearch -- TODO this won't work right?
--   }

-- TODO is it parallel?
-- TODO reverse order? currently matches blast fns but not native hmmbuild args
-- TODO convert to rSimpleScript?
-- aHmmbuild :: CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
-- aHmmbuild cfg ref _ [out, fa] = do
--   wrappedCmdWrite False True cfg ref out'' [fa'] [] [] "hmmbuild" [out', fa']
--   where
--     out'  = fromCutPath cfg out
--     out'' = traceA "aHmmbuild" out' [out', fa']
--     fa'   = fromCutPath cfg fa
-- aHmmbuild _ _ _ args = error $ "bad argument to aHmmbuild: " ++ show args

-- TODO make it parallel and mark as such if possible
aHmmsearch :: CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
aHmmsearch cfg ref _ [out, e, hm, fa] = do
  eStr <- readLit cfg ref e'
  let eDec   = formatScientific Fixed Nothing (read eStr) -- format as decimal

      -- TODO warn users about this? hmmer fails on smaller values than ~1e-307 on my machine
      eMin   = formatScientific Fixed Nothing (read "1e-307")
      eDec'  = if eDec < eMin then eMin else eDec

      tmpDir = cfgTmpDir cfg </> "cache" </> "hmmsearch"
      tmpOut = tmpDir </> takeFileName out'
  liftIO $ createDirectoryIfMissing True tmpDir
  -- wrappedCmdWrite False True cfg ref out'' [e', hm', fa'] [tmpOut] []
  --   "hmmsearch.sh" [out'', eDec', tmpOut, hm', fa']
  runCmd cfg ref $ CmdDesc
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
  where
    out'  = fromCutPath cfg out
    out'' = traceA "aHmmsearch" out' [out', fa']
    e'    = fromCutPath cfg e
    hm'   = fromCutPath cfg hm
    fa'   = fromCutPath cfg fa
aHmmsearch _ _ _ args = error $ "bad argument to aHmmsearch: " ++ show args

extractHmmTargets :: CutFunction
extractHmmTargets = let name = "extract_hmm_targets" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [hht] (ListOf str)
  , fTypeDesc  = name ++ " : hht -> str.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aExtractHmm 1
  }

extractHmmTargetsEach :: CutFunction
extractHmmTargetsEach = let name = "extract_hmm_targets_each" in CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf hht] (ListOf $ ListOf str)
  , fTypeDesc  = name ++ " : hht.list -> str.list.list"
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aExtractHmm 1
  }

-- TODO clean this up! it's pretty ugly
-- TODO how to integrate the script since it needs the colnum?
aExtractHmm :: Int -> CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
aExtractHmm n cfg ref _ [outPath, tsvPath] = do
  -- lits <- readLits ref tsvPath'
  -- let lits'   = filter (\l -> not $ "#" `isPrefixOf` l) lits
  --     lits''  = if uniq then sort $ nub lits' else lits'
  --     lits''' = map (\l -> (words l) !! (n - 1)) lits''
  -- writeLits cfg ref outPath'' lits'''
  -- wrappedCmdWrite False True cfg ref outPath'' [outPath'] [] [] "extract-hmm.py" [outPath', tsvPath', show n]
  runCmd cfg ref $ CmdDesc
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
  where
    outPath'  = fromCutPath cfg outPath
    outPath'' = traceA "aExtractHmm" outPath' [show n, outPath', tsvPath']
    tsvPath'  = fromCutPath cfg tsvPath
aExtractHmm _ _ _ _ _ = fail "bad arguments to aExtractHmm"
