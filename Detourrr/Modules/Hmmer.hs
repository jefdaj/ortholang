module Detourrr.Modules.Hmmer
  where

import Development.Shake
import Detourrr.Core.Types
import Detourrr.Modules.SeqIO (faa)
import Detourrr.Modules.Muscle (aln)
import Detourrr.Core.Compile.Basic (defaultTypeCheck, rSimple)
import Detourrr.Core.Paths (RrrPath, fromRrrPath)
import Detourrr.Core.Actions (debugA, wrappedCmdWrite, wrappedCmdOut, readLit, readLits, writeLits)
import Data.Scientific (formatScientific, FPFormat(..))
import Data.List (isPrefixOf, nub, sort)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath             (takeFileName, (</>))
import Detourrr.Core.Compile.Map  (rMap)

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "HMMER"
  , mDesc = "Search sequences with hidden Markov models"
  , mTypes = [faa, aln, hmm, hht]
  , mFunctions = [hmmbuild, hmmbuildEach,
                  hmmsearch, hmmsearchEach,
                  extractHmmTargets, extractHmmTargetsEach]
  }

hmm :: RrrType
hmm = RrrType
  { tExt  = "hmm"
  , tDesc = "hidden markov model"
  -- , tShow = \_ _ f -> return $ "hidden markov model '" ++ f ++ "'"
  , tShow = defaultShow
  }

-- TODO add to hit tables types in length, extract_hits etc.
hht :: RrrType
hht = RrrType
  { tExt  = "hht"
  , tDesc = "HMMER hits table"
  , tShow = defaultShow
  }

-- TODO hmmfetch : str -> hmm
-- TODO hmmfetch_each : str.list -> hmm.list

hmmbuild :: RrrFunction
hmmbuild = let name = "hmmbuild" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [aln] hmm
  , fDesc = Nothing, fTypeDesc  = name ++ " : aln -> hmm" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rSimple aHmmbuild
  }

hmmbuildEach :: RrrFunction
hmmbuildEach = let name = "hmmbuild_each" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf aln] (ListOf hmm)
  , fDesc = Nothing, fTypeDesc  = name ++ " : aln.list -> hmm.list" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rMap 1 aHmmbuild
  }

hmmsearch :: RrrFunction
hmmsearch = let name = "hmmsearch" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, hmm, faa] hht
  , fDesc = Nothing, fTypeDesc  = name ++ " : num hmm faa -> hht" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rSimple aHmmsearch
  }

-- TODO is this the right name for mapping over arg 2?
hmmsearchEach :: RrrFunction
hmmsearchEach = let name = "hmmsearch_each" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [num, ListOf hmm, faa] (ListOf hht)
  , fDesc = Nothing, fTypeDesc  = name ++ " : num hmm.list faa -> hht.list" -- TODO generate
  , fFixity    = Prefix
  , fRules     = rMap 2 aHmmsearch
  }

-- TODO better name, or is this actually the most descriptive way?
-- hmmsearchEachEach :: RrrFunction
-- hmmsearchEachEach = let name = "hmmsearch_each_each" in RrrFunction
--   { fName      = name
--   , fTypeCheck = defaultTypeCheck [num, ListOf hmm, ListOf faa] (ListOf $ ListOf hht)
--   , fDesc = Nothing, fTypeDesc  = name ++ " : num hmm.list faa.list -> hht.list.list" -- TODO generate
--   , fFixity    = Prefix
--   , fRules     = rMap 2 aHmmsearch -- TODO this won't work right?
--   }

-- TODO is it parallel?
-- TODO reverse order? currently matches blast fns but not native hmmbuild args
aHmmbuild :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aHmmbuild cfg ref _ [out, fa] = do
  wrappedCmdWrite False True cfg ref out'' [fa'] [] [] "hmmbuild" [out', fa']
  where
    out'  = fromRrrPath cfg out
    out'' = debugA cfg "aHmmbuild" out' [out', fa']
    fa'   = fromRrrPath cfg fa
aHmmbuild _ _ _ args = error $ "bad argument to aHmmbuild: " ++ show args

-- TODO make it parallel and mark as such if possible
aHmmsearch :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aHmmsearch cfg ref _ [out, e, hm, fa] = do
  eStr <- readLit cfg ref e'
  let eDec   = formatScientific Fixed Nothing (read eStr) -- format as decimal
      tmpDir = cfgTmpDir cfg </> "cache" </> "hmmsearch"
      tmpOut = tmpDir </> takeFileName out'
  liftIO $ createDirectoryIfMissing True tmpDir
  wrappedCmdWrite False True cfg ref tmpOut [e', hm', fa'] [] []
    "hmmsearch" ["-E", eDec, "--tblout", tmpOut, hm', fa']
  results <- wrappedCmdOut False True cfg ref [tmpOut] [] [] "sed" ["/^#/d", tmpOut]
  writeLits cfg ref out'' $ lines results
  where
    out'  = fromRrrPath cfg out
    out'' = debugA cfg "aHmmsearch" out' [out', fa']
    e'    = fromRrrPath cfg e
    hm'   = fromRrrPath cfg hm
    fa'   = fromRrrPath cfg fa
aHmmsearch _ _ _ args = error $ "bad argument to aHmmsearch: " ++ show args

extractHmmTargets :: RrrFunction
extractHmmTargets = let name = "extract_hmm_targets" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [hht] (ListOf str)
  , fDesc = Nothing, fTypeDesc  = name ++ " : hht -> str.list"
  , fFixity    = Prefix
  , fRules     = rSimple $ aExtractHmm True 1
  }

extractHmmTargetsEach :: RrrFunction
extractHmmTargetsEach = let name = "extract_hmm_targets_each" in RrrFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf hht] (ListOf $ ListOf str)
  , fDesc = Nothing, fTypeDesc  = name ++ " : hht.list -> str.list.list"
  , fFixity    = Prefix
  , fRules     = rMap 1 $ aExtractHmm True 1
  }

-- TODO clean this up! it's pretty ugly
aExtractHmm :: Bool -> Int -> RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aExtractHmm uniq n cfg ref _ [outPath, tsvPath] = do
  lits <- readLits cfg ref tsvPath'
  let lits'   = filter (\l -> not $ "#" `isPrefixOf` l) lits
      lits''  = if uniq then sort $ nub lits' else lits'
      lits''' = map (\l -> (words l) !! (n - 1)) lits''
  writeLits cfg ref outPath'' lits'''
  where
    outPath'  = fromRrrPath cfg outPath
    outPath'' = debugA cfg "aExtractHmm" outPath' [show n, outPath', tsvPath']
    tsvPath'  = fromRrrPath cfg tsvPath
aExtractHmm _ _ _ _ _ _ = error "bad arguments to aExtractHmm"
