-- TODO rename something more general like SeqUtils?
-- TODO when running gbk_to_faa*, also load_faa the result to split out the IDs!

module ShortCut.Modules.SeqIO where

import Development.Shake

import ShortCut.Core.Types
-- import ShortCut.Core.Config (debug)

import ShortCut.Core.Util          (digest)
import ShortCut.Core.Actions       (readPaths, debugA, debugNeed, readLit,
                                    writeCachedLines, runCmd, CmdDesc(..), readPaths, writeCachedVersion)
import ShortCut.Core.Paths         (toCutPath, fromCutPath, CutPath, cacheDir)
import ShortCut.Core.Sanitize      (lookupIDsFile)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple, rSimpleScript, aSimpleScriptNoFix)
import ShortCut.Core.Compile.Map  (rMap, rMapSimpleScript)
import System.FilePath             ((</>), (<.>), takeDirectory, takeFileName)
import System.Directory            (createDirectoryIfMissing)
import ShortCut.Modules.Load       (mkLoaders)
import System.Exit                 (ExitCode(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "SeqIO"
  , mDesc = "Sequence file manipulations using BioPython's SeqIO"
  , mTypes = [gbk, faa, fna, fa]
  , mFunctions =
    [ gbkToFaa, gbkToFaaEach
    , gbkToFna, gbkToFnaEach
    , extractSeqs , extractSeqsEach
    , extractIds  , extractIdsEach
    , translate   , translateEach
    , mkConcat fna  , mkConcatEach fna
    , mkConcat faa  , mkConcatEach faa
    , splitFasta faa, splitFastaEach faa
    , splitFasta fna, splitFastaEach fna
    -- TODO combo that loads multiple fnas or faas and concats them?
    -- TODO combo that loads multiple gbks -> fna or faa?
    ]
    ++ mkLoaders True fna
    ++ mkLoaders True faa
    ++ mkLoaders False gbk
  }

gbk :: CutType
gbk = CutType
  { tExt  = "gbk"
  , tDesc = "genbank"
  , tShow = defaultShow
  }

fa :: CutType
fa = CutTypeGroup
  { tgExt = "fa"
  , tgDesc  = "FASTA (nucleic OR amino acid)"
  , tgMember = \t -> t `elem` [fna, faa]
  }

faa :: CutType
faa = CutType
  { tExt  = "faa"
  , tDesc = "FASTA (amino acid)"
  , tShow = defaultShow
  }

fna :: CutType
fna = CutType
  { tExt  = "fna"
  , tDesc = "FASTA (nucleic acid)"
  , tShow = defaultShow
  }

--------------
-- gbk_to_* --
--------------

-- TODO should these automatically fill in the "CDS" string?

gbkToFaa :: CutFunction
gbkToFaa = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str, gbk] faa
  , fTypeDesc  = mkTypeDesc name  [str, gbk] faa
  , fFixity    = Prefix
  -- , fRules     = rSimpleScript "gbk_to_faa.py"
  , fRules     = rSimple $ aGenbankToFasta faa "aa"
  }
  where
    name = "gbk_to_faa"

-- TODO need to hash IDs afterward!
gbkToFaaEach :: CutFunction
gbkToFaaEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str, ListOf gbk] (ListOf faa)
  , fTypeDesc  = mkTypeDesc name  [str, ListOf gbk] (ListOf faa)
  , fFixity    = Prefix
  -- , fRules     = rMapSimpleScript 1 "gbk_to_faa.py"
  , fRules     = rMap 1 $ aGenbankToFasta faa "aa"
  }
  where
    name = "gbk_to_faa_each"

gbkToFna :: CutFunction
gbkToFna = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str, gbk] fna
  , fTypeDesc  = mkTypeDesc name  [str, gbk] fna
  , fFixity    = Prefix
  -- , fRules     = rSimpleScript "gbk_to_fna.py"
  , fRules     = rSimple $ aGenbankToFasta fna "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_fna"

gbkToFnaEach :: CutFunction
gbkToFnaEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str, ListOf gbk] (ListOf fna)
  , fTypeDesc  = mkTypeDesc name  [str, ListOf gbk] (ListOf fna)
  , fFixity    = Prefix
  -- , fRules     = rMapSimpleScript 1 "gbk_to_fna.py"
  , fRules     = rMap 1 $ aGenbankToFasta fna "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_fna_each"

-- TODO silence the output? or is it helpful?
aGenbankToFasta :: CutType -> String
                -> (CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ())
aGenbankToFasta rtn st cfg ref _ [outPath, ftPath, faPath] = do
  let faPath'   = fromCutPath cfg faPath
      ftPath'   = fromCutPath cfg ftPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = fromCutPath cfg $ cacheDir cfg "seqio"
      outDir'   = exprDir' </> "load_" ++ extOf rtn
      outPath'  = fromCutPath cfg outPath
      outPath'' = debugA cfg "aGenbankToFasta" outPath' [outPath', faPath']
  ft <- readLit cfg ref ftPath'
  let ft'       = if ft  == "cds" then "CDS" else ft
      extraArgs = if ft' == "whole" then ["--annotations", "all"] else []
      args = [ "--in_file", faPath'
             , "--out_file", outPath'
             , "--sequence_type", st
             , "--feature_type", ft'] ++ extraArgs
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True outDir'
  runCmd cfg ref $ CmdDesc
    { cmdBinary = "genbank_to_fasta.py"
    , cmdArguments = args
    , cmdFixEmpties = False
    , cmdParallel = False
    , cmdOptions = []
    , cmdInPatterns = [faPath']
    , cmdOutPath = outPath'
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [outPath']
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath'']
    }
aGenbankToFasta _ _ _ _ _ paths = error $ "bad argument to aGenbankToFasta: " ++ show paths

------------------------
-- extract_ids(_each) --
------------------------

-- TODO this needs to do relative paths again, not absolute!
-- TODO also extract them from genbank files

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractIds :: CutFunction
extractIds = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractIds
  , fTypeDesc  = name ++ " : fa -> str.list"
  , fRules     = rSimpleScript "extract_ids.py"
  }
  where
    name = "extract_ids"

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractIdsEach :: CutFunction
extractIdsEach = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractIdsEach
  , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fRules     = rMapSimpleScript 1 "extract_ids.py"
  }
  where
    name = "extract_ids_each"

tExtractIds :: [CutType] -> Either String CutType
tExtractIds [x] | elem x [faa, fna] = Right (ListOf str)
tExtractIds _ = Left "expected a fasta file"

tExtractIdsEach :: [CutType] -> Either String CutType
tExtractIdsEach [ListOf x] | elem x [faa, fna] = Right (ListOf $ ListOf str)
tExtractIdsEach _ = Left "expected a fasta file"

-------------------------
-- extract_seqs(_each) --
-------------------------

-- TODO also extract them from genbank files

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractSeqs :: CutFunction
extractSeqs = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqs
  , fTypeDesc  = name ++ " : fa str.list -> fa"
  , fRules     = rSimple aExtractSeqs 
  }
  where
    name = "extract_seqs"

aExtractSeqs :: CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
aExtractSeqs cfg ref ids [outPath, inFa, inList] = do
  let cDir     = fromCutPath cfg $ cacheDir cfg "seqio"
      tmpList' = cDir </> digest inList <.> "txt"
      tmpList  = toCutPath cfg tmpList'
  liftIO $ createDirectoryIfMissing True cDir
  lookupIDsFile cfg ref ids inList tmpList
  aSimpleScriptNoFix "extract_seqs.py" cfg ref ids [outPath, inFa, tmpList]
aExtractSeqs _ _ _ ps = error $ "bad argument to aExtractSeqs: " ++ show ps

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractSeqsEach :: CutFunction
extractSeqsEach = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqsEach
  , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fRules     = rMap 1 aExtractSeqs
  }
  where
    name = "extract_seqs_each"

tExtractSeqs  :: [CutType] -> Either String CutType
tExtractSeqs [x, ListOf s] | s == str && elem x [faa, fna] = Right x
tExtractSeqs _ = Left "expected a fasta file and a list of strings"

tExtractSeqsEach  :: [CutType] -> Either String CutType
tExtractSeqsEach [x, ListOf (ListOf s)]
  | s == str && elem x [faa, fna] = Right $ ListOf x
tExtractSeqsEach _ = Left "expected a fasta file and a list of strings"

----------------------
-- translate(_each) --
----------------------

-- TODO name something else like fna_to_faa?
translate :: CutFunction
translate = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [fna] faa
  , fTypeDesc  = mkTypeDesc name  [fna] faa
  , fRules     = rSimpleScript "translate.py"
  }
  where
    name = "translate"

translateEach :: CutFunction
translateEach = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf fna] (ListOf faa)
  , fTypeDesc  = mkTypeDesc name  [ListOf fna] (ListOf faa)
  , fRules     = rMapSimpleScript 1 "translate.py"
  }
  where
    name = "translate_each"

--------------
-- concat_* --
--------------

-- TODO separate concat module?

mkConcat :: CutType -> CutFunction
mkConcat cType = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf cType] cType
  , fTypeDesc  = mkTypeDesc name  [ListOf cType] cType
  , fRules     = rSimple $ aConcat cType
  }
  where
    ext  = extOf cType
    name = "concat_" ++ ext

mkConcatEach :: CutType -> CutFunction
mkConcatEach cType = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf $ ListOf cType] (ListOf cType)
  , fTypeDesc  = mkTypeDesc name  [ListOf $ ListOf cType] (ListOf cType)
  , fRules     = rMap 1 $ aConcat cType
  }
  where
    ext  = extOf cType
    name = "concat_" ++ ext ++ "_each"

{- This is just a fancy `cat`, with handling for a couple cases:
 - * some args are empty and their <<emptywhatever>> should be removed
 - * all args are empty and they should be collapsed to one <<emptywhatever>>
 -
 - TODO special case of error handling here, since cat errors are usually temporary?
 -}
-- aConcat :: CutType -> CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
-- aConcat cType cfg ref ids [oPath, fsPath] = do
--   fPaths <- readPaths cfg ref fs'
--   let fPaths' = map (fromCutPath cfg) fPaths
--   debugNeed cfg "aConcat" fPaths'
--   let out'    = fromCutPath cfg oPath
--       out''   = debugA cfg "aConcat" out' [out', fs']
--       outTmp  = out'' <.> "tmp"
--       emptyStr = "<<empty" ++ extOf cType ++ ">>"
--       grepCmd = "egrep -v '^" ++ emptyStr ++ "$'"
--       catArgs = fPaths' ++ ["|", grepCmd, ">", outTmp]
--   wrappedCmdWrite cfg ref outTmp fPaths' [] [Shell] "cat"
--     (debug cfg ("catArgs: " ++ show catArgs) catArgs)
--   needsFix <- isReallyEmpty outTmp
--   if needsFix
--     then liftIO $ writeFile out'' emptyStr
--     else copyFile' outTmp out''
--   where
--     fs' = fromCutPath cfg fsPath
-- aConcat _ _ _ _ = fail "bad argument to aConcat"

-- TODO WHY DID THIS BREAK CREATING THE CACHE/PSIBLAST DIR? FIX THAT TODAY, QUICK!
aConcat :: CutType -> (CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ())
aConcat cType cfg ref ids [outPath, inList] = do
  -- This is all so we can get an example <<emptywhatever>> to cat.py
  -- ... there's gotta be a simpler way right?
  let tmpDir'   = cfgTmpDir cfg </> "cache" </> "concat"
      emptyPath = tmpDir' </> ("empty" ++ extOf cType) <.> "txt"
      emptyStr  = "<<empty" ++ extOf cType ++ ">>"
      inList'   = tmpDir' </> digest inList <.> "txt" -- TODO is that right?
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True $ takeDirectory $ fromCutPath cfg outPath
  writeCachedLines cfg ref emptyPath [emptyStr]
  inPaths <- readPaths cfg ref $ fromCutPath cfg inList
  let inPaths' = map (fromCutPath cfg) inPaths
  debugNeed cfg "aConcat" inPaths'
  writeCachedLines cfg ref inList' inPaths'
  aSimpleScriptNoFix "cat.py" cfg ref ids [ outPath
                                      , toCutPath cfg inList'
                                      , toCutPath cfg emptyPath]
aConcat _ _ _ _ _ = fail "bad argument to aConcat"

-- writeCachedLines cfg ref outPath content = do

-- TODO would it work to just directly creat a string and tack onto paths here?
-- aSimpleScript' :: Bool -> String -> (CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ())
-- aSimpleScript' fixEmpties script cfg ref (out:ins) = aSimple' cfg ref ids out actFn Nothing ins

------------------------
-- split_fasta(_each) --
------------------------

splitFasta :: CutType -> CutFunction
splitFasta faType = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [faType] (ListOf faType)
  , fTypeDesc  = mkTypeDesc name  [faType] (ListOf faType)
  , fRules     = rSimple $ aSplit name ext
  }
  where
    ext  = extOf faType
    name = "split_" ++ ext

splitFastaEach :: CutType -> CutFunction
splitFastaEach faType = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf faType] (ListOf $ ListOf faType)
  , fTypeDesc  = mkTypeDesc name  [ListOf faType] (ListOf $ ListOf faType)
  , fRules     = rMap 1 $ aSplit name ext -- TODO is 1 wrong?
  }
  where
    ext  = extOf faType
    name = "split_" ++ ext ++ "_each"

aSplit :: String -> String -> (CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ())
aSplit name ext cfg ref _ [outPath, faPath] = do
  let faPath'   = fromCutPath cfg faPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = cfgTmpDir cfg </> "cache" </> name -- TODO is there a fn for this?
      prefix'   = tmpDir' </> digest faPath ++ "_"
      outDir'   = exprDir' </> "load_" ++ ext
      outPath'  = fromCutPath cfg outPath
      outPath'' = debugA cfg "aSplit" outPath' [outPath', faPath']
      tmpList   = tmpDir' </> takeFileName outPath' <.> "tmp"
      args      = [tmpList, outDir', prefix', faPath']
  -- TODO make sure stderr doesn't come through?
  -- TODO any locking needed here?
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True outDir'
  -- TODO rewrite with runCmd -> tmpfile, then correct paths afterward in haskell
  -- out <- wrappedCmdOut False True cfg ref [faPath'] [] [] "split_fasta.py" args
  -- TODO why does this work when loaders are called one at a time, but not as part of a big script?
  -- TODO the IDs are always written properly, so why not the sequences??
  -- withWriteLock' ref tmpDir' $ do -- why is this required?
  runCmd cfg ref $ CmdDesc
    { cmdBinary = "split_fasta.py"
    , cmdArguments = args
    , cmdFixEmpties = False -- TODO will be done in the next step right?
    , cmdParallel = True -- TODO make it parallel again?
    , cmdOptions = []
    , cmdInPatterns = [faPath']
    , cmdOutPath = tmpList
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [tmpList]
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath'', tmpList] -- TODO any more?
    }
  -- loadPaths <- readPaths cfg ref tmpList
  -- when (null loadPaths) $ error $ "no fasta file written: " ++ tmpList
  -- writePaths cfg ref outPath'' loadPaths
  writeCachedVersion cfg ref outPath'' tmpList
aSplit _ _ _ _ _ paths = error $ "bad argument to aSplit: " ++ show paths
