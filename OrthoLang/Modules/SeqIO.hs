-- TODO rename something more general like SeqUtils?
-- TODO when running gbk_to_faa*, also load_faa the result to split out the IDs!
-- TODO gbk_to_fna (and probably others) need to substitute seqid_* hashes

module OrthoLang.Modules.SeqIO where

import Development.Shake

import OrthoLang.Core.Types
-- import OrthoLang.Core.Config (debug)

import OrthoLang.Core.Util          (digest)
import OrthoLang.Core.Actions       (readPaths, traceA, need', readLit,
                                    writeCachedLines, runCmd, CmdDesc(..), readPaths, writeCachedVersion)
import OrthoLang.Core.Paths         (toOrthoLangPath, fromOrthoLangPath, OrthoLangPath, cacheDir)
import OrthoLang.Core.Sanitize      (lookupIDsFile)
import OrthoLang.Core.Compile.Basic (defaultTypeCheck, rSimple, rSimpleScript, aSimpleScriptNoFix)
import OrthoLang.Core.Compile.Map  (rMap, rMapSimpleScript)
import System.FilePath             ((</>), (<.>), takeDirectory, takeFileName)
import System.Directory            (createDirectoryIfMissing)
import OrthoLang.Modules.Load       (mkLoaders)
import System.Exit                 (ExitCode(..))

orthoLangModule :: OrthoLangModule
orthoLangModule = OrthoLangModule
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
    ++ mkLoaders False gbk -- TODO should seqids be hashed here too?
  }

gbk :: OrthoLangType
gbk = OrthoLangType
  { tExt  = "gbk"
  , tDesc = "genbank"
  , tShow = defaultShow
  }

fa :: OrthoLangType
fa = OrthoLangTypeGroup
  { tgExt = "fa"
  , tgDesc  = "FASTA (nucleic OR amino acid)"
  , tgMember = \t -> t `elem` [fna, faa]
  }

faa :: OrthoLangType
faa = OrthoLangType
  { tExt  = "faa"
  , tDesc = "FASTA (amino acid)"
  , tShow = defaultShow
  }

fna :: OrthoLangType
fna = OrthoLangType
  { tExt  = "fna"
  , tDesc = "FASTA (nucleic acid)"
  , tShow = defaultShow
  }

--------------
-- gbk_to_* --
--------------

-- TODO should these automatically fill in the "CDS" string?

gbkToFaa :: OrthoLangFunction
gbkToFaa = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [str, gbk] faa
  , fTypeDesc  = mkTypeDesc name  [str, gbk] faa
  , fFixity    = Prefix, fTags = []
  , fNewRules = Nothing, fOldRules = rSimple $ aGenbankToFasta faa "aa"
  }
  where
    name = "gbk_to_faa"

-- TODO need to hash IDs afterward!
gbkToFaaEach :: OrthoLangFunction
gbkToFaaEach = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [str, ListOf gbk] (ListOf faa)
  , fTypeDesc  = mkTypeDesc name  [str, ListOf gbk] (ListOf faa)
  , fFixity    = Prefix, fTags = []
  , fNewRules = Nothing, fOldRules = rMap 2 $ aGenbankToFasta faa "aa"
  }
  where
    name = "gbk_to_faa_each"

gbkToFna :: OrthoLangFunction
gbkToFna = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [str, gbk] fna
  , fTypeDesc  = mkTypeDesc name  [str, gbk] fna
  , fFixity    = Prefix, fTags = []
  , fNewRules = Nothing, fOldRules = rSimple $ aGenbankToFasta fna "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_fna"

gbkToFnaEach :: OrthoLangFunction
gbkToFnaEach = OrthoLangFunction
  { fNames     = [name]
  , fTypeCheck = defaultTypeCheck [str, ListOf gbk] (ListOf fna)
  , fTypeDesc  = mkTypeDesc name  [str, ListOf gbk] (ListOf fna)
  , fFixity    = Prefix, fTags = []
  , fNewRules = Nothing, fOldRules = rMap 2 $ aGenbankToFasta fna "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_fna_each"

-- TODO error if no features extracted since it probably means a wrong ft string
-- TODO silence the output? or is it helpful?
aGenbankToFasta :: OrthoLangType -> String
                -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
aGenbankToFasta rtn st cfg ref _ [outPath, ftPath, faPath] = do
  let faPath'   = fromOrthoLangPath cfg faPath
      ftPath'   = fromOrthoLangPath cfg ftPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = fromOrthoLangPath cfg $ cacheDir cfg "seqio"
      outDir'   = exprDir' </> "load_" ++ extOf rtn
      outPath'  = fromOrthoLangPath cfg outPath
      outPath'' = traceA "aGenbankToFasta" outPath' [outPath', faPath']
  -- liftIO $ putStrLn $ "ftPath': " ++ show ftPath'
  ft <- readLit cfg ref ftPath'
  let ft' = if ft  == "cds" then "CDS" else ft
      (st', extraArgs) = if ft' == "whole" then ("whole", ["--annotations", "all"]) else (st, [])
      args = [ "--in_file", faPath'
             , "--out_file", outPath'
             , "--sequence_type", st'
             , "--feature_type", ft'] ++ extraArgs
  -- liftIO $ putStrLn $ "args: " ++ show args
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
extractIds :: OrthoLangFunction
extractIds = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = tExtractIds
  , fTypeDesc  = name ++ " : fa -> str.list"
  , fNewRules = Nothing, fOldRules = rSimpleScript "extract_ids.py"
  }
  where
    name = "extract_ids"

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractIdsEach :: OrthoLangFunction
extractIdsEach = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = tExtractIdsEach
  , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fNewRules = Nothing, fOldRules = rMapSimpleScript 1 "extract_ids.py"
  }
  where
    name = "extract_ids_each"

tExtractIds :: [OrthoLangType] -> Either String OrthoLangType
tExtractIds [x] | elem x [faa, fna] = Right (ListOf str)
tExtractIds _ = Left "expected a fasta file"

tExtractIdsEach :: [OrthoLangType] -> Either String OrthoLangType
tExtractIdsEach [ListOf x] | elem x [faa, fna] = Right (ListOf $ ListOf str)
tExtractIdsEach _ = Left "expected a fasta file"

-------------------------
-- extract_seqs(_each) --
-------------------------

-- TODO also extract them from genbank files

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractSeqs :: OrthoLangFunction
extractSeqs = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = tExtractSeqs
  , fTypeDesc  = name ++ " : fa str.list -> fa"
  , fNewRules = Nothing, fOldRules = rSimple aExtractSeqs 
  }
  where
    name = "extract_seqs"

aExtractSeqs :: OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ()
aExtractSeqs cfg ref ids [outPath, inFa, inList] = do
  let cDir     = fromOrthoLangPath cfg $ cacheDir cfg "seqio"
      tmpList' = cDir </> digest inList <.> "txt"
      tmpList  = toOrthoLangPath cfg tmpList'
  liftIO $ createDirectoryIfMissing True cDir
  lookupIDsFile cfg ref ids inList tmpList
  aSimpleScriptNoFix "extract_seqs.py" cfg ref ids [outPath, inFa, tmpList]
aExtractSeqs _ _ _ ps = error $ "bad argument to aExtractSeqs: " ++ show ps

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractSeqsEach :: OrthoLangFunction
extractSeqsEach = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = tExtractSeqsEach
  , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fNewRules = Nothing, fOldRules = rMap 1 aExtractSeqs
  }
  where
    name = "extract_seqs_each"

tExtractSeqs  :: [OrthoLangType] -> Either String OrthoLangType
tExtractSeqs [x, ListOf s] | s == str && elem x [faa, fna] = Right x
tExtractSeqs _ = Left "expected a fasta file and a list of strings"

tExtractSeqsEach  :: [OrthoLangType] -> Either String OrthoLangType
tExtractSeqsEach [x, ListOf (ListOf s)]
  | s == str && elem x [faa, fna] = Right $ ListOf x
tExtractSeqsEach _ = Left "expected a fasta file and a list of strings"

----------------------
-- translate(_each) --
----------------------

-- TODO name something else like fna_to_faa?
translate :: OrthoLangFunction
translate = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = defaultTypeCheck [fna] faa
  , fTypeDesc  = mkTypeDesc name  [fna] faa
  , fNewRules = Nothing, fOldRules = rSimpleScript "translate.py"
  }
  where
    name = "translate"

translateEach :: OrthoLangFunction
translateEach = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = defaultTypeCheck [ListOf fna] (ListOf faa)
  , fTypeDesc  = mkTypeDesc name  [ListOf fna] (ListOf faa)
  , fNewRules = Nothing, fOldRules = rMapSimpleScript 1 "translate.py"
  }
  where
    name = "translate_each"

--------------
-- concat_* --
--------------

-- TODO separate concat module?

mkConcat :: OrthoLangType -> OrthoLangFunction
mkConcat cType = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = defaultTypeCheck [ListOf cType] cType
  , fTypeDesc  = mkTypeDesc name  [ListOf cType] cType
  , fNewRules = Nothing, fOldRules = rSimple $ aConcat cType
  }
  where
    ext  = extOf cType
    name = "concat_" ++ ext

mkConcatEach :: OrthoLangType -> OrthoLangFunction
mkConcatEach cType = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = defaultTypeCheck [ListOf $ ListOf cType] (ListOf cType)
  , fTypeDesc  = mkTypeDesc name  [ListOf $ ListOf cType] (ListOf cType)
  , fNewRules = Nothing, fOldRules = rMap 1 $ aConcat cType
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
-- aConcat :: OrthoLangType -> OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ()
-- aConcat cType cfg ref ids [oPath, fsPath] = do
--   fPaths <- readPaths cfg ref fs'
--   let fPaths' = map (fromOrthoLangPath cfg) fPaths
--   need' cfg ref "aConcat" fPaths'
--   let out'    = fromOrthoLangPath cfg oPath
--       out''   = traceA "aConcat" out' [out', fs']
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
--     fs' = fromOrthoLangPath cfg fsPath
-- aConcat _ _ _ _ = fail "bad argument to aConcat"

-- TODO WHY DID THIS BREAK CREATING THE CACHE/PSIBLAST DIR? FIX THAT TODAY, QUICK!
aConcat :: OrthoLangType -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
aConcat cType cfg ref ids [outPath, inList] = do
  -- This is all so we can get an example <<emptywhatever>> to cat.py
  -- ... there's gotta be a simpler way right?
  let tmpDir'   = cfgTmpDir cfg </> "cache" </> "concat"
      emptyPath = tmpDir' </> ("empty" ++ extOf cType) <.> "txt"
      emptyStr  = "<<empty" ++ extOf cType ++ ">>"
      inList'   = tmpDir' </> digest inList <.> "txt" -- TODO is that right?
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True $ takeDirectory $ fromOrthoLangPath cfg outPath
  writeCachedLines cfg ref emptyPath [emptyStr]
  inPaths <- readPaths cfg ref $ fromOrthoLangPath cfg inList
  let inPaths' = map (fromOrthoLangPath cfg) inPaths
  need' cfg ref "ortholang.modules.seqio.aConcat" inPaths'
  writeCachedLines cfg ref inList' inPaths'
  aSimpleScriptNoFix "cat.py" cfg ref ids [ outPath
                                      , toOrthoLangPath cfg inList'
                                      , toOrthoLangPath cfg emptyPath]
aConcat _ _ _ _ _ = fail "bad argument to aConcat"

-- writeCachedLines cfg ref outPath content = do

-- TODO would it work to just directly creat a string and tack onto paths here?
-- aSimpleScript' :: Bool -> String -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
-- aSimpleScript' fixEmpties script cfg ref (out:ins) = aSimple' cfg ref ids out actFn Nothing ins

------------------------
-- split_fasta(_each) --
------------------------

splitFasta :: OrthoLangType -> OrthoLangFunction
splitFasta faType = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = defaultTypeCheck [faType] (ListOf faType)
  , fTypeDesc  = mkTypeDesc name  [faType] (ListOf faType)
  , fNewRules = Nothing, fOldRules = rSimple $ aSplit name ext
  }
  where
    ext  = extOf faType
    name = "split_" ++ ext

splitFastaEach :: OrthoLangType -> OrthoLangFunction
splitFastaEach faType = OrthoLangFunction
  { fNames     = [name]
  , fFixity    = Prefix, fTags = []
  , fTypeCheck = defaultTypeCheck [ListOf faType] (ListOf $ ListOf faType)
  , fTypeDesc  = mkTypeDesc name  [ListOf faType] (ListOf $ ListOf faType)
  , fNewRules = Nothing, fOldRules = rMap 1 $ aSplit name ext -- TODO is 1 wrong?
  }
  where
    ext  = extOf faType
    name = "split_" ++ ext ++ "_each"

aSplit :: String -> String -> (OrthoLangConfig -> Locks -> HashedIDsRef -> [OrthoLangPath] -> Action ())
aSplit name ext cfg ref _ [outPath, faPath] = do
  let faPath'   = fromOrthoLangPath cfg faPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = cfgTmpDir cfg </> "cache" </> name -- TODO is there a fn for this?
      prefix'   = tmpDir' </> digest faPath ++ "/"
      outDir'   = exprDir' </> "load_" ++ ext
      outPath'  = fromOrthoLangPath cfg outPath
      outPath'' = traceA "aSplit" outPath' [outPath', faPath']
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
