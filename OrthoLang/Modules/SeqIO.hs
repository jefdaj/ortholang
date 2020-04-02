-- TODO rename something more general like SeqUtils?
-- TODO when running gbk_to_faa*, also load_faa the result to split out the IDs!
-- TODO gbk_to_fna (and probably others) need to substitute seqid_* hashes

module OrthoLang.Modules.SeqIO where

import Development.Shake

import OrthoLang.Core
-- import OrthoLang.Core (debug)

import OrthoLang.Core          (digest)
import OrthoLang.Core       (readPaths, traceA, need', readLit,
                                    writeCachedLines, runCmd, CmdDesc(..), readPaths, writeCachedVersion)
import OrthoLang.Core         (toPath, fromPath, Path, cacheDir)
import OrthoLang.Core      (lookupIDsFile)
import OrthoLang.Core (defaultTypeCheck)
import OrthoLang.Core (rSimple, rSimpleScript, aSimpleScriptNoFix)
import OrthoLang.Core  (rMap, rMapSimpleScript)
import System.FilePath             ((</>), (<.>), takeDirectory, takeFileName)
import System.Directory            (createDirectoryIfMissing)
import OrthoLang.Modules.Load       (mkLoaders)
import System.Exit                 (ExitCode(..))
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
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

gbk :: Type
gbk = Type
  { tExt  = "gbk"
  , tDesc = "genbank"
  , tShow = defaultShow
  }

fa :: Type
fa = TypeGroup
  { tgExt = "fa"
  , tgDesc  = "FASTA (nucleic OR amino acid)"
  , tgMember = \t -> t `elem` [fna, faa]
  }

faa :: Type
faa = Type
  { tExt  = "faa"
  , tDesc = "FASTA (amino acid)"
  , tShow = defaultShow
  }

fna :: Type
fna = Type
  { tExt  = "fna"
  , tDesc = "FASTA (nucleic acid)"
  , tShow = defaultShow
  }

--------------
-- gbk_to_* --
--------------

-- TODO should these automatically fill in the "CDS" string?

gbkToFaa :: Function
gbkToFaa = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str, gbk] faa
  , fTypeDesc  = mkTypeDesc name  [str, gbk] faa
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rSimple $ aGenbankToFasta faa "aa"
  }
  where
    name = "gbk_to_faa"

-- TODO need to hash IDs afterward!
gbkToFaaEach :: Function
gbkToFaaEach = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str, ListOf gbk] (ListOf faa)
  , fTypeDesc  = mkTypeDesc name  [str, ListOf gbk] (ListOf faa)
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMap 2 $ aGenbankToFasta faa "aa"
  }
  where
    name = "gbk_to_faa_each"

gbkToFna :: Function
gbkToFna = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str, gbk] fna
  , fTypeDesc  = mkTypeDesc name  [str, gbk] fna
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rSimple $ aGenbankToFasta fna "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_fna"

gbkToFnaEach :: Function
gbkToFnaEach = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str, ListOf gbk] (ListOf fna)
  , fTypeDesc  = mkTypeDesc name  [str, ListOf gbk] (ListOf fna)
  ,fTags = []
  , fNewRules = NewNotImplemented, fOldRules = rMap 2 $ aGenbankToFasta fna "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_fna_each"

-- TODO error if no features extracted since it probably means a wrong ft string
-- TODO silence the output? or is it helpful?
aGenbankToFasta :: Type -> String -> ([Path] -> Action ())
aGenbankToFasta rtn st [outPath, ftPath, faPath] = do
  cfg <- fmap fromJust getShakeExtra
  let faPath'   = fromPath cfg faPath
      ftPath'   = fromPath cfg ftPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = fromPath cfg $ cacheDir cfg "seqio"
      outDir'   = exprDir' </> "load_" ++ extOf rtn
      outPath'  = fromPath cfg outPath
      outPath'' = traceA "aGenbankToFasta" outPath' [outPath', faPath']
  -- liftIO $ putStrLn $ "ftPath': " ++ show ftPath'
  ft <- readLit ftPath'
  let ft' = if ft  == "cds" then "CDS" else ft
      (st', extraArgs) = if ft' == "whole" then ("whole", ["--annotations", "all"]) else (st, [])
      args = [ "--in_file", faPath'
             , "--out_file", outPath'
             , "--sequence_type", st'
             , "--feature_type", ft'] ++ extraArgs
  -- liftIO $ putStrLn $ "args: " ++ show args
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True outDir'
  runCmd $ CmdDesc
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
aGenbankToFasta _ _ paths = error $ "bad argument to aGenbankToFasta: " ++ show paths

------------------------
-- extract_ids(_each) --
------------------------

-- TODO this needs to do relative paths again, not absolute!
-- TODO also extract them from genbank files

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractIds :: Function
extractIds = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = tExtractIds
  , fTypeDesc  = name ++ " : fa -> str.list"
  , fNewRules = NewNotImplemented, fOldRules = rSimpleScript "extract_ids.py"
  }
  where
    name = "extract_ids"

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractIdsEach :: Function
extractIdsEach = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = tExtractIdsEach
  , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fNewRules = NewNotImplemented, fOldRules = rMapSimpleScript 1 "extract_ids.py"
  }
  where
    name = "extract_ids_each"

tExtractIds :: [Type] -> Either String Type
tExtractIds [x] | elem x [faa, fna] = Right (ListOf str)
tExtractIds _ = Left "expected a fasta file"

tExtractIdsEach :: [Type] -> Either String Type
tExtractIdsEach [ListOf x] | elem x [faa, fna] = Right (ListOf $ ListOf str)
tExtractIdsEach _ = Left "expected a fasta file"

-------------------------
-- extract_seqs(_each) --
-------------------------

-- TODO also extract them from genbank files

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractSeqs :: Function
extractSeqs = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = tExtractSeqs
  , fTypeDesc  = name ++ " : fa str.list -> fa"
  , fNewRules = NewNotImplemented, fOldRules = rSimple aExtractSeqs 
  }
  where
    name = "extract_seqs"

aExtractSeqs :: [Path] -> Action ()
aExtractSeqs [outPath, inFa, inList] = do
  cfg <- fmap fromJust getShakeExtra
  let cDir     = fromPath cfg $ cacheDir cfg "seqio"
      tmpList' = cDir </> digest inList <.> "txt"
      tmpList  = toPath cfg tmpList'
  liftIO $ createDirectoryIfMissing True cDir
  lookupIDsFile inList tmpList
  aSimpleScriptNoFix "extract_seqs.py" [outPath, inFa, tmpList]
aExtractSeqs ps = error $ "bad argument to aExtractSeqs: " ++ show ps

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractSeqsEach :: Function
extractSeqsEach = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = tExtractSeqsEach
  , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fNewRules = NewNotImplemented, fOldRules = rMap 1 aExtractSeqs
  }
  where
    name = "extract_seqs_each"

tExtractSeqs  :: [Type] -> Either String Type
tExtractSeqs [x, ListOf s] | s == str && elem x [faa, fna] = Right x
tExtractSeqs _ = Left "expected a fasta file and a list of strings"

tExtractSeqsEach  :: [Type] -> Either String Type
tExtractSeqsEach [x, ListOf (ListOf s)]
  | s == str && elem x [faa, fna] = Right $ ListOf x
tExtractSeqsEach _ = Left "expected a fasta file and a list of strings"

----------------------
-- translate(_each) --
----------------------

-- TODO name something else like fna_to_faa?
translate :: Function
translate = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = defaultTypeCheck name [fna] faa
  , fTypeDesc  = mkTypeDesc name  [fna] faa
  , fNewRules = NewNotImplemented, fOldRules = rSimpleScript "translate.py"
  }
  where
    name = "translate"

translateEach :: Function
translateEach = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = defaultTypeCheck name [ListOf fna] (ListOf faa)
  , fTypeDesc  = mkTypeDesc name  [ListOf fna] (ListOf faa)
  , fNewRules = NewNotImplemented, fOldRules = rMapSimpleScript 1 "translate.py"
  }
  where
    name = "translate_each"

--------------
-- concat_* --
--------------

-- TODO separate concat module?

mkConcat :: Type -> Function
mkConcat cType = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = defaultTypeCheck name [ListOf cType] cType
  , fTypeDesc  = mkTypeDesc name  [ListOf cType] cType
  , fNewRules = NewNotImplemented, fOldRules = rSimple $ aConcat cType
  }
  where
    ext  = extOf cType
    name = "concat_" ++ ext

mkConcatEach :: Type -> Function
mkConcatEach cType = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = defaultTypeCheck name [ListOf $ ListOf cType] (ListOf cType)
  , fTypeDesc  = mkTypeDesc name  [ListOf $ ListOf cType] (ListOf cType)
  , fNewRules = NewNotImplemented, fOldRules = rMap 1 $ aConcat cType
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
-- aConcat :: Type -> [Path] -> Action ()
-- aConcat cType cfg ref ids [oPath, fsPath] = do
--   fPaths <- readPaths fs'
--   let fPaths' = map (fromPath cfg) fPaths
--   need' "aConcat" fPaths'
--   let out'    = fromPath cfg oPath
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
--     fs' = fromPath cfg fsPath
-- aConcat _ _ _ _ = fail "bad argument to aConcat"

-- TODO WHY DID THIS BREAK CREATING THE CACHE/PSIBLAST DIR? FIX THAT TODAY, QUICK!
aConcat :: Type -> ([Path] -> Action ())
aConcat cType [outPath, inList] = do
  -- This is all so we can get an example <<emptywhatever>> to cat.py
  -- ... there's gotta be a simpler way right?
  cfg <- fmap fromJust getShakeExtra
  let tmpDir'   = cfgTmpDir cfg </> "cache" </> "concat"
      emptyPath = tmpDir' </> ("empty" ++ extOf cType) <.> "txt"
      emptyStr  = "<<empty" ++ extOf cType ++ ">>"
      inList'   = tmpDir' </> digest inList <.> "txt" -- TODO is that right?
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True $ takeDirectory $ fromPath cfg outPath
  writeCachedLines emptyPath [emptyStr]
  inPaths <- readPaths $ fromPath cfg inList
  let inPaths' = map (fromPath cfg) inPaths
  need' "ortholang.modules.seqio.aConcat" inPaths'
  writeCachedLines inList' inPaths'
  aSimpleScriptNoFix "cat.py" [ outPath
                              , toPath cfg inList'
                              , toPath cfg emptyPath]
aConcat _ _ = fail "bad argument to aConcat"

-- writeCachedLines outPath content = do

-- TODO would it work to just directly creat a string and tack onto paths here?
-- aSimpleScript' :: Bool -> String -> ([Path] -> Action ())
-- aSimpleScript' fixEmpties script cfg ref (out:ins) = aSimple' cfg ref ids out actFn Nothing ins

------------------------
-- split_fasta(_each) --
------------------------

splitFasta :: Type -> Function
splitFasta faType = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = defaultTypeCheck name [faType] (ListOf faType)
  , fTypeDesc  = mkTypeDesc name  [faType] (ListOf faType)
  , fNewRules = NewNotImplemented, fOldRules = rSimple $ aSplit name ext
  }
  where
    ext  = extOf faType
    name = "split_" ++ ext

splitFastaEach :: Type -> Function
splitFastaEach faType = Function
  { fOpChar = Nothing, fName = name
  ,fTags = []
  , fTypeCheck = defaultTypeCheck name [ListOf faType] (ListOf $ ListOf faType)
  , fTypeDesc  = mkTypeDesc name  [ListOf faType] (ListOf $ ListOf faType)
  , fNewRules = NewNotImplemented, fOldRules = rMap 1 $ aSplit name ext -- TODO is 1 wrong?
  }
  where
    ext  = extOf faType
    name = "split_" ++ ext ++ "_each"

aSplit :: String -> String -> ([Path] -> Action ())
aSplit name ext [outPath, faPath] = do
  cfg <- fmap fromJust getShakeExtra
  let faPath'   = fromPath cfg faPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = cfgTmpDir cfg </> "cache" </> name -- TODO is there a fn for this?
      prefix'   = tmpDir' </> digest faPath ++ "/"
      outDir'   = exprDir' </> "load_" ++ ext
      outPath'  = fromPath cfg outPath
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
  -- withWriteLock' tmpDir' $ do -- why is this required?
  runCmd $ CmdDesc
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
  -- loadPaths <- readPaths tmpList
  -- when (null loadPaths) $ error $ "no fasta file written: " ++ tmpList
  -- writePaths outPath'' loadPaths
  writeCachedVersion outPath'' tmpList
aSplit _ _ paths = error $ "bad argument to aSplit: " ++ show paths
