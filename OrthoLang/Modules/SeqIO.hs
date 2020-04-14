-- TODO rename something more general like SeqUtils?
-- TODO when running gbk_to_faa*, also load_faa the result to split out the IDs!
-- TODO gbk_to_fna (and probably others) need to substitute seqid_* hashes

module OrthoLang.Modules.SeqIO where

import Development.Shake

import OrthoLang.Core
-- import OrthoLang.Core (debug)

import System.FilePath             ((</>), (<.>), takeDirectory, takeFileName)
import System.Directory            (createDirectoryIfMissing)
import OrthoLang.Modules.Load       (mkLoadPath, mkLoad, mkLoadPathEach, mkLoadEach, mkLoadGlob, path)
import System.Exit                 (ExitCode(..))
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "SeqIO"
  , mDesc = "Sequence file manipulations using BioPython's SeqIO"
  , mTypes = [gbk, faa, fna, path]
  , mGroups = [fa]
  , mEncodings = []
  , mFunctions =
    [ gbkToFaaRawIDs, gbkToFaaRawIDsEach, gbkToFaa, gbkToFaaEach
    , gbkToFnaRawIDs, gbkToFnaRawIDsEach, gbkToFna, gbkToFnaEach
    , extractSeqs , extractSeqsEach
    , extractIds  , extractIdsEach
    , translate   , translateEach
    , mkConcat fna  , mkConcatEach fna
    , mkConcat faa  , mkConcatEach faa
    , splitFasta faa, splitFastaEach faa
    , splitFasta fna, splitFastaEach fna

    , loadFnaPath, loadFna, loadFnaPathEach, loadFnaEach, loadFnaGlob
    , loadFaaPath, loadFaa, loadFaaPathEach, loadFaaEach, loadFaaGlob
    , loadGbkPath, loadGbk, loadGbkPathEach, loadGbkEach, loadGbkGlob

    -- , mkLoad True fna, mkLoadEach True fna, mkLoadGlob True fna
    -- TODO combo that loads multiple fnas or faas and concats them?
    -- TODO combo that loads multiple gbks -> fna or faa?
    ]
    -- ++ mkLoaders True fna
    -- ++ mkLoaders True fna
    -- ++ mkLoaders True faa
    -- ++ mkLoaders False gbk -- TODO should seqids be hashed here too?
  }

loadFnaPath     = mkLoadPath     True "load_fna_path"      (Exactly fna)
loadFnaPathEach = mkLoadPathEach True "load_fna_path_each" (Exactly fna)
loadFna         = mkLoad              "load_fna"           loadFnaPath
loadFnaEach     = mkLoadEach          "load_fna_each"      loadFnaPathEach
loadFnaGlob     = mkLoadGlob          "load_fna_glob"      loadFnaPathEach

loadFaaPath     = mkLoadPath     True "load_faa_path"      (Exactly faa)
loadFaaPathEach = mkLoadPathEach True "load_faa_path_each" (Exactly faa)
loadFaa         = mkLoad              "load_faa"           loadFaaPath
loadFaaEach     = mkLoadEach          "load_faa_each"      loadFaaPathEach
loadFaaGlob     = mkLoadGlob          "load_faa_glob"      loadFaaPathEach

loadGbkPath     = mkLoadPath     False "load_gbk_path"      (Exactly gbk)
loadGbkPathEach = mkLoadPathEach False "load_gbk_path_each" (Exactly gbk)
loadGbk         = mkLoad               "load_gbk"           loadGbkPath
loadGbkEach     = mkLoadEach           "load_gbk_each"      loadGbkPathEach
loadGbkGlob     = mkLoadGlob           "load_gbk_glob"      loadGbkPathEach

gbk :: Type
gbk = Type
  { tExt  = "gbk"
  , tDesc = "genbank"
  , tShow = defaultShow
  }

fa :: TypeGroup
fa = TypeGroup
  { tgExt = "fa"
  , tgDesc  = "FASTA (nucleic OR amino acid)"
  , tgMembers = [Exactly fna, Exactly faa]
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
gbkToFaa = newMacro "gbk_to_faa" [Exactly str, Exactly gbk] (Exactly faa) mGbkToFaa [ReadsFile]

mGbkToFaa :: MacroExpansion
mGbkToFaa _ (Fun r ms ds n [s, g]) = Fun r ms ds "load_faa_path" [Fun r ms ds (n ++ "_rawids") [s, g]]
mGbkToFaa _ e = error "modules.seqio.mGbkToFaa" $ "bad argument: " ++ show e

gbkToFaaRawIDs :: Function
gbkToFaaRawIDs = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [str, gbk] faa
  -- , fTypeDesc  = mkTypeDesc name  [str, gbk] faa
  , fInputs = [Exactly str, Exactly gbk]
  , fOutput = Exactly faa
  , fTags = [Hidden]
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aGenbankToFasta faa "aa"
  }
  where
    name = "gbk_to_faa_rawids"

gbkToFna :: Function
-- gbkToFna = compose1 "gbk_to_fna" [ReadsFile] loadFna gbkToFnaRawIDs
gbkToFna = newMacro "gbk_to_fna" [Exactly str, Exactly gbk] (Exactly fna) mGbkToFna [ReadsFile]

mGbkToFna :: MacroExpansion
mGbkToFna _ (Fun r ms ds n [s, g]) = Fun r ms ds "load_fna_path" [Fun r ms ds (n ++ "_rawids") [s, g]]
mGbkToFna _ e = error "modules.seqio.mGbkToFna" $ "bad argument: " ++ show e

gbkToFnaRawIDs :: Function
gbkToFnaRawIDs = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [str, gbk] fna
  -- , fTypeDesc  = mkTypeDesc name  [str, gbk] fna
  , fInputs = [Exactly str, Exactly gbk]
  , fOutput = Exactly fna
  , fTags = [Hidden]
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aGenbankToFasta fna "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_fna_rawids"

-- TODO fix this!
gbkToFaaEach :: Function
-- gbkToFaaEach = compose1 "gbk_to_faa_each" [ReadsFile] gbkToFaaRawIDsEach $ mkLoadEach True "load_faa_each" (Exactly faa)
gbkToFaaEach = compose1 "gbk_to_faa_each" [ReadsFile] loadFaaEach gbkToFaaRawIDsEach
-- gbkToFaaEach = newMacro "gbk_to_faa_each" [Exactly str, Exactly $ ListOf gbk] (Exactly $ ListOF fna) mGbkToFaaEach [ReadsFile]

gbkToFaaRawIDsEach :: Function
gbkToFaaRawIDsEach = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [str, ListOf gbk] (ListOf faa)
  -- , fTypeDesc  = mkTypeDesc name  [str, ListOf gbk] (ListOf faa)
  , fInputs = [Exactly str, Exactly (ListOf gbk)]
  , fOutput = Exactly (ListOf faa)
  , fTags = [Hidden]
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 2 $ aGenbankToFasta faa "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_faa_rawids_each"

gbkToFnaEach :: Function
-- gbkToFnaEach = compose1 "gbk_to_fna_each" [ReadsFile] gbkToFnaRawIDsEach $ mkLoadEach True "load_fna_each" (Exactly fna)
gbkToFnaEach = compose1 "gbk_to_fna_each" [ReadsFile] loadFnaEach gbkToFnaRawIDsEach

gbkToFnaRawIDsEach :: Function
gbkToFnaRawIDsEach = Function
  { fOpChar = Nothing, fName = name
  -- , fTypeCheck = defaultTypeCheck name [str, ListOf gbk] (ListOf fna)
  -- , fTypeDesc  = mkTypeDesc name  [str, ListOf gbk] (ListOf fna)
  , fInputs = [Exactly str, Exactly (ListOf gbk)]
  , fOutput = Exactly (ListOf fna)
  , fTags = [Hidden]
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 2 $ aGenbankToFasta fna "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_fna_rawids_each"

-- TODO error if no features extracted since it probably means a wrong ft string
-- TODO silence the output? or is it helpful?
aGenbankToFasta :: Type -> String -> ([Path] -> Action ())
aGenbankToFasta rtn st [outPath, ftPath, faPath] = do
  cfg <- fmap fromJust getShakeExtra
  let faPath'   = fromPath loc cfg faPath
      ftPath'   = fromPath loc cfg ftPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = fromPath loc cfg $ cacheDir cfg "seqio"
      outDir'   = exprDir' </> "load_" ++ tExtOf rtn
      outPath'  = fromPath loc cfg outPath
      loc = "modules.seqio.aGenbankToFasta"
      outPath'' = traceA loc outPath' [outPath', faPath']
  -- liftIO $ putStrLn $ "ftPath': " ++ show ftPath'
  ft <- readLit loc ftPath'
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
  , fTags = []
  -- , fTypeCheck = tExtractIds
  -- , fTypeDesc  = name ++ " : fa -> str.list"
  , fInputs = [Some fa "any fasta file"]
  , fOutput = Exactly (ListOf str)
  , fNewRules = NewNotImplemented
  , fOldRules = rSimpleScript "extract_ids.py"
  }
  where
    name = "extract_ids"

-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractIdsEach :: Function
extractIdsEach = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  -- , fTypeCheck = tExtractIdsEach
  -- , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fInputs = [ListSigs (Some fa "any fasta file")]
  , fOutput = Exactly (ListOf (ListOf str))
  , fNewRules = NewNotImplemented
  , fOldRules = rMapSimpleScript 1 "extract_ids.py"
  }
  where
    name = "extract_ids_each"

-- Some fa "any fasta file" (ListOf str)
-- shown as "fa -> str.list, where fa is any fasta file"
-- tExtractIds :: [Type] -> Either String Type
-- tExtractIds [x] | elem x [faa, fna] = Right (ListOf str)
-- tExtractIds _ = Left "expected a fasta file"

-- (ListOf (Some fa "any fasta file")) (ListOf (ListOf str))
-- shown as "fa.list -> str.list.list, where fa is any fasta file"
-- tExtractIdsEach :: [Type] -> Either String Type
-- tExtractIdsEach [ListOf x] | elem x [faa, fna] = Right (ListOf $ ListOf str)
-- tExtractIdsEach _ = Left "expected a fasta file"

-------------------------
-- extract_seqs(_each) --
-------------------------

-- TODO also extract them from genbank files

extractSeqs :: Function
extractSeqs = newFnA2
  "extract_seqs"
  (Some fa "any fasta file", Exactly $ ListOf str)
  (Some fa "any fasta file")
  aExtractSeqs
  [] -- TODO tag for "re-load output"?

{-|
This is a little more complicated than it would seem because users will
provide a list of actual seqids, and we need to look up their hashes to extract
the hash-named ones from the previously-sanitized fasta file.
-}
aExtractSeqs :: NewAction2
aExtractSeqs out inFa inList = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.seqio.aExtractSeqs"
      tmp  = fromPath loc cfg $ cacheDir cfg "seqio"
      ids  = tmp </> digest (toPath loc cfg inList) <.> "txt"
      ids' = toPath loc cfg ids
  lookupIDsFile (toPath loc cfg inList) ids' -- TODO implement as a macro
  aNewRulesS2 "extract_seqs.py" id out inFa ids

-- TODO remove by rewriting map functions to work on the new one above
aExtractSeqsOld :: [Path] -> Action ()
aExtractSeqsOld [outPath, inFa, inList] = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.seqio.aExtractSeqsOld"
      cDir     = fromPath loc cfg $ cacheDir cfg "seqio"
      tmpList' = cDir </> digest inList <.> "txt"
      tmpList  = toPath loc cfg tmpList'
  liftIO $ createDirectoryIfMissing True cDir
  lookupIDsFile inList tmpList
  aSimpleScriptNoFix "extract_seqs.py" [outPath, inFa, tmpList]
aExtractSeqsOld ps = error $ "bad argument to aExtractSeqs: " ++ show ps

-- TODO does this one even make sense? maybe only as an _all version for mixed id lists?
--      or maybe for singletons or something?
-- TODO needs to go through (reverse?) lookup in the hashedids dict somehow!
extractSeqsEach :: Function
extractSeqsEach = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  -- , fTypeCheck = tExtractSeqsEach
  -- , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fInputs = [Some fa "any fasta file", Exactly (ListOf (ListOf str))]
  , fOutput = ListSigs (Some fa "any fasta file")
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 1 aExtractSeqsOld
  }
  where
    name = "extract_seqs_each"

-- (Some fa "any fasta file", ListOf str) (Some fa "any fasta file")
-- shown as "fa str.list -> fa, where fa is any fasta file"
-- tExtractSeqs  :: [Type] -> Either String Type
-- tExtractSeqs [x, ListOf s] | s == str && elem x [faa, fna] = Right x
-- tExtractSeqs _ = Left "expected a fasta file and a list of strings"

-- (Some fa "any fasta file", (ListOf (ListOf str))) (ListOf (Some fa "any fasta file"))
-- shown as "fa str.list -> fa.list, where fa is any fasta file"
-- tExtractSeqsEach  :: [Type] -> Either String Type
-- tExtractSeqsEach [x, ListOf (ListOf s)] | s == str && elem x [faa, fna] = Right $ ListOf x
-- tExtractSeqsEach _ = Left "expected a fasta file and a list of strings"

----------------------
-- translate(_each) --
----------------------

-- translate = Function
--   { fOpChar = Nothing, fName = name
--   ,fTags = []
--   , fTypeCheck = defaultTypeCheck name [fna] faa
--   , fTypeDesc  = mkTypeDesc name  [fna] faa
--   , fNewRules = NewNotImplemented, fOldRules = rSimpleScript "translate.py"
--   }
--   where
--     name = "translate"

-- TODO fix unable to decode the fna error
--      must be that load_fna* aren't adding their digests?
translate :: Function
translate = newFnS1 "translate" (Exactly fna) (Exactly faa) "translate.py" [ReadsFile] id

-- TODO how to reuse loadFaa from main modules list?
-- translate :: Function
-- translate = compose1 "translate" [ReadsFile] translateRawIDs $ mkLoad True "load_faa" (Exactly faa)

translateEach :: Function
translateEach = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  , fInputs = [Exactly (ListOf fna)]
  , fOutput =  Exactly (ListOf faa)
  , fNewRules = NewNotImplemented
  , fOldRules = rMapSimpleScript 1 "translate.py"
  }
  where
    name = "translate_each"

-- TODO how to reuse loadFaa from main modules list?
-- translateEach :: Function
-- translateEach = compose1 "translate_each" [ReadsFile] translateRawIDsEach $ mkLoadEach True "load_faa_each" (Exactly faa)

--------------
-- concat_* --
--------------

-- TODO separate concat module?

mkConcat :: Type -> Function
mkConcat cType = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  -- , fTypeCheck = defaultTypeCheck name [ListOf cType] cType
  -- , fTypeDesc  = mkTypeDesc name  [ListOf cType] cType
  , fInputs = [Exactly (ListOf cType)]
  , fOutput =  Exactly cType
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aConcat cType
  }
  where
    ext  = tExtOf cType
    name = "concat_" ++ ext

mkConcatEach :: Type -> Function
mkConcatEach cType = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  -- , fTypeCheck = defaultTypeCheck name [ListOf $ ListOf cType] (ListOf cType)
  -- , fTypeDesc  = mkTypeDesc name  [ListOf $ ListOf cType] (ListOf cType)
  , fInputs = [Exactly (ListOf (ListOf cType))]
  , fOutput =  Exactly (ListOf cType)
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 1 $ aConcat cType
  }
  where
    ext  = tExtOf cType
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
--   let fPaths' = map (fromPath loc cfg) fPaths
--   need' "aConcat" fPaths'
--   let out'    = fromPath loc cfg oPath
--       out''   = traceA "aConcat" out' [out', fs']
--       outTmp  = out'' <.> "tmp"
--       emptyStr = "<<empty" ++ tExtOf cType ++ ">>"
--       grepCmd = "egrep -v '^" ++ emptyStr ++ "$'"
--       catArgs = fPaths' ++ ["|", grepCmd, ">", outTmp]
--   wrappedCmdWrite cfg ref outTmp fPaths' [] [Shell] "cat"
--     (debug cfg ("catArgs: " ++ show catArgs) catArgs)
--   needsFix <- isReallyEmpty outTmp
--   if needsFix
--     then liftIO $ writeFile out'' emptyStr
--     else copyFile' outTmp out''
--   where
--     fs' = fromPath loc cfg fsPath
-- aConcat _ _ _ _ = fail "bad argument to aConcat"

-- TODO WHY DID THIS BREAK CREATING THE CACHE/PSIBLAST DIR? FIX THAT TODAY, QUICK!
aConcat :: Type -> ([Path] -> Action ())
aConcat cType [outPath, inList] = do
  -- This is all so we can get an example <<emptywhatever>> to cat.py
  -- ... there's gotta be a simpler way right?
  cfg <- fmap fromJust getShakeExtra
  let tmpDir'   = cfgTmpDir cfg </> "cache" </> "concat"
      emptyPath = tmpDir' </> ("empty" ++ tExtOf cType) <.> "txt"
      emptyStr  = "<<empty" ++ tExtOf cType ++ ">>"
      inList'   = tmpDir' </> digest inList <.> "txt" -- TODO is that right?
      loc = "ortholang.modules.seqio.aConcat"
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True $ takeDirectory $ fromPath loc cfg outPath
  writeCachedLines loc emptyPath [emptyStr]
  inPaths <- readPaths loc $ fromPath loc cfg inList
  let inPaths' = map (fromPath loc cfg) inPaths
  need' loc inPaths'
  writeCachedLines loc inList' inPaths'
  aSimpleScriptNoFix "cat.py" [ outPath
                              , toPath loc cfg inList'
                              , toPath loc cfg emptyPath]
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
  , fTags = []
  -- , fTypeCheck = defaultTypeCheck name [faType] (ListOf faType)
  -- , fTypeDesc  = mkTypeDesc name  [faType] (ListOf faType)
  , fInputs = [Exactly faType]
  , fOutput =  Exactly (ListOf faType)
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aSplit name ext
  }
  where
    ext  = tExtOf faType
    name = "split_" ++ ext

splitFastaEach :: Type -> Function
splitFastaEach faType = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  -- , fTypeCheck = defaultTypeCheck name [ListOf faType] (ListOf $ ListOf faType)
  -- , fTypeDesc  = mkTypeDesc name  [ListOf faType] (ListOf $ ListOf faType)
  , fInputs = [Exactly (ListOf faType)]
  , fOutput =  Exactly (ListOf (ListOf faType))
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 1 $ aSplit name ext -- TODO is 1 wrong?
  }
  where
    ext  = tExtOf faType
    name = "split_" ++ ext ++ "_each"

aSplit :: String -> String -> ([Path] -> Action ())
aSplit name ext [outPath, faPath] = do
  cfg <- fmap fromJust getShakeExtra
  let faPath'   = fromPath loc cfg faPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = cfgTmpDir cfg </> "cache" </> name -- TODO is there a fn for this?
      prefix'   = tmpDir' </> digest faPath ++ "/"
      outDir'   = exprDir' </> "load_" ++ ext
      outPath'  = fromPath loc cfg outPath
      loc = "modules.seqio.aSplit"
      outPath'' = traceA loc outPath' [outPath', faPath']
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
  writeCachedVersion loc outPath'' tmpList
aSplit _ _ paths = error $ "bad argument to aSplit: " ++ show paths
