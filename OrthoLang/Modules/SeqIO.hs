-- TODO rename something more general like SeqUtils?

module OrthoLang.Modules.SeqIO where

import Development.Shake

import OrthoLang.Types
import OrthoLang.Interpreter

import System.FilePath             ((</>), (<.>), takeDirectory, takeFileName)
import System.Directory            (createDirectoryIfMissing)
import OrthoLang.Modules.Load       (mkLoad, mkLoadPath, mkLoadEach, mkLoadPathEach, mkLoadGlob)
import System.Exit                 (ExitCode(..))
import Data.Maybe (fromJust)
import Data.List.Utils (replace)

olModule :: Module
olModule = Module
  { mName = "SeqIO"
  , mDesc = "Sequence file manipulations using BioPython's SeqIO"
  , mTypes = [gbk, faa, fna]
  , mGroups = [fa]
  , mEncodings = []
  , mFunctions =
    [ gbkToFaaRawIDs, gbkToFaaRawIDsEach, gbkToFaa, gbkToFaaEach
    , gbkToFnaRawIDs, gbkToFnaRawIDsEach, gbkToFna, gbkToFnaEach
    , extractSeqs   , extractSeqsEach
    , extractIds    , extractIdsEach
    , translate     , translateEach
    , mkConcat fna  , mkConcatEach fna -- TODO pull these apart too
    , mkConcat faa  , mkConcatEach faa -- TODO pull these apart too
    , splitFasta faa, splitFastaEach faa
    , splitFasta fna, splitFastaEach fna
    , loadFna, loadFnaPath, loadFnaEach, loadFnaPathEach, loadFnaGlob
    , loadFaa, loadFaaPath, loadFaaEach, loadFaaPathEach, loadFaaGlob
    , loadGbk, loadGbkPath, loadGbkEach, loadGbkPathEach, loadGbkGlob
    -- TODO combo that loads multiple fnas or faas and concats them?
    -- TODO combo that loads multiple gbks -> fna or faa?
    ]
  }

loadFna         = mkLoad         True "load_fna"           (Exactly fna)
loadFnaPath     = mkLoadPath     True "load_fna_path"      (Exactly fna)
loadFnaEach     = mkLoadEach     True "load_fna_each"      (Exactly fna)
loadFnaPathEach = mkLoadPathEach True "load_fna_path_each" (Exactly fna)
loadFnaGlob     = mkLoadGlob          "load_fna_glob"       loadFnaEach

loadFaa         = mkLoad         True "load_faa"           (Exactly faa)
loadFaaPath     = mkLoadPath     True "load_faa_path"      (Exactly faa)
loadFaaEach     = mkLoadEach     True "load_faa_each"      (Exactly faa)
loadFaaPathEach = mkLoadPathEach True "load_faa_path_each" (Exactly faa)
loadFaaGlob     = mkLoadGlob          "load_faa_glob"       loadFaaEach

loadGbk         = mkLoad         False "load_gbk"           (Exactly gbk)
loadGbkPath     = mkLoad         False "load_gbk_path"      (Exactly gbk)
loadGbkEach     = mkLoadEach     False "load_gbk_each"      (Exactly gbk)
loadGbkPathEach = mkLoadPathEach False "load_gbk_path_each" (Exactly gbk)
loadGbkGlob     = mkLoadGlob           "load_gbk_glob"       loadGbkEach

gbk :: Type
gbk = Type
  { tExt  = "gbk"
  , tDesc = "Genbank files"
  , tShow = defaultShow
  }

fa :: TypeGroup
fa = TypeGroup
  { tgExt = "fa"
  , tgDesc  = "FASTA nucleic OR amino acid"
  , tgMembers = [Exactly fna, Exactly faa]
  }

faa :: Type
faa = Type
  { tExt  = "faa"
  , tDesc = "FASTA amino acid"
  , tShow = defaultShow
  }

fna :: Type
fna = Type
  { tExt  = "fna"
  , tDesc = "FASTA nucleic acid"
  , tShow = defaultShow
  }

--------------
-- gbk_to_* --
--------------

-- TODO should these automatically fill in the "CDS" string?

gbkToFaa :: Function
gbkToFaa = newExprExpansion "gbk_to_faa" [Exactly str, Exactly gbk] (Exactly faa) mGbkToFaa [ReadsFile]

mGbkToFaa :: ExprExpansion
mGbkToFaa _ (Fun r ms ds n [s, g]) = Fun r ms ds "load_faa_path" [Fun r ms ds (n ++ "_rawids") [s, g]]
mGbkToFaa _ e = error "modules.seqio.mGbkToFaa" $ "bad argument: " ++ show e

gbkToFaaRawIDs :: Function
gbkToFaaRawIDs = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly str, Exactly gbk]
  , fOutput = Exactly faa
  , fTags = [Hidden]
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aGenbankToFasta faa "aa"
  }
  where
    name = "gbk_to_faa_rawids"

gbkToFna :: Function
gbkToFna = newExprExpansion "gbk_to_fna" [Exactly str, Exactly gbk] (Exactly fna) mGbkToFna [ReadsFile]

mGbkToFna :: ExprExpansion
mGbkToFna _ (Fun r ms ds n [s, g]) = Fun r ms ds "load_fna_path" [Fun r ms ds (n ++ "_rawids") [s, g]]
mGbkToFna _ e = error "modules.seqio.mGbkToFna" $ "bad argument: " ++ show e

gbkToFnaRawIDs :: Function
gbkToFnaRawIDs = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly str, Exactly gbk]
  , fOutput = Exactly fna
  , fTags = [Hidden]
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aGenbankToFasta fna "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_fna_rawids"

gbkToFaaEach :: Function
gbkToFaaEach = newExprExpansion "gbk_to_faa_each" [Exactly str, Exactly $ ListOf gbk] (Exactly $ ListOf faa) mGbkToFaaEach [ReadsFile]

mGbkToFaaEach :: ExprExpansion
mGbkToFaaEach _ (Fun r ms ds n [s, g]) = Fun r ms ds "load_faa_path_each" [Fun r ms ds (replace "_each" "_rawids_each" n) [s, g]]
mGbkToFaaEach _ e = error "modules.seqio.mGbkToFaaEach" $ "bad argument: " ++ show e

gbkToFaaRawIDsEach :: Function
gbkToFaaRawIDsEach = Function
  { fOpChar = Nothing, fName = name
  , fInputs = [Exactly str, Exactly (ListOf gbk)]
  , fOutput = Exactly (ListOf faa)
  , fTags = [Hidden]
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 2 $ aGenbankToFasta faa "nt" -- TODO add --qualifiers all?
  }
  where
    name = "gbk_to_faa_rawids_each"

gbkToFnaEach :: Function
gbkToFnaEach = newExprExpansion "gbk_to_fna_each" [Exactly str, Exactly $ ListOf gbk] (Exactly $ ListOf fna) mGbkToFnaEach [ReadsFile]

mGbkToFnaEach :: ExprExpansion
mGbkToFnaEach _ (Fun r ms ds n [s, g]) = Fun r ms ds "load_fna_path_each" [Fun r ms ds (replace "_each" "_rawids_each" n) [s, g]]
mGbkToFnaEach _ e = error "modules.seqio.mGbkToFnaEach" $ "bad argument: " ++ show e


gbkToFnaRawIDsEach :: Function
gbkToFnaRawIDsEach = Function
  { fOpChar = Nothing, fName = name
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
      exprDir'  = tmpdir cfg </> "exprs"
      tmpDir'   = fromPath loc cfg $ cacheDir cfg "seqio"
      outDir'   = exprDir' </> "load_" ++ ext rtn
      outPath'  = fromPath loc cfg outPath
      loc = "modules.seqio.aGenbankToFasta"
      outPath'' = traceA loc outPath' [outPath', faPath']
  ft <- readLit loc ftPath'
  let ft' = if ft  == "cds" then "CDS" else ft
      (st', extraArgs) = if ft' == "whole" then ("whole", ["--annotations", "all"]) else (st, [])
      args = [ "--in_file", faPath'
             , "--out_file", outPath'
             , "--sequence_type", st'
             , "--feature_type", ft'] ++ extraArgs
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

-- TODO also extract them from genbank files

extractIds :: Function
extractIds = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  , fInputs = [Some fa "any fasta file"]
  , fOutput = Exactly (ListOf str)
  , fNewRules = NewNotImplemented
  , fOldRules = rSimpleScript "extract_ids.py"
  }
  where
    name = "extract_ids"

extractIdsEach :: Function
extractIdsEach = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  , fInputs = [ListSigs (Some fa "any fasta file")]
  , fOutput = Exactly (ListOf (ListOf str))
  , fNewRules = NewNotImplemented
  , fOldRules = rMapSimpleScript 1 "extract_ids.py"
  }
  where
    name = "extract_ids_each"

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
      ids  = tmp </> digest loc (toPath loc cfg inList) <.> "txt"
      ids' = toPath loc cfg ids
  -- TODO these should be the seqid_... ids themselves, not unhashed?
  -- unhashIDsFile (toPath loc cfg inList) ids -- TODO implement as a macro?
  aNewRulesS2 "extract_seqs.py" id out inFa inList

-- TODO remove by rewriting map functions to work on the new one above
aExtractSeqsOld :: [Path] -> Action ()
aExtractSeqsOld [outPath, inFa, inList] = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.seqio.aExtractSeqsOld"
      cDir     = fromPath loc cfg $ cacheDir cfg "seqio"
      tmpList' = cDir </> digest loc inList <.> "txt"
      tmpList  = toPath loc cfg tmpList'
  liftIO $ createDirectoryIfMissing True cDir
  -- lookupIDsFile inList tmpList
  aSimpleScriptNoFix "extract_seqs.py" [outPath, inFa, inList]
aExtractSeqsOld ps = error $ "bad argument to aExtractSeqs: " ++ show ps

-- TODO does this one even make sense? maybe only as an _all version for mixed id lists?
--      or maybe for singletons or something?
extractSeqsEach :: Function
extractSeqsEach = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  , fInputs = [Some fa "any fasta file", Exactly (ListOf (ListOf str))]
  , fOutput = ListSigs (Some fa "any fasta file")
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 1 aExtractSeqsOld
  }
  where
    name = "extract_seqs_each"

----------------------
-- translate(_each) --
----------------------

translate :: Function
translate = newFnS1 "translate" (Exactly fna) (Exactly faa) "translate.py" [ReadsFile] id

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

--------------
-- concat_* --
--------------

-- TODO separate concat module? or maybe this goes in ListLike?

mkConcat :: Type -> Function
mkConcat cType = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  , fInputs = [Exactly (ListOf cType)]
  , fOutput =  Exactly cType
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aConcat cType
  }
  where
    name = "concat_" ++ ext cType

mkConcatEach :: Type -> Function
mkConcatEach cType = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  , fInputs = [Exactly (ListOf (ListOf cType))]
  , fOutput =  Exactly (ListOf cType)
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 1 $ aConcat cType
  }
  where
    name = "concat_" ++ ext cType ++ "_each"

{- This is just a fancy `cat`, with handling for a couple cases:
 - * some args are empty and their <<emptywhatever>> should be removed
 - * all args are empty and they should be collapsed to one <<emptywhatever>>
 -
 - TODO special case of error handling here, since cat errors are usually temporary?
 -}
aConcat :: Type -> ([Path] -> Action ())
aConcat cType [outPath, inList] = do
  -- This is all so we can get an example <<emptywhatever>> to cat.py
  -- ... there's gotta be a simpler way right?
  cfg <- fmap fromJust getShakeExtra
  let tmpDir'   = tmpdir cfg </> "cache" </> "concat"
      emptyPath = tmpDir' </> ("empty" ++ ext cType) <.> "txt"
      emptyStr  = "<<empty" ++ ext cType ++ ">>"
      loc = "ortholang.modules.seqio.aConcat"
      inList'   = tmpDir' </> digest loc inList <.> "txt" -- TODO is that right?
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

------------------------
-- split_fasta(_each) --
------------------------

splitFasta :: Type -> Function
splitFasta faType = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  , fInputs = [Exactly faType]
  , fOutput =  Exactly (ListOf faType)
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple $ aSplit name $ ext faType
  }
  where
    name = "split_" ++ ext faType

splitFastaEach :: Type -> Function
splitFastaEach faType = Function
  { fOpChar = Nothing, fName = name
  , fTags = []
  , fInputs = [Exactly (ListOf faType)]
  , fOutput =  Exactly (ListOf (ListOf faType))
  , fNewRules = NewNotImplemented
  , fOldRules = rMap 1 $ aSplit name $ ext faType -- TODO is 1 wrong?
  }
  where
    name = "split_" ++ ext faType ++ "_each"

aSplit :: String -> String -> ([Path] -> Action ())
aSplit name e [outPath, faPath] = do
  cfg <- fmap fromJust getShakeExtra
  let faPath'   = fromPath loc cfg faPath
      exprDir'  = tmpdir cfg </> "exprs"
      tmpDir'   = tmpdir cfg </> "cache" </> name -- TODO is there a fn for this?
      loc = "ortholang.modules.seqio.aSplit"
      prefix'   = tmpDir' </> digest loc faPath ++ "/"
      outDir'   = exprDir' </> "load_" ++ e
      outPath'  = fromPath loc cfg outPath
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
