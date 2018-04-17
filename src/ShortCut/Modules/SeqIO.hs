-- TODO rename something more general like SeqUtils?

module ShortCut.Modules.SeqIO where

import Prelude hiding (concat)

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Config (debug)

import ShortCut.Core.Util          (digest)
import ShortCut.Core.Actions       (readPaths, writePaths, debugA, debugNeed,
                                    wrappedCmdOut, wrappedCmdWrite)
import ShortCut.Core.Paths         (toCutPath, fromCutPath, CutPath)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, mkLoad,
                                    mkLoadList, rSimple, rSimpleScript)
import ShortCut.Core.Compile.Vectorize  (rVectorize, rVectorizeSimpleScript)
import System.FilePath             ((</>))
import System.Directory            (createDirectoryIfMissing)
import ShortCut.Core.Compile.Compose (compose1)
import ShortCut.Modules.Glob (globFiles)

cutModule :: CutModule
cutModule = CutModule
  { mName = "seqio"
  , mFunctions =
    -- [ loadGbk     , loadGbkEach, loadGbkGlob
    -- , loadFaa     , loadFaaEach, loadFaaGlob
    -- , loadFna     , loadFnaEach, loadFnaGlob
    mkLoaders fna ++ mkLoaders faa ++ mkLoaders gbk ++
    [ gbkToFaa    , gbkToFaaEach
    , gbkToFna    , gbkToFnaEach
    , extractSeqs , extractSeqsEach
    , extractIds  , extractIdsEach
    , translate   , translateEach
    , concat fna  , concatEach fna
    , concat faa  , concatEach faa
    , splitFasta faa, splitFastaEach faa
    , splitFasta fna, splitFastaEach fna
    -- TODO combo that loads multiple fnas or faas and concats them?
    -- TODO combo that loads multiple gbks -> fna or faa?
    ]
  }

gbk :: CutType
gbk = CutType
  { tExt  = "gbk"
  , tDesc = "genbank file"
  , tShow = defaultShow
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

-------------------
-- load_*(_each) --
-------------------

-- loadFaa :: CutFunction
-- loadFaa = mkLoad "load_faa" faa
-- 
-- loadFaaEach :: CutFunction
-- loadFaaEach = mkLoadList "load_faa_each" faa
-- 
-- loadFna :: CutFunction
-- loadFna = mkLoad "load_fna" fna
-- 
-- loadFnaEach :: CutFunction
-- loadFnaEach = mkLoadList "load_fna_each" fna
-- 
-- loadGbk :: CutFunction
-- loadGbk = mkLoad "load_gbk" gbk
-- 
-- loadGbkEach :: CutFunction
-- loadGbkEach = mkLoadList "load_gbk_each" gbk

------------
-- glob_* --
------------

mkLoadGlob :: String -> CutType -> CutFunction -> CutFunction
mkLoadGlob name loadType eachFn = compose1 globFiles eachFn name (ListOf str) desc
  where
    -- loadList = mkLoadList ("load_" ++ extOf loadType ++ "_each") loadType
    desc     = mkTypeDesc name [str] (ListOf loadType)

-- loadFaaGlob :: CutFunction
-- loadFaaGlob = mkLoadGlob "load_faa_glob" faa
-- 
-- loadFnaGlob :: CutFunction
-- loadFnaGlob = mkLoadGlob "load_fna_glob" fna
-- 
-- loadGbkGlob :: CutFunction
-- loadGbkGlob = mkLoadGlob "load_gbk_glob" gbk

mkLoaders :: CutType -> [CutFunction]
-- mkLoaders loadType = [single, each, glob]
mkLoaders loadType = [single, each]
  where
    ext    = extOf loadType
    single = mkLoad     ("load_" ++ ext           ) loadType
    each   = mkLoadList ("load_" ++ ext ++ "_each") loadType
    globFn = mkLoadGlob ("load_" ++ ext ++ "_glob") loadType each

-----------------------
-- gbk_to_f*a(_each) --
-----------------------

gbkToFaa :: CutFunction
gbkToFaa = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [gbk] faa
  , fTypeDesc  = mkTypeDesc name  [gbk] faa
  , fFixity    = Prefix
  , fRules     = rSimpleScript "gbk_to_faa.py"
  }
  where
    name = "gbk_to_faa"

gbkToFaaEach :: CutFunction
gbkToFaaEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf gbk] (ListOf faa)
  , fTypeDesc  = mkTypeDesc name  [ListOf gbk] (ListOf faa)
  , fFixity    = Prefix
  , fRules     = rVectorizeSimpleScript 1 "gbk_to_faa.py"
  }
  where
    name = "gbk_to_faa_each"

gbkToFna :: CutFunction
gbkToFna = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [gbk] fna
  , fTypeDesc  = mkTypeDesc name  [gbk] fna
  , fFixity    = Prefix
  , fRules     = rSimpleScript "gbk_to_fna.py"
  }
  where
    name = "gbk_to_fna"

gbkToFnaEach :: CutFunction
gbkToFnaEach = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [ListOf gbk] (ListOf fna)
  , fTypeDesc  = mkTypeDesc name  [ListOf gbk] (ListOf fna)
  , fFixity    = Prefix
  , fRules     = rVectorizeSimpleScript 1 "gbk_to_fna.py"
  }
  where
    name = "gbk_to_fna_each"

------------------------
-- extract_ids(_each) --
------------------------

-- TODO this needs to do relative paths again, not absolute!
-- TODO also extract them from genbank files

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

extractIdsEach :: CutFunction
extractIdsEach = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractIdsEach
  , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fRules     = rVectorizeSimpleScript 1 "extract_ids.py"
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

extractSeqs :: CutFunction
extractSeqs = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqs
  , fTypeDesc  = name ++ " : fa -> str.list"
  , fRules     = rSimpleScript "extract_seqs.py"
  }
  where
    name = "extract_seqs"

extractSeqsEach :: CutFunction
extractSeqsEach = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqsEach
  , fTypeDesc  = name ++ " : fa.list -> str.list.list"
  , fRules     = rVectorizeSimpleScript 1 "extract_seqs.py"
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
  , fRules     = rVectorizeSimpleScript 1 "translate.py"
  }
  where
    name = "translate_each"

--------------
-- concat_* --
--------------

concat :: CutType -> CutFunction
concat cType = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf cType] cType
  , fTypeDesc  = mkTypeDesc name  [ListOf cType] cType
  , fRules     = rSimple aConcat
  }
  where
    ext  = extOf cType
    name = "concat_" ++ ext

concatEach :: CutType -> CutFunction
concatEach cType = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf $ ListOf cType] (ListOf cType)
  , fTypeDesc  = mkTypeDesc name  [ListOf $ ListOf cType] (ListOf cType)
  , fRules     = rVectorize 1 aConcat
  }
  where
    ext  = extOf cType
    name = "concat_" ++ ext ++ "_each"

-- TODO specific error handling here, since cat errors are usually transitory?
aConcat :: CutConfig -> Locks -> [CutPath] -> Action ()
aConcat cfg ref [oPath, fsPath] = do
  fPaths <- readPaths cfg ref fs'
  let fPaths' = map (fromCutPath cfg) fPaths
  debugNeed cfg "aConcat" fPaths'
  let out'    = fromCutPath cfg oPath
      out''   = debugA cfg "aConcat" out' [out', fs']
      catArgs = fPaths' ++ [">", out']
  wrappedCmdWrite cfg ref out'' fPaths' [] [Shell] "cat"
    (debug cfg ("catArgs: " ++ show catArgs) catArgs)
  where
    fs' = fromCutPath cfg fsPath
aConcat _ _ _ = error "bad argument to aConcat"

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
  , fRules     = rVectorize 1 $ aSplit name ext -- TODO is 1 wrong?
  }
  where
    ext  = extOf faType
    name = "split_" ++ ext ++ "_each"

aSplit :: String -> String -> (CutConfig -> Locks -> [CutPath] -> Action ())
aSplit name ext cfg ref [outPath, faPath] = do
  let faPath'   = fromCutPath cfg faPath
      exprDir'  = cfgTmpDir cfg </> "exprs"
      tmpDir'   = cfgTmpDir cfg </> "cache" </> name -- TODO is there a fn for this?
      prefix'   = tmpDir' </> digest faPath ++ "_"
      outDir'   = exprDir' </> "load_" ++ ext
      outPath'  = fromCutPath cfg outPath
      outPath'' = debugA cfg "aSplit" outPath' [outPath', faPath']
      args      = [outDir', prefix', faPath']
  -- TODO make sure stderr doesn't come through?
  -- TODO any locking needed here?
  liftIO $ createDirectoryIfMissing True tmpDir'
  liftIO $ createDirectoryIfMissing True outDir'
  out <- wrappedCmdOut cfg ref [faPath'] [] [] "split_fasta.py" args
  let loadPaths = map (toCutPath cfg) $ lines out
  writePaths cfg ref outPath'' loadPaths
aSplit _ _ _ _ paths = error $ "bad argument to aSplit: " ++ show paths
