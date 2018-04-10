-- TODO rename something more general like SeqUtils?

module ShortCut.Modules.SeqIO where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Config (debug)

import ShortCut.Core.Util          (digest)
import ShortCut.Core.Actions       (readPaths, writePaths, debugA, debugNeed,
                                    wrappedCmdOut, wrappedCmdWrite)
import ShortCut.Core.Paths         (toCutPath, fromCutPath, CutPath)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, mkLoad,
                                    mkLoadList, rSimple, rSimpleScript)
import ShortCut.Core.Compile.Each  (rEach, rSimpleScriptEach, rEach)
import System.FilePath             ((</>))
import System.Directory            (createDirectoryIfMissing)

cutModule :: CutModule
cutModule = CutModule
  { mName = "seqio"
  , mFunctions =
    [ loadGbk     , loadGbkEach
    , loadFaa     , loadFaaEach
    , loadFna     , loadFnaEach
    , gbkToFaa    , gbkToFaaEach
    , gbkToFna    , gbkToFnaEach
    , extractSeqs , extractSeqsEach
    , extractIds  , extractIdsEach
    , translate   , translateEach
    , concatFastas, concatFastasEach
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

loadFaa :: CutFunction
loadFaa = mkLoad "load_faa" faa

loadFaaEach :: CutFunction
loadFaaEach = mkLoadList "load_faa_each" faa

loadFna :: CutFunction
loadFna = mkLoad "load_fna" fna

loadFnaEach :: CutFunction
loadFnaEach = mkLoadList "load_fna_each" fna

loadGbk :: CutFunction
loadGbk = mkLoad "load_gbk" gbk

loadGbkEach :: CutFunction
loadGbkEach = mkLoadList "load_gbk_each" gbk

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
  , fRules     = rSimpleScriptEach "gbk_to_faa.py"
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
  , fRules     = rSimpleScriptEach "gbk_to_fna.py"
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
  , fRules     = rSimpleScriptEach "extract_ids.py"
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
  , fRules     = rSimpleScriptEach "extract_seqs.py"
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
  , fRules     = rSimpleScriptEach "translate.py"
  }
  where
    name = "translate_each"

--------------------------
-- concat_fastas(_each) --
--------------------------

concatFastas :: CutFunction
concatFastas = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tConcatFastas
  , fTypeDesc  = name ++ " : fa.list -> fa"
  , fRules     = rSimple aConcat
  }
  where
    name = "concat_fastas"

concatFastasEach :: CutFunction
concatFastasEach = CutFunction
  { fName      = name
  , fFixity    = Prefix
  , fTypeCheck = tConcatFastasEach
  , fTypeDesc  = name ++ " : fa.list.list -> fa.list"
  , fRules     = rEach aConcat
  }
  where
    name = "concat_fastas_each"

tConcatFastas :: [CutType] -> Either String CutType
tConcatFastas [ListOf x] | elem x [faa, fna] = Right x
tConcatFastas _ = Left "expected a list of fasta files (of the same type)"

tConcatFastasEach :: [CutType] -> Either String CutType
tConcatFastasEach [ListOf (ListOf x)] | elem x [faa, fna] = Right $ ListOf x
tConcatFastasEach _ = Left "expected a list of fasta files (of the same type)"

aConcat :: CutConfig -> Locks -> [CutPath] -> Action ()
aConcat cfg ref [oPath, fsPath] = do
  faPaths <- readPaths cfg ref fs'
  let faPaths' = map (fromCutPath cfg) faPaths
  debugNeed cfg "aConcat" faPaths'
  let out'    = fromCutPath cfg oPath
      out''   = debugA cfg "aConcat" out' [out', fs']
      catArgs = faPaths' ++ [">", out']
  wrappedCmdWrite cfg ref out'' faPaths' [] [Shell] "cat"
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
  , fRules     = rEach $ aSplit name ext
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
