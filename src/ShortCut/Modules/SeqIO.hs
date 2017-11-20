-- TODO rename something more general like SeqUtils?

module ShortCut.Modules.SeqIO where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions       (wrappedCmd, debugTrackWrite, readPaths)
import ShortCut.Core.Debug         (debug, debugAction)
import ShortCut.Core.Paths         (fromCutPath, CutPath)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, mkLoad,
                                    mkLoadList, rSimple, rSimpleScript)
import ShortCut.Core.Compile.Each  (rEach, rSimpleScriptEach)

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
  { fName      = "gbk_to_faa"
  , fTypeCheck = defaultTypeCheck [gbk] faa
  , fFixity    = Prefix
  , fRules     = rSimpleScript "gbk_to_faa.py"
  }

gbkToFaaEach :: CutFunction
gbkToFaaEach = CutFunction
  { fName      = "gbk_to_faa_each"
  , fTypeCheck = defaultTypeCheck [ListOf gbk] (ListOf faa)
  , fFixity    = Prefix
  , fRules     = rSimpleScriptEach "gbk_to_faa.py"
  }

gbkToFna :: CutFunction
gbkToFna = CutFunction
  { fName      = "gbk_to_fna"
  , fTypeCheck = defaultTypeCheck [gbk] fna
  , fFixity    = Prefix
  , fRules     = rSimpleScript "gbk_to_fna.py"
  }

gbkToFnaEach :: CutFunction
gbkToFnaEach = CutFunction
  { fName      = "gbk_to_fna_each"
  , fTypeCheck = defaultTypeCheck [ListOf gbk] (ListOf fna)
  , fFixity    = Prefix
  , fRules     = rSimpleScriptEach "gbk_to_fna.py"
  }

------------------------
-- extract_ids(_each) --
------------------------

-- TODO this needs to do relative paths again, not absolute!
-- TODO also extract them from genbank files

extractIds :: CutFunction
extractIds = CutFunction
  { fName      = "extract_ids"
  , fFixity    = Prefix
  , fTypeCheck = tExtractIds
  , fRules     = rSimpleScript "extract_ids.py"
  }

extractIdsEach :: CutFunction
extractIdsEach = CutFunction
  { fName      = "extract_ids_each"
  , fFixity    = Prefix
  , fTypeCheck = tExtractIdsEach
  , fRules     = rSimpleScriptEach "extract_ids.py"
  }

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
  { fName      = "extract_seqs"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqs
  , fRules     = rSimpleScript "extract_seqs.py"
  }

extractSeqsEach :: CutFunction
extractSeqsEach = CutFunction
  { fName      = "extract_seqs_each"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqsEach
  , fRules     = rSimpleScriptEach "extract_seqs.py"
  }

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
  { fName      = "translate"
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [fna] faa
  , fRules     = rSimpleScript "translate.py"
  }

translateEach :: CutFunction
translateEach = CutFunction
  { fName      = "translate_each"
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [ListOf fna] (ListOf faa)
  , fRules     = rSimpleScriptEach "translate.py"
  }

--------------------------
-- concat_fastas(_each) --
--------------------------

concatFastas :: CutFunction
concatFastas = CutFunction
  { fName      = "concat_fastas"
  , fFixity    = Prefix
  , fTypeCheck = tConcatFastas
  , fRules     = rSimple aConcat
  }

concatFastasEach :: CutFunction
concatFastasEach = CutFunction
  { fName      = "concat_fastas_each"
  , fFixity    = Prefix
  , fTypeCheck = tConcatFastasEach
  , fRules     = rEach aConcat
  }

tConcatFastas :: [CutType] -> Either String CutType
tConcatFastas [ListOf x] | elem x [faa, fna] = Right x
tConcatFastas _ = Left "expected a list of fasta files (of the same type)"

tConcatFastasEach :: [CutType] -> Either String CutType
tConcatFastasEach [ListOf (ListOf x)] | elem x [faa, fna] = Right $ ListOf x
tConcatFastasEach _ = Left "expected a list of fasta files (of the same type)"

aConcat :: CutConfig -> [CutPath] -> Action ()
aConcat cfg [oPath, fsPath] = do
  faPaths <- readPaths cfg fs'
  let faPaths' = map (fromCutPath cfg) faPaths
  need (debug cfg ("faPaths: " ++ show faPaths) faPaths')
  let out'    = fromCutPath cfg oPath
      out''   = debugAction cfg "aConcat" out' [out', fs']
      catArgs = faPaths' ++ [">", out']
  unit $ quietly $ wrappedCmd cfg [out''] [Shell] "cat"
                     (debug cfg ("catArgs: " ++ show catArgs) catArgs)
  debugTrackWrite cfg [out'']
  where
    fs' = fromCutPath cfg fsPath
aConcat _ _ = error "bad argument to aConcat"
