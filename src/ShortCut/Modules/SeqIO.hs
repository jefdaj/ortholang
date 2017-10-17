-- TODO rename something more general like SeqUtils

module ShortCut.Modules.SeqIO where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Config        (wrappedCmd)
import ShortCut.Core.Debug         (debug, debugTrackWrite, debugAction)
import ShortCut.Core.Paths         (exprPath, toCutPath,
                                    fromCutPath, readPaths, CutPath)
import ShortCut.Core.Compile.Basic (rExpr, defaultTypeCheck, rLoadOne,
                                    rLoadList, rSimple, rSimpleScript)
import ShortCut.Core.Compile.Each  (rEach, rSimpleScriptEach)

cutModule :: CutModule
cutModule = CutModule
  { mName = "seqio"
  , mFunctions =
    [ loadGbk , loadGbkEach
    , loadFaa , loadFaaEach
    , loadFna , loadFnaEach
    , gbkToFaa, gbkToFaaEach
    , gbkToFna, gbkToFnaEach
    , extractSeqs
    , extractIds
    , translate, translateEach
    , concatFastas, concatFastasEach
    -- TODO combo that loads multiple fnas or faas and concats them?
    -- TODO combo that loads multiple gbks -> fna or faa?
    ]
  }

-- load a single file --

{- Takes a string with the filepath to load. Creates a trivial expression file
 - that's just a symlink to the given path. These should be the only absolute
 - links, and the only ones that point outside the temp dir.
 - TODO still true?
 -}
mkLoad :: String -> CutType -> CutFunction
mkLoad name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] rtn
  , fFixity    = Prefix
  , fRules     = rLoadOne
  }

-- load a list of files --

{- Like cLoad, except it operates on a list of strings. Note that you can also
 - load lists using cLoad, but it's not recommended because then you have to
 - write the list in a file, whereas this can handle literal lists in the
 - source code.
 -}

-- TODO fix it putting both the initial files and lists of them in the same dir!
--      (.faa and .faa.list are together in exprs/load_faa_each,
--       when the former should be in exprs/load_faa)
mkLoadList :: String -> CutType -> CutFunction
mkLoadList name rtn = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [(ListOf str)] (ListOf rtn)
  , fFixity    = Prefix
  , fRules     = rLoadList
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

--------------------
-- load sequences --
--------------------

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

-------------------------------------------
-- extract sequence IDs from FASTA files --
-------------------------------------------

-- TODO this needs to do relative paths again, not absolute!
-- TODO also extract them from genbank files

extractIds :: CutFunction
extractIds = CutFunction
  { fName      = "extract_ids"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqIDs
  , fRules     = rSimpleScript "extract_ids.py"
  }

-- TODO write this
-- extractIdsEach :: CutFunction
-- extractIdsEach = CutFunction
--   { fName      = "extract_ids_each"
--   , fFixity    = Prefix
--   , fTypeCheck = tExtractSeqIDsEach
--   , fRules     = rExtractSeqIDs
--   }

tExtractSeqIDs :: [CutType] -> Either String CutType
tExtractSeqIDs [x] | elem x [faa, fna] = Right (ListOf str)
tExtractSeqIDs _ = Left "expected a fasta file"

----------------------------------------------
-- extract sequences from FASTA files by ID --
----------------------------------------------

-- TODO also extract them from genbank files

extractSeqs :: CutFunction
extractSeqs = CutFunction
  { fName      = "extract_seqs"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqs
  , fRules     = rSimpleScript "extract_seqs.py"
  }

tExtractSeqs  :: [CutType] -> Either String CutType
tExtractSeqs [x, ListOf s] | s == str && elem x [faa, fna] = Right x
tExtractSeqs _ = Left "expected a fasta file and a list of strings"

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

-------------------
-- concat_fastas --
-------------------

concatFastas :: CutFunction
concatFastas = CutFunction
  { fName      = "concat_fastas"
  , fFixity    = Prefix
  , fTypeCheck = tConcatFastas
  , fRules  = rSimple aConcat
  }

concatFastasEach :: CutFunction
concatFastasEach = CutFunction
  { fName      = "concat_fastas_each"
  , fFixity    = Prefix
  , fTypeCheck = tConcatFastasEach
  , fRules  = rEach aConcat
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
