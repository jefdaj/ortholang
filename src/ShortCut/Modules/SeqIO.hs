-- TODO rename something more general like SeqUtils

module ShortCut.Modules.SeqIO where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Paths        (exprPath, cacheDir)
import ShortCut.Core.Compile      (rExpr)
import ShortCut.Core.Debug        (debug, debugReadLines, debugTrackWrite)
import ShortCut.Core.ModuleAPI    (mkLoad, mkLoadList, defaultTypeCheck,
                                   rOneArgScript, rOneArgListScript)
import System.FilePath            ((</>))
-- import System.Directory           (createDirectoryIfMissing)
import ShortCut.Core.Config       (wrappedCmd)

cutModule :: CutModule
cutModule = CutModule
  { mName = "seqio"
  , mFunctions =
    [ loadGbk
    , loadGbkEach
    , loadFaa
    , loadFaaEach
    , loadFna
    , loadFnaEach
    , gbkToFaa -- TODO each?
    , gbkToFna -- TODO each?
    , extractSeqs
    , extractSeqIDs
    , translate
    , concatFastas
    -- TODO combo that loads multiple fnas or faas and concats them?
    -- TODO combo that loads multiple gbks -> fna or faa?
    ]
  }

gbk :: CutType
gbk = CutType
  { tExt  = "gbk"
  , tDesc = "genbank file"
  , tShow  = defaultShow
  }

faa :: CutType
faa = CutType
  { tExt  = "faa"
  , tDesc = "FASTA (amino acid)"
  , tShow  = defaultShow
  }

fna :: CutType
fna = CutType
  { tExt  = "fna"
  , tDesc = "FASTA (nucleic acid)"
  , tShow  = defaultShow
  }

gbkToFaa :: CutFunction
gbkToFaa = CutFunction
  { fName      = "gbk_to_faa"
  , fTypeCheck = defaultTypeCheck [gbk] faa
  , fFixity    = Prefix
  , fRules  = rOneArgScript "seqio" "gbk_to_faa.py"
  }

gbkToFna :: CutFunction
gbkToFna = CutFunction
  { fName      = "gbk_to_fna"
  , fTypeCheck = defaultTypeCheck [gbk] fna
  , fFixity    = Prefix
  , fRules  = rOneArgScript "seqio" "gbk_to_fna.py"
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

extractSeqIDs :: CutFunction
extractSeqIDs = CutFunction
  { fName      = "extract_ids"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqIDs
  , fRules  = rExtractSeqIDs
  }

tExtractSeqIDs :: [CutType] -> Either String CutType
tExtractSeqIDs [x] | elem x [faa, fna] = Right (SetOf str)
tExtractSeqIDs _ = Left "expected a fasta file"

-- TODO these should put their tmpfiles in cache/extract_ids!
rExtractSeqIDs :: CutState -> CutExpr -> Rules ExprPath
rExtractSeqIDs = rOneArgListScript "seqio" "extract_ids.py"

----------------------------------------------
-- extract sequences from FASTA files by ID --
----------------------------------------------

-- TODO also extract them from genbank files

extractSeqs :: CutFunction
extractSeqs = CutFunction
  { fName      = "extract_seqs"
  , fFixity    = Prefix
  , fTypeCheck = tExtractSeqs
  , fRules  = rExtractSeqs
  }

-- TODO does SetOf str match on the value or just the constructor?
tExtractSeqs  :: [CutType] -> Either String CutType
tExtractSeqs [x, SetOf s] | s == str && elem x [faa, fna] = Right x
tExtractSeqs _ = Left "expected a list of strings and a fasta file"

-- TODO can this be replaced with rOneArgListScript?
rExtractSeqs :: CutState -> CutExpr -> Rules ExprPath
rExtractSeqs s@(_,cfg) e@(CutFun _ _ _ _ [fa, ids]) = do
  (ExprPath faPath ) <- rExpr s fa
  (ExprPath idsPath) <- rExpr s ids
  -- liftIO . putStrLn $ "extracting sequences from " ++ faPath
  let (CacheDir tmpDir ) = cacheDir cfg "seqio"
      (ExprPath outPath) = exprPath cfg True e []
      -- tmpList = cacheFile cfg "seqio" ids "txt"
  -- TODO remove extra tmpdir here if possible, and put file somewhere standard
  -- tmpList %> \_ -> do
  outPath %> \_ -> aExtractSeqs cfg outPath tmpDir faPath idsPath 
  return (ExprPath outPath)
rExtractSeqs _ _ = error "bad argument to extractSeqs"

aExtractSeqs :: CutConfig -> String -> FilePath -> FilePath -> FilePath -> Action ()
aExtractSeqs cfg outPath tmpDir faPath idsPath = do
  -- liftIO $ createDirectoryIfMissing True tmpDir -- TODO put in fromShortCutSet?
  -- fromShortCutSet cfg idsPath (ExprPath tmpList)
-- outPath %> \_ -> do
  -- need [faPath, tmpList]
  -- liftIO $ createDirectoryIfMissing True tmpDir
  need [faPath, idsPath]
  quietly $ wrappedCmd cfg [outPath] [Cwd tmpDir]
                       "extract_seqs.py" [outPath, faPath, idsPath]

-------------------------------------
-- convert between DNA and protein --
-------------------------------------

-- TODO name something else like fna_to_faa?
translate :: CutFunction
translate = CutFunction
  { fName      = "translate"
  , fFixity    = Prefix
  , fTypeCheck = defaultTypeCheck [fna] faa
  , fRules  = rConvert "translate.py"
  }

-- TODO remove as biologically invalid?
-- back_transcribe :: CutFunction
-- back_transcribe = CutFunction
--   { fName      = "back_transcribe"
--   , fFixity    = Prefix
--   , fTypeCheck = defaultTypeCheck [faa] fna
--   , fRules  = rConvert "back_transcribe.py"
--   }

-- TODO can this use rOneArgScript?
rConvert :: FilePath -> CutState -> CutExpr -> Rules ExprPath
rConvert script s@(_,cfg) e@(CutFun _ _ _ _ [fa]) = do
  (ExprPath faPath) <- rExpr s fa
  let (ExprPath oPath) = exprPath cfg True e []
  oPath %> \_ -> do
    need [faPath]
    unit $ wrappedCmd cfg [oPath] [] script [oPath, faPath]
    -- debugTrackWrite cfg [oPath] TODO is this implied?
  return (ExprPath oPath)
rConvert _ _ _ = error "bad argument to rConvert"

------------------------
-- concat fasta files --
------------------------

concatFastas :: CutFunction
concatFastas = CutFunction
  { fName      = "concat_fastas"
  , fFixity    = Prefix
  , fTypeCheck = tConcatFastas
  , fRules  = rConcat
  }

tConcatFastas :: [CutType] -> Either String CutType
tConcatFastas [SetOf x] | elem x [faa, fna] = Right x
tConcatFastas _ = Left "expected a list of fasta files (of the same type)"

rConcat :: CutState -> CutExpr -> Rules ExprPath
rConcat s@(_,cfg) e@(CutFun _ _ _ _ [fs]) = do
  (ExprPath fsPath) <- rExpr s fs
  let (ExprPath oPath) = exprPath cfg True e []
  oPath %> \_ -> do
    faPaths <- fmap (map (cfgTmpDir cfg </>)) -- TODO utility fn for this!
               (debugReadLines cfg (debug cfg ("fsPath: " ++ fsPath)
                                    fsPath))
    need (debug cfg ("faPaths: " ++ show faPaths) faPaths)
    let catArgs = faPaths ++ [">", oPath]
    unit $ quietly $ wrappedCmd cfg [oPath] [Shell] "cat"
                       (debug cfg ("catArgs: " ++ show catArgs) catArgs)
    debugTrackWrite cfg [oPath]
     -- TODO shouldn't have to read the files into memory!
    -- need fPaths
    -- txt <- fmap concat $ mapM (debugReadFile cfg) (debug cfg ("fPaths: " ++ show fPaths) fPaths)
    -- debugWriteFile cfg oPath txt
  return (ExprPath oPath)
rConcat _ _ = error "bad argument to rConcat"
