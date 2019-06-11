module ShortCut.Modules.Busco
  where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Paths (cacheDir, toCutPath, fromCutPath, exprPath)
import ShortCut.Core.Actions (debugA, writeLits, runCmd, CmdDesc(..), readLit, symlink, readFileStrict)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rExpr, mkLoad, rSimple, curl)
import ShortCut.Modules.SeqIO (fna, faa)
import ShortCut.Modules.BlastDB (aFilterList)
import System.FilePath (takeBaseName, takeDirectory, (<.>), (</>))
import System.Directory           (createDirectoryIfMissing)
import ShortCut.Core.Util         (resolveSymlinks, unlessExists)
import System.Exit (ExitCode(..))
import System.FilePath.Glob       (glob)

cutModule :: CutModule
cutModule = CutModule
  { mName = "Busco"
  , mDesc = "Benchmarking Universal Single-Copy Orthologs"
  , mTypes = [bul, bur, faa]
  , mFunctions =
      [ loadLineage
      , buscoListLineages
      , buscoFetchLineage
      , buscoProteins
      , buscoTranscriptome
      -- TODO buscoGenome (have to package Augustus first?)
      -- TODO each versions
      ]
  }

bul :: CutType
bul = CutType
  { tExt  = "bul"
  , tDesc = "BUSCO lineage" -- TODO call it something better like database?
  , tShow = defaultShowN 6
  }

bur :: CutType
bur = CutType
  { tExt  = "bur"
  , tDesc = "BUSCO results"
  , tShow = \_ ref path -> do
      txt <- readFileStrict ref path
      let tail9 = unlines . reverse . take 9 . reverse . lines
      return $ init $ "BUSCO result:\n" ++ tail9 txt
  }

loadLineage :: CutFunction
loadLineage = mkLoad False "load_lineage" bul

buscoCache :: CutConfig -> CutPath
buscoCache cfg = cacheDir cfg "busco"

-------------------------
-- busco_list_lineages --
-------------------------

buscoListLineages :: CutFunction
buscoListLineages = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [str] (ListOf str)
  , fDesc      = Nothing
  , fFixity    = Prefix
  , fRules     = rBuscoListLineages
  }
  where
    name = "busco_list_lineages"

rBuscoListLineages :: RulesFn
rBuscoListLineages s@(_, cfg, ref, ids) e@(CutFun _ _ _ _ [f]) = do
  (ExprPath fPath) <- rExpr s f
  let fPath' = toCutPath   cfg fPath
  listTmp %> \_ -> aBuscoListLineages   cfg ref ids lTmp'
  oPath'  %> \_ -> aFilterList cfg ref ids oPath lTmp' fPath'
  return (ExprPath oPath')
  where
    oPath   = exprPath s e
    tmpDir  = buscoCache cfg
    tmpDir' = fromCutPath cfg tmpDir
    listTmp = tmpDir' </> "dblist" <.> "txt"
    oPath'  = fromCutPath cfg oPath
    lTmp'   = toCutPath   cfg listTmp
rBuscoListLineages _ _ = fail "bad argument to rBuscoListLineages"

aBuscoListLineages :: CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> Action ()
aBuscoListLineages cfg ref _ listTmp = do
  liftIO $ createDirectoryIfMissing True tmpDir
  writeLits cfg ref oPath allLineages
  where
    listTmp' = fromCutPath cfg listTmp
    tmpDir   = takeDirectory $ listTmp'
    oPath    = debugA cfg "aBuscoListLineages" listTmp' [listTmp']
    -- These seem static, but may have to be updated later.
    -- The list is generated by "Download all datasets" on the homepage
    allLineages =
      [ "bacteria_odb9"
      , "proteobacteria_odb9"
      , "rhizobiales_odb9"
      , "betaproteobacteria_odb9"
      , "gammaproteobacteria_odb9"
      , "enterobacteriales_odb9"
      , "deltaepsilonsub_odb9"
      , "actinobacteria_odb9"
      , "cyanobacteria_odb9"
      , "firmicutes_odb9"
      , "clostridia_odb9"
      , "lactobacillales_odb9"
      , "bacillales_odb9"
      , "bacteroidetes_odb9"
      , "spirochaetes_odb9"
      , "tenericutes_odb9"
      , "eukaryota_odb9"
      , "fungi_odb9"
      , "microsporidia_odb9"
      , "dikarya_odb9"
      , "ascomycota_odb9"
      , "pezizomycotina_odb9"
      , "eurotiomycetes_odb9"
      , "sordariomyceta_odb9"
      , "saccharomyceta_odb9"
      , "saccharomycetales_odb9"
      , "basidiomycota_odb9"
      , "metazoa_odb9"
      , "nematoda_odb9"
      , "arthropoda_odb9"
      , "insecta_odb9"
      , "endopterygota_odb9"
      , "hymenoptera_odb9"
      , "diptera_odb9"
      , "vertebrata_odb9"
      , "actinopterygii_odb9"
      , "tetrapoda_odb9"
      , "aves_odb9"
      , "mammalia_odb9"
      , "euarchontoglires_odb9"
      , "laurasiatheria_odb9"
      , "embryophyta_odb9"
      , "protists_ensembl"
      , "alveolata_stramenophiles_ensembl"
      ]

------------------------
-- busco_fetch_lineage --
------------------------

-- TODO consistent naming with similar functions

buscoFetchLineage :: CutFunction
buscoFetchLineage  = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [str] bul
  , fTypeDesc  = mkTypeDesc name  [str] bul
  , fDesc      = Nothing
  , fFixity    = Prefix
  , fRules     = rBuscoFetchLineage
  }
  where
    name = "busco_fetch_lineage"

-- TODO move to Util?
untar :: CutConfig -> Locks -> CutPath -> CutPath -> Action ()
untar cfg ref from to = runCmd cfg ref $ CmdDesc
  { cmdBinary = "tar"
  , cmdArguments = (if cfgDebug cfg then "-v" else ""):["-xf", from', "-C", takeDirectory to']
  , cmdFixEmpties = False
  , cmdParallel   = False
  , cmdInPatterns = [from']
  , cmdOutPath    = to'
  , cmdExtraOutPaths = []
  , cmdSanitizePaths = []
  , cmdOptions = []
  , cmdExitCode = ExitSuccess
  , cmdRmPatterns = [to']
  }
  where
    from' = fromCutPath cfg from
    to' = fromCutPath cfg to

-- TODO also have to untar it in the lineages dir
-- TODO and then might as well show dataset.cfg
rBuscoFetchLineage :: RulesFn
-- rBuscoFetchLineage st expr = (fRules loadLineage) st $ withBuscoUrl expr
-- type CutState = (CutScript, CutConfig, Locks, HashedSeqIDsRef)
rBuscoFetchLineage st@(_, cfg, ref, _) expr@(CutFun _ _ _ _ [nPath]) = do
  (ExprPath namePath) <- rExpr st nPath
  let outPath  = exprPath st expr
      outPath' = fromCutPath cfg outPath
      bulDir   = (fromCutPath cfg $ buscoCache cfg) </> "lineages"
  outPath' %> \_ -> do
    nameStr <- readLit cfg ref namePath
    let untarPath = bulDir </> nameStr
        url       = "http://busco.ezlab.org/v2/datasets/" ++ nameStr ++ ".tar.gz"
        datasetPath'  = untarPath </> "dataset.cfg" -- final output we link to
        datasetPath   = toCutPath cfg datasetPath'
    -- liftIO $ putStrLn $ "nameStr:   '" ++ nameStr ++ "'"
    -- liftIO $ putStrLn $ "untarPath: '" ++ untarPath ++ "'"
    -- liftIO $ putStrLn $ "url:       '" ++ url ++ "'"
    -- liftIO $ createDirectoryIfMissing True bulDir
    tarPath <- fmap (fromCutPath cfg) $ curl cfg ref url
    -- liftIO $ putStrLn $ "tarPath: '" ++ tarPath ++ "'"
    unlessExists untarPath $ do
      untar cfg ref (toCutPath cfg tarPath) (toCutPath cfg untarPath)
    symlink cfg ref outPath datasetPath
  return $ ExprPath outPath'
rBuscoFetchLineage _ e = error $ "bad argument to rBuscoFetchLineage: " ++ show e

-------------------------------------------
-- busco_{genome,proteins,transcriptome} --
-------------------------------------------

mkBusco :: String -> String -> CutType -> CutFunction
mkBusco name mode inType = CutFunction
  { fName      = name
  , fTypeCheck = defaultTypeCheck [bul, inType] bur
  , fTypeDesc  = mkTypeDesc name  [bul, inType] bur
  , fDesc      = Nothing
  , fFixity    = Prefix
  , fRules     = rSimple $ aBusco mode
  }

buscoProteins, buscoTranscriptome :: CutFunction
buscoProteins      = mkBusco "busco_proteins"      "prot" faa
buscoTranscriptome = mkBusco "busco_transcriptome" "tran" fna
-- buscoGenome = mkBusco "busco_genome" "geno"

-- TODO need to generate + pass in the unique config file
-- TODO need to pass only the basename prefix of the outpath?
aBusco :: String -> (CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ())
aBusco mode cfg ref _ [outPath, bulPath, faaPath] = do
  let out' = fromCutPath cfg outPath
      bul' = takeDirectory $ fromCutPath cfg bulPath
      cDir = fromCutPath cfg $ buscoCache cfg
      faa' = fromCutPath cfg faaPath
  -- liftIO $ createDirectoryIfMissing True $ fromCutPath cfg $ buscoCache cfg
  bul'' <- liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) bul'
  runCmd cfg ref $ CmdDesc
    { cmdBinary = "busco.sh"
    , cmdArguments = [out', faa', bul'', mode, cDir] -- TODO cfgtemplate, tdir
    , cmdFixEmpties = False
    , cmdParallel = False -- TODO fix shake error and set to True
    , cmdInPatterns = [faa'] -- TODO lineage file
    , cmdOutPath = out'
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out']
    }
  -- This is rediculous but I haven't been able to shorten it...
  let oBase = "*" ++ takeBaseName out' ++ "*"
      tmpOutPtn = cDir </> oBase </> oBase </> "short_summary*.txt"
  tmpOut <- liftIO $ glob tmpOutPtn
  -- liftIO $ putStrLn $ "glob: " ++ show tmpOut
  symlink cfg ref outPath $ toCutPath cfg $ head tmpOut
aBusco _ _ _ _ as = error $ "bad argument to aBusco: " ++ show as
