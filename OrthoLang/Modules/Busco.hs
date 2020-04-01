module OrthoLang.Modules.Busco
  where

-- TODO update to BUSCO v4.0.0
-- TODO add old datasets? maybe no need

import Development.Shake
import OrthoLang.Core

import Control.Monad             (when)
import Data.List                 ((\\))
import Data.Maybe                (isJust, fromJust)
import Data.Scientific           (Scientific)
import OrthoLang.Core            (resolveSymlinks, unlessExists, headOrDie)
import OrthoLang.Modules.BlastDB (aFilterList)
import OrthoLang.Modules.SeqIO   (fna, faa, mkConcat)
import System.Directory          (createDirectoryIfMissing)
import System.Exit               (ExitCode(..))
import System.FilePath           (takeBaseName, takeDirectory, (<.>), (</>))
import System.FilePath.Glob      (glob)

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "Busco"
  , mDesc = "Benchmarking Universal Single-Copy Orthologs"
  , mTypes = [blh, bsr, bst, faa]
  , mFunctions =
      [ loadLineage
      , buscoListLineages
      , buscoFetchLineage
      , buscoProteins       , buscoProteinsEach
      , buscoTranscriptome  , buscoTranscriptomeEach
      , buscoPercentComplete, buscoPercentCompleteEach
      , buscoScoresTable
      , buscoFilterCompleteness
      , mkConcat bst -- TODO Each too?
      ]
  }

blh :: Type
blh = Type
  { tExt  = "blh"
  , tDesc = "BUSCO lineage HMMs"
  , tShow = defaultShow
  }

bsr :: Type
bsr = Type
  { tExt  = "bsr"
  , tDesc = "BUSCO results"
  , tShow = \_ ref path -> do
      txt <- readFileStrict ref path
      let tail9 = unlines . filter (not . null) . reverse . take 9 . reverse . lines
      return $ init $ "BUSCO result:" ++ tail9 txt
  }

bst :: Type
bst = Type
  { tExt  = "bst"
  , tDesc = "BUSCO scores table"
  , tShow = defaultShow
  }

loadLineage :: Function
loadLineage = mkLoad False "load_lineage" blh

buscoCache :: Config -> Path
buscoCache cfg = cacheDir cfg "busco"

-------------------------
-- busco_list_lineages --
-------------------------

buscoListLineages :: Function
buscoListLineages = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str] (ListOf str)
  , fTypeDesc  = mkTypeDesc name  [str] (ListOf str)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rBuscoListLineages
  }
  where
    name = "busco_list_lineages"

rBuscoListLineages :: RulesFn
rBuscoListLineages scr e@(Fun _ _ _ _ [f]) = do
  (ExprPath fPath) <- rExpr scr f
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let oPath   = exprPath cfg dRef scr e
      tmpDir  = buscoCache cfg
      tmpDir' = fromPath cfg tmpDir
      listTmp = tmpDir' </> "dblist" <.> "txt"
      oPath'  = fromPath cfg oPath
      lTmp'   = toPath   cfg listTmp
  let fPath' = toPath   cfg fPath
  listTmp %> \_ -> aBuscoListLineages lTmp'
  oPath'  %> \_ -> aFilterList oPath lTmp' fPath'
  return (ExprPath oPath')
rBuscoListLineages _ _ = fail "bad argument to rBuscoListLineages"

aBuscoListLineages :: Path -> Action ()
aBuscoListLineages listTmp = do
  cfg <- fmap fromJust getShakeExtra
  let listTmp' = fromPath cfg listTmp
      tmpDir   = takeDirectory $ listTmp'
      oPath    = traceA "aBuscoListLineages" listTmp' [listTmp']
  liftIO $ createDirectoryIfMissing True tmpDir
  writeLits oPath allLineages
  where
    -- These seem static, but may have to be updated later.
    -- The list is generated by "Download all datasets" on the homepage
    allLineages =
      -- Bacteria
      [ "v2/datasets/bacteria_odb9"
      , "v2/datasets/proteobacteria_odb9"
      , "v2/datasets/rhizobiales_odb9"
      , "v2/datasets/betaproteobacteria_odb9"
      , "v2/datasets/gammaproteobacteria_odb9"
      , "v2/datasets/enterobacteriales_odb9"
      , "v2/datasets/deltaepsilonsub_odb9"
      , "v2/datasets/actinobacteria_odb9"
      , "v2/datasets/cyanobacteria_odb9"
      , "v2/datasets/firmicutes_odb9"
      , "v2/datasets/clostridia_odb9"
      , "v2/datasets/lactobacillales_odb9"
      , "v2/datasets/bacillales_odb9"
      , "v2/datasets/bacteroidetes_odb9"
      , "v2/datasets/spirochaetes_odb9"
      , "v2/datasets/tenericutes_odb9"
      -- Eukaryota
      , "v2/datasets/eukaryota_odb9"
      , "v2/datasets/fungi_odb9"
      , "v2/datasets/microsporidia_odb9"
      , "v2/datasets/dikarya_odb9"
      , "v2/datasets/ascomycota_odb9"
      , "v2/datasets/pezizomycotina_odb9"
      , "v2/datasets/eurotiomycetes_odb9"
      , "v2/datasets/sordariomyceta_odb9"
      , "v2/datasets/saccharomyceta_odb9"
      , "v2/datasets/saccharomycetales_odb9"
      , "v2/datasets/basidiomycota_odb9"
      , "v2/datasets/metazoa_odb9"
      , "v2/datasets/nematoda_odb9"
      , "v2/datasets/arthropoda_odb9"
      , "v2/datasets/insecta_odb9"
      , "v2/datasets/endopterygota_odb9"
      , "v2/datasets/hymenoptera_odb9"
      , "v2/datasets/diptera_odb9"
      , "v2/datasets/vertebrata_odb9"
      , "v2/datasets/actinopterygii_odb9"
      , "v2/datasets/tetrapoda_odb9"
      , "v2/datasets/aves_odb9"
      , "v2/datasets/mammalia_odb9"
      , "v2/datasets/euarchontoglires_odb9"
      , "v2/datasets/laurasiatheria_odb9"
      , "v2/datasets/embryophyta_odb9"
      , "v2/datasets/protists_ensembl"
      , "v2/datasets/alveolata_stramenophiles_ensembl"
      -- prerelease
      , "datasets/prerelease/chlorophyta_odb10"
      , "datasets/prerelease/embryophyta_odb10"
      , "datasets/prerelease/eudicotyledons_odb10"
      , "datasets/prerelease/liliopsida_odb10"
      , "datasets/prerelease/solanaceae_odb10"
      , "datasets/prerelease/viridiplantae_odb10"

      ]

------------------------
-- busco_fetch_lineage --
------------------------

-- TODO consistent naming with similar functions
-- TODO busco_fetch_lineages? (the _each version)

buscoFetchLineage :: Function
buscoFetchLineage  = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [str] blh
  , fTypeDesc  = mkTypeDesc name  [str] blh
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rBuscoFetchLineage
  }
  where
    name = "busco_fetch_lineage"

-- TODO move to Util?
untar :: Path -> Path -> Action ()
untar from to = do
  cfg <- fmap fromJust getShakeExtra
  let from' = fromPath cfg from
      to' = fromPath cfg to
  runCmd $ CmdDesc
    { cmdBinary = "tar"
    , cmdArguments = (if isJust (cfgDebug cfg) then "-v" else ""):["-xf", from', "-C", takeDirectory to']
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

rBuscoFetchLineage :: RulesFn
rBuscoFetchLineage scr expr@(Fun _ _ _ _ [nPath]) = do
  (ExprPath namePath) <- rExpr scr nPath
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let outPath  = exprPath cfg dRef scr expr
      outPath' = fromPath cfg outPath
      blhDir   = (fromPath cfg $ buscoCache cfg) </> "lineages"
  outPath' %> \_ -> do
    nameStr <- readLit namePath
    let untarPath = blhDir </> nameStr
        url       = "http://busco.ezlab.org/" ++ nameStr ++ ".tar.gz"
        datasetPath'  = untarPath </> "dataset.cfg" -- final output we link to
        datasetPath   = toPath cfg datasetPath'
    tarPath <- fmap (fromPath cfg) $ curl url
    unlessExists untarPath $ do
      untar (toPath cfg tarPath) (toPath cfg untarPath)
    symlink outPath datasetPath
  return $ ExprPath outPath'
rBuscoFetchLineage _ e = error $ "bad argument to rBuscoFetchLineage: " ++ show e

-------------------------------------------
-- busco_{genome,proteins,transcriptome} --
-------------------------------------------

mkBusco :: String -> String -> Type -> Function
mkBusco name mode inType = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [blh, inType] bsr
  , fTypeDesc  = mkTypeDesc name  [blh, inType] bsr
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple $ aBusco mode
  }

buscoProteins, buscoTranscriptome :: Function
buscoProteins      = mkBusco "busco_proteins"      "prot" faa
buscoTranscriptome = mkBusco "busco_transcriptome" "tran" fna
-- buscoGenome = mkBusco "busco_genome" "geno"

aBusco :: String -> ([Path] -> Action ())
aBusco mode [outPath, blhPath, faaPath] = do
  cfg <- fmap fromJust getShakeExtra
  let out' = fromPath cfg outPath
      blh' = takeDirectory $ fromPath cfg blhPath
      cDir = fromPath cfg $ buscoCache cfg
      rDir = cDir </> "runs"
      faa' = fromPath cfg faaPath
  blh'' <- liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) blh'
  liftIO $ createDirectoryIfMissing True rDir
  runCmd $ CmdDesc
    { cmdBinary = "busco.sh"
    , cmdArguments = [out', faa', blh'', mode, cDir] -- TODO cfgtemplate, tdir
    , cmdFixEmpties = False
    , cmdParallel = False -- TODO fix shake error and set to True
    , cmdInPatterns = [faa']
    , cmdOutPath = out'
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [out']
    }
  -- This is rediculous but I haven't been able to shorten it...
  let oBasePtn = "*" ++ takeBaseName out' ++ "*"
      tmpOutPtn = rDir </> oBasePtn </> "short_summary*.txt"
  tmpOut <- liftIO $ fmap (headOrDie "failed to read BUSCO summary in aBusco") $ glob tmpOutPtn
  sanitizeFileInPlace tmpOut -- will this confuse shake?
  symlink outPath $ toPath cfg tmpOut
aBusco _ as = error $ "bad argument to aBusco: " ++ show as

------------------------------------------------
-- busco_{genome,proteins,transcriptome}_each --
------------------------------------------------

mkBuscoEach :: String -> String -> Type -> Function
mkBuscoEach name mode inType = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [blh, (ListOf inType)] (ListOf bsr)
  , fTypeDesc  = mkTypeDesc name  [blh, (ListOf inType)] (ListOf bsr)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMap 2 $ aBusco mode
  }

buscoProteinsEach, buscoTranscriptomeEach :: Function
buscoProteinsEach      = mkBuscoEach "busco_proteins_each"      "prot" faa
buscoTranscriptomeEach = mkBuscoEach "busco_transcriptome_each" "tran" fna
-- buscoGenomeEach = mkBusco "busco_genome_each" "geno"

-----------------------------
-- busco_percent_complete* --
-----------------------------

buscoPercentComplete :: Function
buscoPercentComplete  = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [bsr] num
  , fTypeDesc  = mkTypeDesc name  [bsr] num
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimpleScript "busco_percent_complete.sh"
  }
  where
    name = "busco_percent_complete"

buscoPercentCompleteEach :: Function
buscoPercentCompleteEach  = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [ListOf bsr] (ListOf num)
  , fTypeDesc  = mkTypeDesc name  [ListOf bsr] (ListOf num)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rMapSimpleScript 1 "busco_percent_complete.sh"
  }
  where
    name = "busco_percent_complete_each"

------------------------
-- busco_scores_table --
------------------------

buscoScoresTable :: Function
buscoScoresTable  = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [ListOf bsr] bst
  , fTypeDesc  = mkTypeDesc name  [ListOf bsr] bst
  ,fTags = []
  -- , fNewRules = Nothing, fOldRules = rSimpleScript $ name <.> "py"
  , fNewRules = Nothing, fOldRules = rBuscoScoresTable
  }
  where
    name = "busco_scores_table"

-- TODO variant of rSimpleScript that reads + passes in a list of input files?
rBuscoScoresTable :: RulesFn
rBuscoScoresTable scr e@(Fun _ _ _ _ [l]) = do
  (ExprPath lsPath) <- rExpr scr l
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let o  = exprPath cfg dRef scr e
      o' = fromPath cfg o
  o' %> \_ -> do
    ins <- readPaths lsPath
    let ins' = map (fromPath cfg) ins
    runCmd $ CmdDesc
      { cmdBinary = "busco_scores_table.py"
      , cmdArguments = o':ins'
      , cmdFixEmpties = False
      , cmdParallel   = False
      , cmdInPatterns = ins'
      , cmdOutPath    = o'
      , cmdExtraOutPaths = []
      , cmdSanitizePaths = [] -- TODO any?
      , cmdOptions = []
      , cmdExitCode = ExitSuccess
      , cmdRmPatterns = [o']
      }
  return $ ExprPath o'
rBuscoScoresTable _ e = error $ "bad argument to rBuscoScoresTable: " ++ show e

-------------------------------
-- busco_filter_completeness --
-------------------------------

-- TODO this can filter proteomes/transcriptomes by which their completeness in a table
--      bst should it take the table as an explicit arg, or generate it from the inputs?
--      explicit is probably better! abort with error if the table doesn't contain all of them
-- TODO remove busco_percent_complete* afterward since the table will be more useful?
-- TODO make an _each version of this one

buscoFilterCompleteness :: Function
buscoFilterCompleteness  = Function
  { fOpChar = Nothing, fName = name
  , fTypeCheck = defaultTypeCheck name [num, bst, ListOf faa] (ListOf faa) -- TODO or fna?
  , fTypeDesc  = mkTypeDesc name  [num, bst, ListOf faa] (ListOf faa) -- TODO or fna?
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rBuscoFilterCompleteness
  }
  where
    name = "busco_filter_completeness"

-- TODO how to get the hash? resolveSymlinks and read it from the filename?
--      that might fail if it was generated by a fn instead of loaded from an external file
--      maybe the solution is to add generated fastas to cached lines?
-- TODO try the same way it works for sets: one canonical full path!
-- TODO do it the simple way for now, then see if it breaks and if so fix it
rBuscoFilterCompleteness :: RulesFn
rBuscoFilterCompleteness scr e@(Fun _ _ _ _ [m, t, fs]) = do
  (ExprPath scorePath) <- rExpr scr m
  (ExprPath tablePath) <- rExpr scr t
  (ExprPath faasList ) <- rExpr scr fs
  cfg  <- fmap fromJust getShakeExtraRules
  dRef <- fmap fromJust getShakeExtraRules
  let out  = exprPath cfg dRef scr e
      out' = fromPath cfg out
  out' %> \_ -> do
    score <- fmap (read :: String -> Scientific) $ readLit scorePath
    table <- readFileStrict' tablePath -- TODO best read fn?
    faaPaths <- readPaths faasList
    let allScores = map parseWords $ map words $ lines table
        missing   = faaPaths \\ map fst allScores
        okPaths   = map fst $ filter (\(_, c) -> c >= score) allScores
    when (not $ null missing) $
      error $ "these paths are missing from the table: " ++ show missing
    writePaths out' okPaths
  return $ ExprPath out'
  where
    parseWords (p:c:_) = (Path p, read c :: Scientific)
    parseWords ws = error $ "bad argument to parseWords: " ++ show ws
rBuscoFilterCompleteness _ e = error $
  "bad argument to rBuscoFilterCompleteness: " ++ show e
