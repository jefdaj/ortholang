module ShortCut.Modules.OrthoFinder
  where

-- TODO write a function to extract_seqs from multiple fastas at once, useful here + elsewhere?
-- TODO can all "extract" functions be renamed with "list"?
-- TODO try DIAMOND, MMseqs2

import Development.Shake
import ShortCut.Core.Types

import Data.List                   (isPrefixOf)
import ShortCut.Core.Actions       (debugA, debugNeed, readPaths, symlink, wrappedCmd,
                                    readLit, readLits, writeLits, cachedLinesPath, writePaths)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Util          (digest, readFileStrict, unlessExists, resolveSymlinks)
import ShortCut.Modules.SeqIO      (faa)
import System.Directory            (createDirectoryIfMissing, renameDirectory)
import System.FilePath             ((</>), takeFileName, takeDirectory)
import Control.Monad               (forM)

cutModule :: CutModule
cutModule = CutModule
  { mName = "OrthoFinder"
  , mDesc = "Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree"
  , mTypes = [faa, ofr]
  , mFunctions =
      [ orthofinder
      , orthogroups
      , orthogroupContaining
      , orthogroupsContaining
      ]
  }

ofr :: CutType
ofr = CutType
  { tExt  = "ofr"
  , tDesc = "OrthoFinder results"
  , tShow = \_ ref path -> do
      txt <- readFileStrict ref path
      return $ unlines $ take 17 $ lines txt
  }

-----------------
-- orthofinder --
-----------------

orthofinder :: CutFunction
orthofinder = let name = "orthofinder" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ListOf faa] ofr
  , fTypeCheck = defaultTypeCheck [ListOf faa] ofr
  , fDesc      = Just "Run OrthoFinder on a list of genomes in FASTA format.\n\
                      \It produces lots of result files! Use the extract_* functions\n\
                      \or look in the TMPDIR to find the specific info you want."
  , fFixity    = Prefix
  , fRules     = rSimple aOrthofinder
  }

-- TODO do blast separately and link to outputs from the WorkingDirectory dir, and check if same results
-- TODO what's diamond blast? do i need to add it?
aOrthofinder :: CutConfig -> Locks -> [CutPath] -> Action ()
aOrthofinder cfg ref [out, faListPath] = do
  let tmpDir = cfgTmpDir cfg </> "cache" </> "orthofinder" </> digest faListPath
      resDir = tmpDir </> "result"
  unlessExists resDir $ do
    liftIO $ createDirectoryIfMissing True tmpDir
    faPaths <- readPaths cfg ref faListPath'
    let faPaths' = map (fromCutPath cfg) faPaths
    debugNeed cfg "aOrthofinder" faPaths'
    let faLinks = map (\p -> toCutPath cfg $ tmpDir </> (takeFileName $ fromCutPath cfg p)) faPaths
    mapM_ (\(p, l) -> symlink cfg ref l p) $ zip faPaths faLinks
    (o, e, _) <- wrappedCmd True False cfg ref (Just out'') faPaths' [] "orthofinder"
      [ "-f", tmpDir
      , "-S", "diamond" -- use DIAMOND instead of BLAST+
      , "-t", "8" -- TODO figure out with shake or ghc
      , "-a", "8" -- TODO figure out with shake or ghc
      ]
    putNormal $ unlines [o, e] -- TODO remove
    resName <- fmap last $ fmap (filter $ \p -> "Results_" `isPrefixOf` p) $ getDirectoryContents $ tmpDir </> "OrthoFinder"
    liftIO $ renameDirectory (tmpDir </> "OrthoFinder" </> resName) resDir
  symlink cfg ref out $ toCutPath cfg $ resDir </> "Comparative_Genomics_Statistics" </> "Statistics_Overall.tsv"
  where
    out'        = fromCutPath cfg out
    faListPath' = fromCutPath cfg faListPath
    out''       = debugA cfg "aOrthofinder" out' [out', faListPath']
aOrthofinder _ _ args = error $ "bad argument to aOrthofinder: " ++ show args

-----------------
-- orthogroups --
-----------------

-- TODO list_groups?
-- TODO separate module that works with multiple ortholog programs?
-- TODO version to get the group matching an ID
orthogroups :: CutFunction
orthogroups = let name = "orthogroups" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ofr] (ListOf (ListOf str))
  , fTypeCheck = defaultTypeCheck [ofr] (ListOf (ListOf str))
  , fDesc      = Just "List genes in all orthogroups."
  , fFixity    = Prefix
  , fRules     = rSimple aOrthogroups
  }

findResDir :: CutConfig -> FilePath -> IO FilePath
findResDir cfg outPath = do
  statsPath <- resolveSymlinks (Just $ cfgTmpDir cfg) outPath
  return $ takeDirectory $ takeDirectory statsPath

readOrthogroups :: FilePath -> Action [[String]]
readOrthogroups resDir = do
  let orthoPath = resDir </> "Orthogroups" </> "Orthogroups.txt"
  txt <- readFile' orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  return groups

writeOrthogroups :: CutConfig -> Locks -> CutPath -> [[String]] -> Action ()
writeOrthogroups cfg ref out groups = do
  paths <- forM groups $ \group -> do
    let path = cachedLinesPath cfg group
    writeLits cfg ref path group
    return $ toCutPath cfg path
  writePaths cfg ref (fromCutPath cfg out) paths

-- TODO something wrong with the paths/lits here, and it breaks parsing the script??
-- TODO separate haskell fn to just list groups, useful for extracting only one too?
aOrthogroups :: CutConfig -> Locks -> [CutPath] -> Action ()
aOrthogroups cfg ref [out, ofrPath] = do
  resDir <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups <- readOrthogroups resDir
  writeOrthogroups cfg ref out groups
aOrthogroups _ _ args = error $ "bad argument to aOrthogroups: " ++ show args

---------------------------
-- orthogroup_containing --
---------------------------

-- TODO this should only return a str.list

orthogroupContaining :: CutFunction
orthogroupContaining = let name = "orthogroup_containing" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ofr, str] (ListOf str)
  , fTypeCheck = defaultTypeCheck [ofr, str] (ListOf str)
  , fDesc      = Just "Given one gene ID, list others in the same orthogroup."
  , fFixity    = Prefix
  , fRules     = rSimple aOrthogroupContaining
  }

aOrthogroupContaining :: CutConfig -> Locks -> [CutPath] -> Action ()
aOrthogroupContaining cfg ref [out, ofrPath, idPath] = do
  geneId <- readLit cfg ref $ fromCutPath cfg idPath
  resDir <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups <- fmap (filter $ elem geneId) $ readOrthogroups resDir
  let group = if null groups then [] else head groups -- TODO check for more?
  writeLits cfg ref (fromCutPath cfg out) group
aOrthogroupContaining _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args

----------------------------
-- orthogroups_containing --
----------------------------

orthogroupsContaining :: CutFunction
orthogroupsContaining = let name = "orthogroups_containing" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ofr, ListOf str] (ListOf (ListOf str))
  , fTypeCheck = defaultTypeCheck [ofr, ListOf str] (ListOf (ListOf str))
  , fDesc      = Just "Given a list of gene IDs, list the orthogroups that contain them."
  , fFixity    = Prefix
  , fRules     = rSimple aOrthogroupsContaining
  }

-- see https://stackoverflow.com/a/13271723
filterContainsOne :: Eq a => [a] -> [[a]] -> [[a]]
filterContainsOne elems lists = filter (flip any elems . flip elem) lists

aOrthogroupsContaining :: CutConfig -> Locks -> [CutPath] -> Action ()
aOrthogroupsContaining cfg ref [out, ofrPath, idsPath] = do
  geneIds <- readLits cfg ref $ fromCutPath cfg idsPath
  resDir  <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups  <- fmap (filterContainsOne geneIds) $ readOrthogroups resDir
  writeOrthogroups cfg ref out groups
aOrthogroupsContaining _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args
