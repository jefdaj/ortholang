module ShortCut.Modules.OrthoGroups
  where

import Development.Shake
import ShortCut.Core.Types

import Control.Monad               (forM)
import ShortCut.Core.Actions       (readLit, readLits, writeLits, cachedLinesPath, writePaths)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Util          (resolveSymlinks)
import System.FilePath             ((</>), takeDirectory)

import ShortCut.Modules.OrthoFinder   (ofr)
import ShortCut.Modules.SonicParanoid (spr)

cutModule :: CutModule
cutModule = CutModule
  { mName = "OrthoGroups"
  , mDesc = "Common interface for working with the results of OrthoFinder, SonicParanoid, etc."
  , mTypes = [ofr, spr]
  , mFunctions =
      [ orthogroups
      , orthogroupContaining
      , orthogroupsContaining
      ]
  }

-----------------
-- orthogroups --
-----------------

-- TODO list_groups?
-- TODO separate module that works with multiple ortholog programs?
-- TODO version to get the group matching an ID
orthogroups :: CutFunction
orthogroups = let name = "orthogroups" in CutFunction
  { fName      = name
  , fTypeDesc  = name ++ " : ofr/spr -> str.list.list"
  , fTypeCheck = defaultTypeCheck [ofr] (ListOf (ListOf str))
  , fDesc      = Just "Parse results from an ortholog finder and list genes in all orthogroups."
  , fFixity    = Prefix
  , fRules     = rSimple aOrthogroups
  }

findResDir :: CutConfig -> FilePath -> IO FilePath
findResDir cfg outPath = do
  statsPath <- resolveSymlinks (Just $ cfgTmpDir cfg) outPath
  return $ takeDirectory $ takeDirectory statsPath

-- TODO write one of these for sonicparanoid too
parseOrthoFinder :: FilePath -> Action [[String]]
parseOrthoFinder resDir = do
  let orthoPath = resDir </> "Orthogroups" </> "Orthogroups.txt"
  txt <- readFile' orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  return groups

writeOrthogroups :: CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> [[String]] -> Action ()
writeOrthogroups cfg ref _ out groups = do
  paths <- forM groups $ \group -> do
    let path = cachedLinesPath cfg group
    writeLits cfg ref path group
    return $ toCutPath cfg path
  writePaths cfg ref (fromCutPath cfg out) paths

-- TODO something wrong with the paths/lits here, and it breaks parsing the script??
-- TODO separate haskell fn to just list groups, useful for extracting only one too?
aOrthogroups :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aOrthogroups cfg ref ids [out, ofrPath] = do
  resDir <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups <- parseOrthoFinder resDir
  writeOrthogroups cfg ref ids out groups
aOrthogroups _ _ _ args = error $ "bad argument to aOrthogroups: " ++ show args

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

aOrthogroupContaining :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aOrthogroupContaining cfg ref _ [out, ofrPath, idPath] = do
  geneId <- readLit cfg ref $ fromCutPath cfg idPath
  resDir <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups <- fmap (filter $ elem geneId) $ parseOrthoFinder resDir
  let group = if null groups then [] else head groups -- TODO check for more?
  writeLits cfg ref (fromCutPath cfg out) group
aOrthogroupContaining _ _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args

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

aOrthogroupsContaining :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aOrthogroupsContaining cfg ref ids [out, ofrPath, idsPath] = do
  geneIds <- readLits cfg ref $ fromCutPath cfg idsPath
  resDir  <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups  <- fmap (filterContainsOne geneIds) $ parseOrthoFinder resDir -- TODO handle sonicparanoid
  writeOrthogroups cfg ref ids out groups
aOrthogroupsContaining _ _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args
