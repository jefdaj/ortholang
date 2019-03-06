module ShortCut.Modules.OrthoGroups
  where

-- TODO homologs module that lists homolog pairs
-- TODO and make that homologs function work on orthogroups too if possible

import Development.Shake
import ShortCut.Core.Types

import Control.Monad               (forM)
import ShortCut.Core.Actions       (readLit, readLits, writeLits, cachedLinesPath, writePaths, readFileStrict')
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Util          (resolveSymlinks)
import ShortCut.Core.Sanitize      (unhashIDs)
import System.FilePath             ((</>), takeDirectory)
import Data.IORef                  (readIORef)

import ShortCut.Modules.OrthoFinder   (ofr)
import ShortCut.Modules.SonicParanoid (spr)

cutModule :: CutModule
cutModule = CutModule
  { mName = "OrthoGroups"
  , mDesc = "Common interface for working with the results of OrthoFinder, SonicParanoid, etc."
  , mTypes = [og]
  , mFunctions =
      [ orthogroups
      , orthogroupContaining
      , orthogroupsContaining
      ]
  }

-- TODO should there be single and plural versions?
og :: CutType
og = CutTypeGroup
  { tgExt = "og"
  , tgDesc = "orthogroups (orthofinder or sonicparanoid results)"
  , tgTypes = [ofr, spr]
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
  , fTypeDesc  = name ++ " : og -> str.list.list"
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
parseOrthoFinder :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> Action [[String]]
parseOrthoFinder cfg ref idref resDir = do
  let orthoPath = resDir </> "Orthogroups" </> "Orthogroups.txt"
  ids <- liftIO $ readIORef idref
  txt <- fmap (unhashIDs cfg ids) $ readFileStrict' cfg ref orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  return groups

writeOrthogroups :: CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> [[String]] -> Action ()
writeOrthogroups cfg ref _ out groups = do
  -- let groups' = (map . map) (unhashIDs cfg ids) groups
  -- ids   <- liftIO $ readIORef idsref
  paths <- forM groups $ \group -> do
    let path = cachedLinesPath cfg group -- TODO should this use group'?
        -- group' = map (unhashIDs cfg ids) group
    -- liftIO $ putStrLn $ "group': " ++ show group'
    writeLits cfg ref path group
    return $ toCutPath cfg path
  writePaths cfg ref (fromCutPath cfg out) paths

-- TODO something wrong with the paths/lits here, and it breaks parsing the script??
-- TODO separate haskell fn to just list groups, useful for extracting only one too?
-- TODO translate hashes back into actual seqids here?
aOrthogroups :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aOrthogroups cfg ref idsref [out, ofrPath] = do
  resDir <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups <- parseOrthoFinder cfg ref idsref resDir
  writeOrthogroups cfg ref idsref out groups
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
aOrthogroupContaining cfg ref ids [out, ofrPath, idPath] = do
  geneId <- readLit cfg ref $ fromCutPath cfg idPath
  resDir <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups' <- fmap (filter $ elem geneId) $ parseOrthoFinder cfg ref ids resDir
  let group = if null groups' then [] else head groups' -- TODO check for more?
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
  groups  <- fmap (filterContainsOne geneIds) $ parseOrthoFinder cfg ref ids resDir -- TODO handle sonicparanoid
  writeOrthogroups cfg ref ids out groups
aOrthogroupsContaining _ _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args
