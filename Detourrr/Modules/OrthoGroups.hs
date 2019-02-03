module Detourrr.Modules.OrthoGroups
  where

import Development.Shake
import Detourrr.Core.Types

import Control.Monad               (forM)
import Detourrr.Core.Actions       (readLit, readLits, writeLits, cachedLinesPath, writePaths)
import Detourrr.Core.Compile.Basic (defaultTypeCheck, rSimple)
import Detourrr.Core.Paths         (RrrPath, toRrrPath, fromRrrPath)
import Detourrr.Core.Util          (resolveSymlinks)
import Detourrr.Core.Sanitize      (unhashIDs)
import System.FilePath             ((</>), takeDirectory)
import Data.IORef                  (readIORef)

import Detourrr.Modules.OrthoFinder   (ofr)
import Detourrr.Modules.SonicParanoid (spr)

rrrModule :: RrrModule
rrrModule = RrrModule
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
orthogroups :: RrrFunction
orthogroups = let name = "orthogroups" in RrrFunction
  { fName      = name
  , fTypeDesc  = name ++ " : ofr/spr -> str.list.list"
  , fTypeCheck = defaultTypeCheck [ofr] (ListOf (ListOf str))
  , fDesc      = Just "Parse results from an ortholog finder and list genes in all orthogroups."
  , fFixity    = Prefix
  , fRules     = rSimple aOrthogroups
  }

findResDir :: RrrConfig -> FilePath -> IO FilePath
findResDir cfg outPath = do
  statsPath <- resolveSymlinks (Just $ cfgTmpDir cfg) outPath
  return $ takeDirectory $ takeDirectory statsPath

-- TODO write one of these for sonicparanoid too
parseOrthoFinder :: RrrConfig -> HashedSeqIDsRef -> FilePath -> Action [[String]]
parseOrthoFinder cfg idref resDir = do
  let orthoPath = resDir </> "Orthogroups" </> "Orthogroups.txt"
  ids <- liftIO $ readIORef idref
  txt <- fmap (unhashIDs cfg ids) $ readFile' orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  return groups

writeOrthogroups :: RrrConfig -> Locks -> HashedSeqIDsRef -> RrrPath -> [[String]] -> Action ()
writeOrthogroups cfg ref _ out groups = do
  -- let groups' = (map . map) (unhashIDs cfg ids) groups
  -- ids   <- liftIO $ readIORef idsref
  paths <- forM groups $ \group -> do
    let path = cachedLinesPath cfg group -- TODO should this use group'?
        -- group' = map (unhashIDs cfg ids) group
    -- liftIO $ putStrLn $ "group': " ++ show group'
    writeLits cfg ref path group
    return $ toRrrPath cfg path
  writePaths cfg ref (fromRrrPath cfg out) paths

-- TODO something wrong with the paths/lits here, and it breaks parsing the script??
-- TODO separate haskell fn to just list groups, useful for extracting only one too?
-- TODO translate hashes back into actual seqids here?
aOrthogroups :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aOrthogroups cfg ref idsref [out, ofrPath] = do
  resDir <- liftIO $ findResDir cfg $ fromRrrPath cfg ofrPath
  groups <- parseOrthoFinder cfg idsref resDir
  writeOrthogroups cfg ref idsref out groups
aOrthogroups _ _ _ args = error $ "bad argument to aOrthogroups: " ++ show args

---------------------------
-- orthogroup_containing --
---------------------------

-- TODO this should only return a str.list

orthogroupContaining :: RrrFunction
orthogroupContaining = let name = "orthogroup_containing" in RrrFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ofr, str] (ListOf str)
  , fTypeCheck = defaultTypeCheck [ofr, str] (ListOf str)
  , fDesc      = Just "Given one gene ID, list others in the same orthogroup."
  , fFixity    = Prefix
  , fRules     = rSimple aOrthogroupContaining
  }

aOrthogroupContaining :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aOrthogroupContaining cfg ref ids [out, ofrPath, idPath] = do
  geneId <- readLit cfg ref $ fromRrrPath cfg idPath
  resDir <- liftIO $ findResDir cfg $ fromRrrPath cfg ofrPath
  groups' <- fmap (filter $ elem geneId) $ parseOrthoFinder cfg ids resDir
  let group = if null groups' then [] else head groups' -- TODO check for more?
  writeLits cfg ref (fromRrrPath cfg out) group
aOrthogroupContaining _ _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args

----------------------------
-- orthogroups_containing --
----------------------------

orthogroupsContaining :: RrrFunction
orthogroupsContaining = let name = "orthogroups_containing" in RrrFunction
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

aOrthogroupsContaining :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aOrthogroupsContaining cfg ref ids [out, ofrPath, idsPath] = do
  geneIds <- readLits cfg ref $ fromRrrPath cfg idsPath
  resDir  <- liftIO $ findResDir cfg $ fromRrrPath cfg ofrPath
  groups  <- fmap (filterContainsOne geneIds) $ parseOrthoFinder cfg ids resDir -- TODO handle sonicparanoid
  writeOrthogroups cfg ref ids out groups
aOrthogroupsContaining _ _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args
