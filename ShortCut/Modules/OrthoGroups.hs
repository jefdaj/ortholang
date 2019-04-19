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

import ShortCut.Modules.SeqIO         (faa)
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
      , orthologInAll
      , orthologInAny
      ]
  }

-- TODO should there be single and plural versions?
og :: CutType
og = CutTypeGroup
  { tgShort = "og"
  , tgLong = "orthogroups (orthofinder or sonicparanoid results)"
  , tgMember = \t -> t `elem` [ofr, spr]
  }

-----------------
-- orthogroups --
-----------------

-- TODO list_groups?
-- TODO separate module that works with multiple ortholog programs?
-- TODO version to get the group matching an ID
-- TODO this works with ofr files too; put them back using a type group!
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

-- TODO think of a better name for this
orthogroupsContaining :: CutFunction
orthogroupsContaining = let name = "orthogroups_containing" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ofr, ListOf str] (ListOf (ListOf str))
  , fTypeCheck = defaultTypeCheck [ofr, ListOf str] (ListOf (ListOf str))
  , fDesc      = Just "Given a list of gene IDs, list the orthogroups that contain any of them."
  , fFixity    = Prefix
  , fRules     = rSimple $ aOrthogroupsFilter containsOneOf
  }

type FilterFn = [String] -> [[String]] -> [[String]]

-- see https://stackoverflow.com/a/13271723
containsOneOf :: FilterFn
containsOneOf elems lists = filter (flip any elems . flip elem) lists

aOrthogroupsFilter :: FilterFn -> CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aOrthogroupsFilter filterFn cfg ref ids [out, ofrPath, idsPath] = do
  geneIds <- readLits cfg ref $ fromCutPath cfg idsPath
  resDir  <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups  <-  parseOrthoFinder cfg ref ids resDir -- TODO handle sonicparanoid
  let groups' = filterFn geneIds groups
  writeOrthogroups cfg ref ids out groups'
aOrthogroupsFilter _ _ _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args

---------------------
-- ortholog_in_any --
---------------------

-- TODO flip args so it reads more naturally?
orthologInAny :: CutFunction
orthologInAny = let name = "ortholog_in_any" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ofr, ListOf faa] (ListOf (ListOf str))
  , fTypeCheck = defaultTypeCheck [ofr, ListOf faa] (ListOf (ListOf str))
  , fDesc      = Just "Filter a list of orthogroups to keep the ones with an ortholog in every given proteome"
  , fFixity    = Prefix
  , fRules     = rOrthologInAny
  }

-- rMakeblastdbEach :: RulesFn
-- rMakeblastdbEach st@(_, cfg, _, _) (CutFun (ListOf dbType) salt deps name [e]) =
--   -- rFun1 (map1of1 faType dbType act1) st expr'
--   (rMap 1 act1) st expr'
--   where
--     -- faType = typeOf e
--     tmpDir = makeblastdbCache cfg 
--     -- act1 c r o a1 = aMakeblastdbAll dbType c r tmpDir [o, a1]
--     act1 c r i = aMakeblastdbAll dbType c r i tmpDir -- TODO should be i right? not ids?
--     expr' = CutFun (ListOf dbType) salt deps name [withSingletons e]
--     -- expr'' = trace ("expr':" ++ show expr') expr'
-- rMakeblastdbEach _ e = error $ "bad argument to rMakeblastdbEach" ++ show e

rOrthologInAny :: RulesFn
rOrthologInAny = rSimple undefined
-- TODO load groups same as above
-- TODO but instead of a str.list, need to load an faa.list and get str.list.list from it (extract_ids_each?)
--      easiest way is probably to make this call a hidden fn that takes the str.list.list and compose them
-- TODO then come up with the right filter fn
-- TODO and finally save str.list.list same as above

---------------------
-- ortholog_in_all --
---------------------

-- TODO flip args so it reads more naturally?
orthologInAll :: CutFunction
orthologInAll = let name = "ortholog_in_all" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ofr, ListOf faa] (ListOf (ListOf str))
  , fTypeCheck = defaultTypeCheck [ofr, ListOf faa] (ListOf (ListOf str))
  , fDesc      = Just "Filter a list of orthogroups to keep the ones with an ortholog in any given proteome"
  , fFixity    = Prefix
  , fRules     = rOrthologInAll
  }

rOrthologInAll :: RulesFn
rOrthologInAll = undefined
-- TODO basically aOrthologInAny but with the filter fn tweaked. unify them.
