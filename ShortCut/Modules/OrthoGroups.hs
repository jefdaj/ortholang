{-# LANGUAGE FlexibleContexts #-}

-- TODO wow is the list union stuff here a bottleneck? refactor to speed it up

module ShortCut.Modules.OrthoGroups
  where

-- TODO homologs module that lists homolog pairs
-- TODO and make that homologs function work on orthogroups too if possible

-- import Debug.Trace

import Development.Shake
import ShortCut.Core.Types

import Control.Monad               (forM)
import ShortCut.Core.Actions       (readLit, readLits, writeLits, cachedLinesPath, writePaths, readFileStrict',
                                    readPaths)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath, exprPath)
import ShortCut.Core.Util          (resolveSymlinks)
import ShortCut.Core.Sanitize      (unhashIDs)
import System.FilePath             ((</>), takeDirectory)
import Data.IORef                  (readIORef)
import Text.Regex.Posix            ((=~))
import ShortCut.Core.Compile.Basic (rExpr, debugRules)
import Data.List                   (intersect)

import ShortCut.Modules.SeqIO         (faa)
import ShortCut.Modules.OrthoFinder   (ofr)
import ShortCut.Modules.SonicParanoid (spr)

cutModule :: CutModule
cutModule = CutModule
  { mName = "OrthoGroups"
  , mDesc = "Common interface for working with the results of OrthoFinder, SonicParanoid, etc."
  , mTypes = [og] -- TODO ofr, spr too?
  , mFunctions =
      [ orthogroups
      , orthogroupContaining
      , orthogroupsContaining
      , orthologInAny
      , orthologInAnyStr -- TODO hide?
      , orthologInAll
      , orthologInAllStr -- TODO hide?
      ]
  }

-- TODO should there be single and plural versions?
og :: CutType
og = CutTypeGroup
  { tgExt = "og"
  , tgDesc = "orthogroups (orthofinder or sonicparanoid results)"
  , tgMember = \t -> t `elem` [ofr, spr]
  }

-----------------
-- orthogroups --
-----------------

-- for orthofinder, just parse result/Orthogroups/Orthogroups.txt
-- for sonicparanoid?
-- TODO list_groups?
-- TODO separate module that works with multiple ortholog programs?
-- TODO version to get the group matching an ID
-- TODO this works with ofr files too; put them back using a type group!
orthogroups :: CutFunction
orthogroups = let name = "orthogroups" in CutFunction
  { fName      = name
  , fTypeDesc  = name ++ " : og -> str.list.list"
  , fTypeCheck = defaultTypeCheck [og] (ListOf (ListOf str)) -- TODO or ofr!
  , fDesc      = Just "Parse results from an ortholog finder and list genes in all orthogroups."
  , fFixity    = Prefix
  , fRules     = rOrthogroups
  }

rOrthogroups :: RulesFn
rOrthogroups st e@(CutFun _ _ _ _ [arg]) = (rSimple $ aOrthogroups $ typeOf arg) st e
rOrthogroups _ e = error $ "bad argument to rOrthogroups: " ++ show e

findResDir :: CutConfig -> FilePath -> IO FilePath
findResDir cfg outPath = do
  statsPath <- resolveSymlinks (Just $ cfgTmpDir cfg) outPath
  return $ takeDirectory $ takeDirectory statsPath

parseOrthoFinder :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> Action [[String]]
parseOrthoFinder cfg ref idref resDir = do
  let orthoPath = resDir </> "Orthogroups" </> "Orthogroups.txt"
  ids <- liftIO $ readIORef idref
  txt <- fmap (unhashIDs cfg ids) $ readFileStrict' cfg ref orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  return groups

parseSonicParanoid :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> Action [[String]]
parseSonicParanoid cfg ref _ resDir = do
  let orthoPath = resDir </> "multi_species" </> "multispecies_clusters.tsv"
  -- ids <- liftIO $ readIORef idref -- TODO why are we unhashing here again?
  -- txt <- fmap (unhashIDs cfg ids) $ readFileStrict' cfg ref orthoPath -- TODO openFile error during this?
  txt <- readFileStrict' cfg ref orthoPath -- TODO openFile error during this?
  let groups = map parseLine $ tail $ lines txt -- TODO be safe about tail
  -- liftIO $ putStrLn $ "groups: " ++ show groups
      --groups' = map (unhashIDs cfg) groups
  -- let groups = map (words . drop 11) (lines txt)
  return groups
  where
    parseLine l = concat (l =~ "seqid_[a-zA-Z0-9]*?" :: [[String]])

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
aOrthogroups :: CutType -> CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aOrthogroups rtn cfg ref idsref [out, ogPath] = do
  resDir <- liftIO $ findResDir cfg $ fromCutPath cfg ogPath
  let parser = if      rtn == spr then parseSonicParanoid
               else if rtn == ofr then parseOrthoFinder
               else                    error $ "bad type for aOrthogroups: " ++ show rtn
  groups <- parser cfg ref idsref resDir
  writeOrthogroups cfg ref idsref out groups
aOrthogroups _ _ _ _ args = error $ "bad argument to aOrthogroups: " ++ show args

---------------------------
-- orthogroup_containing --
---------------------------

-- TODO this should only return a str.list

orthogroupContaining :: CutFunction
orthogroupContaining = let name = "orthogroup_containing" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [og, str] (ListOf str)
  , fTypeCheck = defaultTypeCheck [og, str] (ListOf str)
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
-- TODO should it start from og instead of ofr/spr?
orthogroupsContaining :: CutFunction
orthogroupsContaining = let name = "orthogroups_containing" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [og, ListOf str] (ListOf (ListOf str))
  , fTypeCheck = defaultTypeCheck [og, ListOf str] (ListOf (ListOf str))
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
  , fTypeDesc  = mkTypeDesc  name [spr, ListOf faa] (ListOf (ListOf str))
  , fTypeCheck = defaultTypeCheck [spr, ListOf faa] (ListOf (ListOf str))
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
rOrthologInAny st (CutFun rType salt deps "ortholog_in_any" [groups, faas]) =
  rExpr st $ CutFun rType salt deps "ortholog_in_any_str" [groups', faas']
  where
    groups' = CutFun sll salt (depsOf groups) "orthogroups"      [groups]
    faas'   = CutFun sll salt (depsOf faas  ) "extract_ids_each" [faas]
rOrthologInAny _ _ = error "bad arguments to rOrthologInAny"

-------------------------
-- ortholog_in_any_str --
-------------------------

sll :: CutType
sll = ListOf (ListOf str)

-- TODO hide this one?

-- TODO flip args so it reads more naturally?
orthologInAnyStr :: CutFunction
orthologInAnyStr = let name = "ortholog_in_any_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [sll, sll] sll
  , fTypeCheck = defaultTypeCheck [sll, sll] sll
  , fDesc      = Just "Version of ortholog_in_any that expects arguments already extracted"
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStr groupMemberInAnyList
  }

type FilterFn2 = [[String]] -> [[String]] -> [[String]]

groupMemberInAnyList :: FilterFn2
groupMemberInAnyList groups ids = filter (memberInAnyList ids) groups
  where
    -- memberInOneList g i = not $ null $ intersect g i
    overlap g i = not $ null $ intersect g i
    memberInAnyList is g = any (overlap g) is
    -- fn g i = let res = overlap g i in trace ("member in one list? " ++ show g ++ " " ++ show i ++ " " ++ show res) res

rOrthologFilterStr :: FilterFn2 -> RulesFn
rOrthologFilterStr filterFn st@(_, cfg, ref, idref) e@(CutFun _ _ _ _ [groupLists, idLists]) = do
  (ExprPath groupListsPath) <- rExpr st groupLists
  (ExprPath idListsPath   ) <- rExpr st idLists
  let out    = exprPath st e
      out'   = debugRules cfg "rOrthologFilterStr" e $ fromCutPath cfg out
  out' %> \_ -> do
    -- hashedIDs  <- liftIO $ readIORef idref
    groupPaths <- readPaths cfg ref groupListsPath -- TODO readLits?
    groups     <- mapM (readLits cfg ref) $ map (fromCutPath cfg) groupPaths
    idPaths    <- readPaths cfg ref idListsPath
    ids        <- mapM (readLits cfg ref) $ map (fromCutPath cfg) idPaths
    let groups' = filterFn groups ids -- TODO add filter here
    writeOrthogroups cfg ref idref out groups'
  return (ExprPath out')
rOrthologFilterStr _ _ _ = error "bad arguments to rOrthologFilterStr"

---------------------
-- ortholog_in_all --
---------------------

groupMemberInAllList :: FilterFn2
groupMemberInAllList groups ids = filter (memberInAllList ids) groups
  where
    overlap g i = not $ null $ intersect g i
    memberInAllList is g = all (overlap g) is
    -- fn g i = let res = overlap g i in trace ("member in one list? " ++ show g ++ " " ++ show i ++ " " ++ show res) res

-- TODO flip args so it reads more naturally?
orthologInAll :: CutFunction
orthologInAll = let name = "ortholog_in_all" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [spr, ListOf faa] (ListOf (ListOf str))
  , fTypeCheck = defaultTypeCheck [spr, ListOf faa] (ListOf (ListOf str))
  , fDesc      = Just "Filter a list of orthogroups to keep the ones with an ortholog in any given proteome"
  , fFixity    = Prefix
  , fRules     = rOrthologInAll
  }

rOrthologInAll :: RulesFn
rOrthologInAll st (CutFun rType salt deps "ortholog_in_all" [groups, faas]) =
  rExpr st $ CutFun rType salt deps "ortholog_in_all_str" [groups', faas']
  where
    groups' = CutFun sll salt (depsOf groups) "orthogroups"      [groups]
    faas'   = CutFun sll salt (depsOf faas  ) "extract_ids_each" [faas]
rOrthologInAll _ _ = error "bad arguments to rOrthologInAll"

---------------------
-- ortholog_in_all --
---------------------

orthologInAllStr :: CutFunction
orthologInAllStr = let name = "ortholog_in_all_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [sll, sll] sll
  , fTypeCheck = defaultTypeCheck [sll, sll] sll
  , fDesc      = Just "Version of ortholog_in_all that expects arguments already extracted"
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStr groupMemberInAllList
  }
