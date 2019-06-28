{-# LANGUAGE FlexibleContexts #-}

-- TODO wow is the list union stuff here a bottleneck? refactor to speed it up
-- TODO shit, it's measuring how many orthogroups an is in rather than how many fastas
-- TODO think through the overall process here...
--      do we want to get all orthologs from widespread groups? or just from a reference species?

module ShortCut.Modules.OrthoGroups
  where

-- TODO homologs module that lists homolog pairs
-- TODO and make that homologs function work on orthogroups too if possible

-- import Debug.Trace

import Development.Shake
import ShortCut.Core.Types

import Control.Monad               (forM)
import ShortCut.Core.Actions       (readLit, readLits, writeLits, cachedLinesPath,
                                    writePaths, readFileStrict', readPaths)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath, exprPath, upBy)
import ShortCut.Core.Util          (resolveSymlinks)
import ShortCut.Core.Sanitize      (unhashIDs)
import System.FilePath             ((</>), takeDirectory)
import Data.IORef                  (readIORef)
import Text.Regex.Posix            ((=~))
import ShortCut.Core.Compile.Basic (rExpr, debugRules)
import Data.List                   (intersect)
import Data.Scientific             (Scientific, toRealFloat)

import ShortCut.Modules.SeqIO         (faa)
import ShortCut.Modules.OrthoFinder   (ofr)
import ShortCut.Modules.SonicParanoid (spr)

cutModule :: CutModule
cutModule = CutModule
  { mName = "OrthoGroups"
  , mDesc = "Common interface for working with the results of OrthoFinder, SonicParanoid, etc."
  , mTypes = [og, ofr, spr]
  , mFunctions =
      [ orthogroups
      , orthogroupContaining
      , orthogroupsContaining
      , orthologInAny
      , orthologInAll
      , orthologInMin
      -- , orthologInMax
      , orthologInAnyStr     -- TODO hide? rename to show generality?
      , orthologInAllStr     -- TODO hide? rename to show generality?
      , orthologInMinStr -- TODO hide? rename to show generality?
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
  , fFixity    = Prefix
  , fRules     = rOrthogroups
  }

rOrthogroups :: RulesFn
rOrthogroups st e@(CutFun _ _ _ _ [arg]) = (rSimple $ aOrthogroups $ typeOf arg) st e
rOrthogroups _ e = error $ "bad argument to rOrthogroups: " ++ show e

-- TODO any reason to keep this?
findResDir :: CutConfig -> FilePath -> IO FilePath
findResDir cfg outPath = do
  statsPath <- resolveSymlinks (Just $ cfgTmpDir cfg) outPath
  -- liftIO $ putStrLn $ "outPath: " ++ outPath
  -- liftIO $ putStrLn $ "statsPath: " ++ statsPath
  return $ takeDirectory $ takeDirectory statsPath

-- TODO move parse fns to their respective modules for easier maintenance

parseOrthoFinder :: CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> Action [[String]]
parseOrthoFinder cfg ref idref ofrPath = do
  let resDir = fromCutPath cfg $ upBy 2 ofrPath
      orthoPath = resDir </> "Orthogroups" </> "Orthogroups.txt"
  ids <- liftIO $ readIORef idref
  txt <- fmap (unhashIDs cfg ids) $ readFileStrict' cfg ref orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  return groups

parseSonicParanoid :: CutConfig -> Locks -> HashedSeqIDsRef -> CutPath -> Action [[String]]
parseSonicParanoid cfg ref _ ogPath = do
  let resDir = takeDirectory $ fromCutPath cfg ogPath
      grpPath = resDir </> "ortholog_groups.tsv"
  -- ids <- liftIO $ readIORef idref -- TODO why are we unhashing here again?
  -- txt <- fmap (unhashIDs cfg ids) $ readFileStrict' cfg ref grpPath -- TODO openFile error during this?
  txt <- readFileStrict' cfg ref grpPath -- TODO openFile error during this?
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
  -- liftIO $ putStrLn $ "ogPath: " ++ show ogPath
  -- resDir <- liftIO $ findResDir cfg $ fromCutPath cfg ogPath
  let parser = if      rtn == spr then parseSonicParanoid
               else if rtn == ofr then parseOrthoFinder
               else                    error $ "bad type for aOrthogroups: " ++ show rtn
  groups <- parser cfg ref idsref ogPath
  writeOrthogroups cfg ref idsref out groups
aOrthogroups _ _ _ _ args = error $ "bad argument to aOrthogroups: " ++ show args

---------------------------
-- orthogroup_containing --
---------------------------

orthogroupContaining :: CutFunction
orthogroupContaining = let name = "orthogroup_containing" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [og, str] (ListOf str)
  , fTypeCheck = defaultTypeCheck [og, str] (ListOf str)
  , fFixity    = Prefix
  , fRules     = rSimple aOrthogroupContaining
  }

aOrthogroupContaining :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aOrthogroupContaining cfg ref ids [out, ofrPath, idPath] = do
  geneId <- readLit cfg ref $ fromCutPath cfg idPath
  -- resDir <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups' <- fmap (filter $ elem geneId) $ parseOrthoFinder cfg ref ids ofrPath
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
  , fTypeDesc  = mkTypeDesc  name [og, ListOf str] sll
  , fTypeCheck = defaultTypeCheck [og, ListOf str] sll
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
  -- resDir  <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups  <-  parseOrthoFinder cfg ref ids ofrPath -- TODO handle sonicparanoid
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
  , fTypeDesc  = mkTypeDesc  name [spr, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck [spr, ListOf faa] sll
  , fFixity    = Prefix
  , fRules     = mkOrthologsStrRules "ortholog_in_any"
  }

mkOrthologsStrRules :: String -> RulesFn
mkOrthologsStrRules name st (CutFun rType salt deps _  [groups , faas]) =
  rExpr st $ CutFun rType salt deps (name ++ "_str")   [groups', faas']
  where
    groups' = CutFun sll salt (depsOf groups) "orthogroups"      [groups]
    faas'   = CutFun sll salt (depsOf faas  ) "extract_ids_each" [faas]
mkOrthologsStrRules _ _ _ = error "bad arguments to mkOrthologsStrRules"

-- TODO can this be removed somehow?
-- TODO flip args so it reads more naturally?

sll :: CutType
sll = ListOf (ListOf str)

orthologInAnyStr :: CutFunction
orthologInAnyStr = let name = "ortholog_in_any_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [sll, sll] sll
  , fTypeCheck = defaultTypeCheck [sll, sll] sll
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStr groupMemberInAnyList
  }

type FilterFn2 = [[String]] -> [[String]] -> [[String]]

groupMemberInAnyList :: FilterFn2
groupMemberInAnyList groups ids = filter (memberInAnyList ids) groups
  where
    overlap :: [String] -> [String] -> Bool
    overlap g i = not $ null $ intersect g i
    memberInAnyList :: [[String]] -> [String] -> Bool
    memberInAnyList is g = any (overlap g) is

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

-- TODO flip args so it reads more naturally?

groupMemberInAllList :: FilterFn2
groupMemberInAllList groups ids = filter (memberInAllList ids) groups
  where
    overlap g i = not $ null $ intersect g i
    memberInAllList is g = all (overlap g) is

orthologInAll :: CutFunction
orthologInAll = let name = "ortholog_in_all" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [spr, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck [spr, ListOf faa] sll
  , fFixity    = Prefix
  , fRules     = mkOrthologsStrRules "ortholog_in_all"
  }

orthologInAllStr :: CutFunction
orthologInAllStr = let name = "ortholog_in_all_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [sll, sll] sll
  , fTypeCheck = defaultTypeCheck [sll, sll] sll
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStr groupMemberInAllList
  }

--------------------------
-- ortholog_in_min --
--------------------------

pickMin :: (RealFrac a1, Integral a2) => a1 -> a2 -> a2
pickMin userNum nGroups
  | userNum == 0 = 0
  | userNum > -1 && userNum < 1 = pickMin (userNum * fromIntegral nGroups) nGroups
  | userNum < 0 = pickMin (fromIntegral nGroups + userNum) nGroups
  | otherwise = ceiling userNum

groupMemberInMin :: Eq a => Scientific -> [[a]] -> [[a]] -> [[a]]
groupMemberInMin n groups ids = filter (inEnoughLists groups) ids
  where
    n' = pickMin (toRealFloat n :: Double) (length groups)
    overlap gs is = not $ null $ intersect gs is
    inEnoughLists gss is = fromIntegral (length $ filter (overlap is) gss) >= n'

-- TODO can it be derived from pickMin instead?
-- pickMax :: (RealFrac a1, Integral a2) => a1 -> a2 -> a2
-- pickMax userNum nGroups
--   | userNum == 0 = 0
--   | userNum > -1 && userNum < 1 = pickMax (userNum * fromIntegral nGroups) nGroups
--   | userNum < 0 = pickMax (fromIntegral nGroups + userNum) nGroups
--   | otherwise = floor userNum

-- TODO can it be derived from groupMemberInMin instead?
-- groupMemberInMax :: Eq a => Scientific -> [[a]] -> [[a]] -> [[a]]
-- groupMemberInMax n groups ids = filter (notTooManyLists groups) ids
--   where
--     n' = pickMax (toRealFloat n :: Double) (length groups)
--     overlap gs is = not $ null $ intersect gs is
--     notTooManyLists gss is = fromIntegral (length $ filter (overlap is) gss) <= n'

orthologInMinStr :: CutFunction
orthologInMinStr = let name = "ortholog_in_min_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [num, sll, sll] sll
  , fTypeCheck = defaultTypeCheck [num, sll, sll] sll
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStrFrac groupMemberInMin
  }

mkOrthologsStrFracRules :: String -> RulesFn
mkOrthologsStrFracRules name st (CutFun rType salt deps _ [frac, groups , faas]) =
  rExpr st $ CutFun rType salt deps (name ++ "_str")  [frac, groups', faas']
  where
    groups' = CutFun sll salt (depsOf groups) "orthogroups"      [groups]
    faas'   = CutFun sll salt (depsOf faas  ) "extract_ids_each" [faas]
mkOrthologsStrFracRules _ _ _ = error "bad arguments to mkOrthologStrFilter"

rOrthologFilterStrFrac :: (Scientific -> FilterFn2) -> RulesFn
rOrthologFilterStrFrac filterFn st@(_, cfg, ref, idref) e@(CutFun _ _ _ _ [frac, groupLists, idLists]) = do
  (ExprPath fracPath      ) <- rExpr st frac
  (ExprPath groupListsPath) <- rExpr st groupLists
  (ExprPath idListsPath   ) <- rExpr st idLists
  let out    = exprPath st e
      out'   = debugRules cfg "rOrthologFilterStr" e $ fromCutPath cfg out
  out' %> \_ -> do
    f          <- readLit cfg ref fracPath
    groupPaths <- readPaths cfg ref groupListsPath -- TODO readLits?
    groups     <- mapM (readLits cfg ref) $ map (fromCutPath cfg) groupPaths
    idPaths    <- readPaths cfg ref idListsPath
    ids        <- mapM (readLits cfg ref) $ map (fromCutPath cfg) idPaths
    let groups' = filterFn (read f :: Scientific) groups ids
    writeOrthogroups cfg ref idref out groups'
  return (ExprPath out')
rOrthologFilterStrFrac _ _ _ = error "bad arguments to rOrthologFilterStrFrac"

orthologInMin :: CutFunction
orthologInMin = let name = "ortholog_in_min" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [num, spr, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck [num, spr, ListOf faa] sll
  , fFixity    = Prefix
  , fRules     = mkOrthologsStrFracRules "ortholog_in_min"
  }
