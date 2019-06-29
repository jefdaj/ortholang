{-# LANGUAGE FlexibleContexts #-}

-- TODO rewrite only reading the one orthogroups file each time for efficiency
-- TODO add extract_seqs_all to get the sequences for an orthogroup... and have an _each version of that?

-- TODO is the auto-extracting IDs part weird? maybe make that be manual. or, ask ppl
-- TODO wow is the list union stuff here a bottleneck? refactor to speed it up
-- TODO homologs module that lists homolog pairs
-- TODO and make that homologs function work on orthogroups too if possible

module ShortCut.Modules.OrthoGroups
  where

import Development.Shake
import ShortCut.Core.Types

import Control.Monad               (forM)
import ShortCut.Core.Actions       (readLit, readLits, writeLits, cachedLinesPath,
                                    writePaths, readFileStrict', readPaths)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath, exprPath, upBy)
import ShortCut.Core.Util          (resolveSymlinks, headOrDie)
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

-- this is just shorthand
sll :: CutType
sll = ListOf (ListOf str)

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
      , orthologInMax
      , orthologInAnyStr -- TODO hide? rename to show generality?
      , orthologInAllStr -- TODO hide? rename to show generality?
      , orthologInMinStr -- TODO hide? rename to show generality?
      , orthologInMaxStr -- TODO hide? rename to show generality?
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
  , fTypeDesc  = mkTypeDesc  name [og] sll
  , fTypeCheck = defaultTypeCheck [og] sll
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
  let group = if null groups' then [] else headOrDie "aOrthogroupContaining failed" groups' -- TODO check for more?
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

type FilterFn = [[String]] -> [String] -> [[String]]

-- see https://stackoverflow.com/a/13271723
containsOneOf :: FilterFn
containsOneOf lists elems = filter (flip any elems . flip elem) lists

aOrthogroupsFilter :: FilterFn -> CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aOrthogroupsFilter filterFn cfg ref ids [out, ofrPath, idsPath] = do
  geneIds <- readLits cfg ref $ fromCutPath cfg idsPath
  -- resDir  <- liftIO $ findResDir cfg $ fromCutPath cfg ofrPath
  groups  <-  parseOrthoFinder cfg ref ids ofrPath -- TODO handle sonicparanoid
  let groups' = filterFn groups geneIds
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
  , fRules     = mkOrthologsStrRules name
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
groupMemberInAnyList groups idss = filter memberInAnyList groups
  where
    memberInAnyList :: [String] -> Bool
    memberInAnyList g = any (\ids -> not $ null $ intersect g ids) idss

-- TODO parse orthogroups here for efficiency (prevent thousands of extra lists)
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
    idss       <- mapM (readLits cfg ref) $ map (fromCutPath cfg) idPaths
    let groups' = filterFn groups idss
    writeOrthogroups cfg ref idref out groups'
  return (ExprPath out')
rOrthologFilterStr _ _ _ = error "bad arguments to rOrthologFilterStr"

---------------------
-- ortholog_in_all --
---------------------

-- TODO flip args so it reads more naturally?

groupMemberInAllList :: FilterFn2
groupMemberInAllList groups idss = filter oneInAllLists groups
  where
    oneInAllLists g = idss == containsOneOf idss g -- TODO reformat with `all`?

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

---------------------
-- ortholog_in_min --
---------------------

pickMin :: (RealFrac a, Integral b) => a -> b -> b
pickMin userNum nGroups
  | userNum == 0 = 0
  | userNum > -1 && userNum < 1 = ceiling (userNum * fromIntegral nGroups)
  | userNum < 0 - fromIntegral nGroups = 0
  | userNum < 0 = pickMin (fromIntegral nGroups + userNum) nGroups
  | otherwise = ceiling userNum -- TODO floor?

groupMemberInMin :: Scientific -> [[String]] -> [[String]] -> [[String]]
groupMemberInMin n groups idss = filter inEnoughLists groups
  where
    n' = pickMin (toRealFloat n :: Double) (length idss)
    inEnoughLists g = n' <= length (containsOneOf idss g)
    --inEnoughLists g = n' <= let res = length (containsOneOf idss g)
    --                        in trace ("n': " ++ show n' ++ " res: " ++ show res) res

orthologInMinStr :: CutFunction
orthologInMinStr = let name = "ortholog_in_min_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [num, sll, sll] sll
  , fTypeCheck = defaultTypeCheck [num, sll, sll] sll
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStrFrac groupMemberInMin
  }

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
    idss       <- mapM (readLits cfg ref) $ map (fromCutPath cfg) idPaths
    let groups' = filterFn (read f :: Scientific) groups idss
    writeOrthogroups cfg ref idref out groups'
  return (ExprPath out')
rOrthologFilterStrFrac _ _ _ = error "bad arguments to rOrthologFilterStrFrac"

orthologInMin :: CutFunction
orthologInMin = let name = "ortholog_in_min" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [num, spr, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck [num, spr, ListOf faa] sll
  , fFixity    = Prefix
  , fRules     = mkOrthologsStrFracRules name
  }

mkOrthologsStrFracRules :: String -> RulesFn
mkOrthologsStrFracRules name st (CutFun rType salt deps _ [frac, groups , faas]) =
  rExpr st $ CutFun rType salt deps (name ++ "_str")  [frac, groups', faas']
  where
    groups' = CutFun sll salt (depsOf groups) "orthogroups"      [groups]
    faas'   = CutFun sll salt (depsOf faas  ) "extract_ids_each" [faas]
mkOrthologsStrFracRules _ _ _ = error "bad arguments to mkOrthologStrFracRules"

---------------------
-- ortholog_in_max --
---------------------

orthologInMax :: CutFunction
orthologInMax = let name = "ortholog_in_max" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [num, spr, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck [num, spr, ListOf faa] sll
  , fFixity    = Prefix
  , fRules     = mkOrthologsStrFracRules name
  }

orthologInMaxStr :: CutFunction
orthologInMaxStr = let name = "ortholog_in_max_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [num, sll, sll] sll
  , fTypeCheck = defaultTypeCheck [num, sll, sll] sll
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStrFrac groupMemberInMax
  }

-- TODO can it be derived from pickMax instead?
pickMax :: (RealFrac a, Integral b) => a -> b -> b
pickMax userNum nGroups
  | userNum == 0 = 0
  | userNum > -1 && userNum < 1 = floor (userNum * fromIntegral nGroups)
  | userNum > fromIntegral nGroups = nGroups
  | userNum < 0 = pickMax (fromIntegral nGroups + userNum) nGroups
  | otherwise = floor userNum

-- TODO can it be derived from groupMemberInMax instead?
groupMemberInMax :: Scientific -> [[String]] -> [[String]] -> [[String]]
groupMemberInMax n groups idss = filter notInTooManyLists groups
  where
    n' = pickMax (toRealFloat n :: Double) (length idss)
    notInTooManyLists g = n' >= length (containsOneOf idss g)
    -- notInTooManyLists g = n' >= let res = length (containsOneOf idss g)
    --                             in trace ("n': " ++ show n' ++ " res: " ++ show res) res
