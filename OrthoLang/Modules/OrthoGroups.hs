{-# LANGUAGE FlexibleContexts #-}

-- TODO rewrite only reading the one orthogroups file each time for efficiency
-- TODO add extract_seqs_all to get the sequences for an orthogroup... and have an _each version of that?

-- TODO is the auto-extracting IDs part weird? maybe make that be manual. or, ask ppl
-- TODO wow is the list union stuff here a bottleneck? refactor to speed it up
-- TODO homologs module that lists homolog pairs
-- TODO and make that homologs function work on orthogroups too if possible

module OrthoLang.Modules.OrthoGroups
  where

import Development.Shake
import OrthoLang.Core

import OrthoLang.Modules.SeqIO         (faa)
import OrthoLang.Modules.OrthoFinder   (ofr)
import OrthoLang.Modules.SonicParanoid (spr)
import OrthoLang.Modules.GreenCut      (gcr)

import Control.Monad     (forM, when)
import Data.IORef        (readIORef)
import Data.Maybe        (isJust, catMaybes)
import Data.Scientific   (toRealFloat)
import Data.String.Utils (split)
import System.Directory  (createDirectoryIfMissing)
import System.Exit       (ExitCode(..))
import System.FilePath   ((<.>), (</>), takeDirectory)
import Text.Regex.Posix  ((=~))

-- this is just shorthand
sll :: Type
sll = ListOf (ListOf str)

type PickerFn = Int -> Int
type PickerFn2 = Double -> Int -> Int

orthoLangModule :: Module
orthoLangModule = Module
  { mName = "OrthoGroups"
  , mDesc = "Common interface for working with the results of OrthoFinder, SonicParanoid, etc."
  , mTypes = [og, ofr, spr, gcr]
  , mFunctions =
      [ orthogroups
      , orthogroupContaining
      , orthogroupsContaining
      -- these four are meant to be user-facing
      , orthologInAny
      , orthologInAll
      , orthologInMin
      , orthologInMax
      -- and these four implement them (TODO hide?)
      , orthologInAnyStr
      , orthologInAllStr
      , orthologInMinStr
      , orthologInMaxStr
      ]
  }

-- TODO should there be single and plural versions?
og :: Type
og = TypeGroup
  { tgExt = "og"
  , tgDesc = "orthogroups (orthofinder, sonicparanoid, or greencut results)"
  , tgMember = \t -> t `elem` [ofr, spr, gcr]
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
orthogroups :: Function
orthogroups = let name = "orthogroups" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [og] sll
  , fTypeCheck = defaultTypeCheck name [og] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rOrthogroups
  }

rOrthogroups :: RulesFn
rOrthogroups st e@(Fun _ _ _ _ [arg]) = (rSimple $ aOrthogroups $ typeOf arg) st e
rOrthogroups _ e = error $ "bad argument to rOrthogroups: " ++ show e

-- TODO move parse fns to their respective modules for easier maintenance

-- TODO why are we unhashing the ids here?
parseOrthoFinder :: Config -> LocksRef -> IDsRef -> Path -> Action [[String]]
parseOrthoFinder cfg ref _ ofrPath = do
  let resDir = fromPath cfg $ upBy 2 ofrPath
      orthoPath = resDir </> "Orthogroups" </> "Orthogroups.txt"
  -- ids <- liftIO $ readIORef idref
  -- txt <- fmap (unhashIDs False ids) $ readFileStrict' cfg ref orthoPath -- TODO openFile error during this?
  txt <- readFileStrict' cfg ref orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  return groups

parseSonicParanoid :: Config -> LocksRef -> IDsRef -> Path -> Action [[String]]
parseSonicParanoid cfg ref _ ogPath = do
  let resDir = takeDirectory $ fromPath cfg ogPath
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

parseGreenCut :: Config -> LocksRef -> IDsRef -> Path -> Action [[String]]
parseGreenCut cfg ref _ ogPath = do
  txt <- readFileStrict' cfg ref $ fromPath cfg ogPath
  let groups = map parseLine $ lines txt
  return groups
  where
    parseLine l = filter (/= ":") $ split "\t" l

writeOrthogroups :: Config -> LocksRef -> IDsRef -> Path -> [[String]] -> Action ()
writeOrthogroups cfg ref _ out groups = do
  -- let groups' = (map . map) (unhashIDs cfg ids) groups
  -- ids   <- liftIO $ readIORef idsref
  paths <- forM groups $ \group -> do
    let path = cachedLinesPath cfg group -- TODO should this use group'?
        -- group' = map (unhashIDs cfg ids) group
    -- liftIO $ putStrLn $ "group': " ++ show group'
    writeLits cfg ref path group
    return $ toPath cfg path
  writePaths cfg ref (fromPath cfg out) paths

-- TODO something wrong with the paths/lits here, and it breaks parsing the script??
-- TODO separate haskell fn to just list groups, useful for extracting only one too?
-- TODO translate hashes back into actual seqids here?
aOrthogroups :: Type -> Config -> LocksRef -> IDsRef -> [Path] -> Action ()
aOrthogroups rtn cfg ref idsref [out, ogPath] = do
  -- liftIO $ putStrLn $ "ogPath: " ++ show ogPath
  -- TODO extract this into a parseOrthogroups function
  let parser = if      rtn == spr then parseSonicParanoid
               else if rtn == ofr then parseOrthoFinder
               else if rtn == gcr then parseGreenCut
               else error $ "bad type for aOrthogroups: " ++ show rtn
  groups <- parser cfg ref idsref ogPath
  writeOrthogroups cfg ref idsref out groups
aOrthogroups _ _ _ _ args = error $ "bad argument to aOrthogroups: " ++ show args

---------------------------
-- orthogroup_containing --
---------------------------

orthogroupContaining :: Function
orthogroupContaining = let name = "orthogroup_containing" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [og, str] (ListOf str)
  , fTypeCheck = defaultTypeCheck name [og, str] (ListOf str)
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple aOrthogroupContaining
  }

aOrthogroupContaining :: Config -> LocksRef -> IDsRef -> [Path] -> Action ()
aOrthogroupContaining cfg ref ids [out, ofrPath, idPath] = do
  ids' <- liftIO $ readIORef ids
  partialID <- readLit cfg ref $ fromPath cfg idPath
  -- TODO should there be a separate case for multiple matches?
  let geneId = case lookupID ids' partialID of
                 Just k -> k
                 Nothing -> error $ "ERROR: id \"" ++ partialID ++ "' not found"
  groups' <- fmap (filter $ elem geneId) $ parseOrthoFinder cfg ref ids ofrPath -- TODO handle the others!
  let group = if null groups' then [] else headOrDie "aOrthogroupContaining failed" groups' -- TODO check for more?
  writeLits cfg ref (fromPath cfg out) group
aOrthogroupContaining _ _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args

----------------------------
-- orthogroups_containing --
----------------------------

-- TODO think of a better name for this
orthogroupsContaining :: Function
orthogroupsContaining = let name = "orthogroups_containing" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [og, ListOf str] sll
  , fTypeCheck = defaultTypeCheck name [og, ListOf str] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rSimple $ aOrthogroupsFilter containsOneOf
  }

type FilterFn = [[String]] -> [String] -> [[String]]

-- see https://stackoverflow.com/a/13271723
containsOneOf :: FilterFn
containsOneOf lists elems = filter (flip any elems . flip elem) lists

-- TODO should this error when not finding one too, like aOrthogroupContaining?
aOrthogroupsFilter :: FilterFn -> Config -> LocksRef -> IDsRef -> [Path] -> Action ()
aOrthogroupsFilter filterFn cfg ref ids [out, ofrPath, idsPath] = do
  ids' <- liftIO $ readIORef ids
  lookups <- fmap (map $ lookupID ids') $ readLits cfg ref $ fromPath cfg idsPath
  when (not $ all isJust lookups) $ error "unable to find some seqids! probably a programming error"
  let geneIds = catMaybes lookups
  groups <- parseOrthoFinder cfg ref ids ofrPath -- TODO handle the others!
  let groups' = filterFn groups geneIds
  writeOrthogroups cfg ref ids out groups'
aOrthogroupsFilter _ _ _ _ args = error $ "bad argument to aOrthogroupContaining: " ++ show args

---------------------
-- ortholog_in_any --
---------------------

-- TODO flip args so it reads more naturally?
orthologInAny :: Function
orthologInAny = let name = "ortholog_in_any" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [og, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck name [og, ListOf faa] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = mkOrthologsStrRules name
  }

mkOrthologsStrRules :: String -> RulesFn
mkOrthologsStrRules name st (Fun rType salt deps _  [groups , faas]) =
  rExpr st $ Fun rType salt deps (name ++ "_str")   [groups', faas']
  where
    groups' = Fun sll salt (depsOf groups) "orthogroups"      [groups]
    faas'   = Fun sll salt (depsOf faas  ) "extract_ids_each" [faas]
mkOrthologsStrRules _ _ _ = error "bad arguments to mkOrthologsStrRules"

-- TODO can this be removed somehow?
-- TODO flip args so it reads more naturally?

orthologInAnyStr :: Function
orthologInAnyStr = let name = "ortholog_in_any_str" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [sll, sll] sll
  , fTypeCheck = defaultTypeCheck name [sll, sll] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rOrthologFilterStr "min" pickAny
  }

pickAny :: Int -> Int
pickAny _ = 1

rOrthologFilterStr :: String -> PickerFn -> RulesFn
rOrthologFilterStr fnName pickerFn st@(scr, cfg, ref, _) e@(Fun _ _ _ _ [groupLists, idLists]) = do
  (ExprPath groupListsPath) <- rExpr st groupLists
  (ExprPath idListsPath   ) <- rExpr st idLists
  let out     = exprPath cfg scr e
      out'    = debugRules cfg "rOrthologFilterStr" e $ fromPath cfg out
      ogDir   = fromPath cfg $ ogCache cfg
      ogsPath = ogDir </> digest groupListsPath <.> "txt"
      idsPath = ogDir </> digest idListsPath    <.> "txt"
  liftIO $ createDirectoryIfMissing True ogDir
  ogsPath %> absolutizePaths cfg ref groupListsPath
  idsPath %> absolutizePaths cfg ref idListsPath
  out' %> \_ -> do
    -- TODO is there a way to avoid reading this?
    nIDs  <- fmap length $ readPaths cfg ref idListsPath
    let fnArg' = show $ pickerFn nIDs
    need' cfg ref "ortholang.modules.orthogroups.rOrthologFilterStr" [ogsPath, idsPath] -- TODO shouldn't cmdInPatterns pick that up?
    runCmd cfg ref $ CmdDesc
      { cmdParallel = False
      , cmdFixEmpties = True
      , cmdOutPath = out'
      , cmdInPatterns = [ogsPath, idsPath]
      , cmdExtraOutPaths = []
      , cmdSanitizePaths = [out'] -- TODO is this right?
      , cmdOptions =[]
      , cmdBinary = "orthogroups.py"
      , cmdArguments = [out', fnName, fnArg', ogsPath, idsPath]
      , cmdRmPatterns = []
      , cmdExitCode = ExitSuccess
      }
  return (ExprPath out')
rOrthologFilterStr _ _ _ _ = error "bad arguments to rOrthologFilterStr"

---------------------
-- ortholog_in_all --
---------------------

-- TODO flip args so it reads more naturally?

orthologInAll :: Function
orthologInAll = let name = "ortholog_in_all" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [og, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck name [og, ListOf faa] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = mkOrthologsStrRules "ortholog_in_all"
  }

orthologInAllStr :: Function
orthologInAllStr = let name = "ortholog_in_all_str" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [sll, sll] sll
  , fTypeCheck = defaultTypeCheck name [sll, sll] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rOrthologFilterStr "min" pickAll
  }

pickAll :: Int -> Int
pickAll nIDs = nIDs

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

orthologInMinStr :: Function
orthologInMinStr = let name = "ortholog_in_min_str" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [num, sll, sll] sll
  , fTypeCheck = defaultTypeCheck name [num, sll, sll] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rOrthologFilterStrFrac "min" pickMin
  }

-- read a scientific and print again as a string
-- toDecimal :: String -> String
-- toDecimal s = formatScientific Fixed Nothing (read s)

ogCache :: Config -> Path
ogCache cfg = cacheDir cfg "orthogroups"

rOrthologFilterStrFrac :: String -> PickerFn2 -> RulesFn
rOrthologFilterStrFrac fnName pickerFn st@(scr, cfg, ref, _) e@(Fun _ _ _ _ [frac, groupLists, idLists]) = do
  (ExprPath fracPath      ) <- rExpr st frac
  (ExprPath groupListsPath) <- rExpr st groupLists
  (ExprPath idListsPath   ) <- rExpr st idLists
  let out     = exprPath cfg scr e
      out'    = debugRules cfg "rOrthologFilterStr" e $ fromPath cfg out
      ogDir   = fromPath cfg $ ogCache cfg
      ogsPath = ogDir </> digest groupListsPath <.> "txt"
      idsPath = ogDir </> digest idListsPath    <.> "txt"
  liftIO $ createDirectoryIfMissing True ogDir
  ogsPath %> absolutizePaths cfg ref groupListsPath
  idsPath %> absolutizePaths cfg ref idListsPath
  out' %> \_ -> do
    -- TODO is there a way to avoid reading this?
    nIDs  <- fmap length $ readPaths cfg ref idListsPath
    fnArg <- readLit cfg ref fracPath
    let fnArg' = show $ pickerFn (toRealFloat (read fnArg) :: Double) nIDs
    need' cfg ref "ortholang.modules.orthogroups.rOrthologFilterStrFrac" [ogsPath, idsPath] -- TODO shouldn't cmdInPatterns pick that up?
    runCmd cfg ref $ CmdDesc
      { cmdParallel = False
      , cmdFixEmpties = True
      , cmdOutPath = out'
      , cmdInPatterns = [ogsPath, idsPath]
      , cmdExtraOutPaths = []
      , cmdSanitizePaths = [out'] -- TODO is this right?
      , cmdOptions =[]
      , cmdBinary = "orthogroups.py"
      , cmdArguments = [out', fnName, fnArg', ogsPath, idsPath]
      , cmdRmPatterns = []
      , cmdExitCode = ExitSuccess
      }
  return (ExprPath out')
rOrthologFilterStrFrac _ _ _ _ = error "bad arguments to rOrthologFilterStrFrac"

orthologInMin :: Function
orthologInMin = let name = "ortholog_in_min" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [num, og, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck name [num, og, ListOf faa] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = mkOrthologsStrFracRules name
  }

mkOrthologsStrFracRules :: String -> RulesFn
mkOrthologsStrFracRules name st (Fun rType salt deps _ [frac, groups , faas]) =
  rExpr st $ Fun rType salt deps (name ++ "_str")  [frac, groups', faas']
  where
    groups' = Fun sll salt (depsOf groups) "orthogroups"      [groups]
    faas'   = Fun sll salt (depsOf faas  ) "extract_ids_each" [faas]
mkOrthologsStrFracRules _ _ _ = error "bad arguments to mkOrthologStrFracRules"

---------------------
-- ortholog_in_max --
---------------------

orthologInMax :: Function
orthologInMax = let name = "ortholog_in_max" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [num, og, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck name [num, og, ListOf faa] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = mkOrthologsStrFracRules name
  }

orthologInMaxStr :: Function
orthologInMaxStr = let name = "ortholog_in_max_str" in Function
  { fOpChar = Nothing, fName = name
  , fTypeDesc  = mkTypeDesc  name [num, sll, sll] sll
  , fTypeCheck = defaultTypeCheck name [num, sll, sll] sll
  ,fTags = []
  , fNewRules = Nothing, fOldRules = rOrthologFilterStrFrac "max" pickMax
  }

pickMax :: (RealFrac a, Integral b) => a -> b -> b
pickMax userNum nGroups
  | userNum == 0 = 0
  | userNum > -1 && userNum < 1 = floor (userNum * fromIntegral nGroups)
  | userNum > fromIntegral nGroups = nGroups
  | userNum < 0 = pickMax (fromIntegral nGroups + userNum) nGroups
  | otherwise = floor userNum
