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
import qualified Data.Map as M

import Control.Monad               (forM)
import ShortCut.Core.Actions       (readLit, readLits, writeLits, cachedLinesPath, absolutizePaths,
                                    writePaths, readFileStrict', readPaths, runCmd, CmdDesc(..), debugNeed)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath, exprPath, upBy, cacheDir)
import ShortCut.Core.Util          (headOrDie, digest)
import System.FilePath             ((</>), takeDirectory)
import Text.Regex.Posix            ((=~))
import ShortCut.Core.Compile.Basic (rExpr, debugRules)
import Data.Scientific             (toRealFloat)
import System.Exit                 (ExitCode(..))
import System.FilePath             ((<.>))
import System.Directory            (createDirectoryIfMissing)
import Data.IORef                  (readIORef)
import Data.List                   (isPrefixOf)

import ShortCut.Modules.SeqIO         (faa)
import ShortCut.Modules.OrthoFinder   (ofr)
import ShortCut.Modules.SonicParanoid (spr)
import ShortCut.Modules.GreenCut      (gcr)

-- this is just shorthand
sll :: CutType
sll = ListOf (ListOf str)

type PickerFn = Int -> Int
type PickerFn2 = Double -> Int -> Int

cutModule :: CutModule
cutModule = CutModule
  { mName = "OrthoGroups"
  , mDesc = "Common interface for working with the results of OrthoFinder, SonicParanoid, etc."
  , mTypes = [og, ofr, spr]
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
og :: CutType
og = CutTypeGroup
  { tgExt = "og"
  , tgDesc = "orthogroups (orthofinder or sonicparanoid results)"
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

-- TODO move parse fns to their respective modules for easier maintenance

-- TODO why are we unhashing the ids here?
parseOrthoFinder :: CutConfig -> Locks -> HashedIDsRef -> CutPath -> Action [[String]]
parseOrthoFinder cfg ref _ ofrPath = do
  let resDir = fromCutPath cfg $ upBy 2 ofrPath
      orthoPath = resDir </> "Orthogroups" </> "Orthogroups.txt"
  -- ids <- liftIO $ readIORef idref
  -- txt <- fmap (unhashIDs False ids) $ readFileStrict' cfg ref orthoPath -- TODO openFile error during this?
  txt <- readFileStrict' cfg ref orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  return groups

parseSonicParanoid :: CutConfig -> Locks -> HashedIDsRef -> CutPath -> Action [[String]]
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

writeOrthogroups :: CutConfig -> Locks -> HashedIDsRef -> CutPath -> [[String]] -> Action ()
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
aOrthogroups :: CutType -> CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
aOrthogroups rtn cfg ref idsref [out, ogPath] = do
  -- liftIO $ putStrLn $ "ogPath: " ++ show ogPath
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

lookupID :: HashedIDs -> String -> String
lookupID ids s = case filter (\(_,v) -> s `isPrefixOf` v) (M.toList ids) of
  ((k,_):[]) -> k
  ([])   -> error $ "ERROR: id '" ++ s ++ "' not found"
  ms     -> error $ "ERROR: multiple ids match '" ++ s ++ "': " ++ show ms

aOrthogroupContaining :: CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
aOrthogroupContaining cfg ref ids [out, ofrPath, idPath] = do
  ids' <- liftIO $ readIORef ids
  geneId <- fmap (lookupID ids') $ readLit cfg ref $ fromCutPath cfg idPath
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

aOrthogroupsFilter :: FilterFn -> CutConfig -> Locks -> HashedIDsRef -> [CutPath] -> Action ()
aOrthogroupsFilter filterFn cfg ref ids [out, ofrPath, idsPath] = do
  ids' <- liftIO $ readIORef ids
  geneIds <- fmap (map $ lookupID ids') $ readLits cfg ref $ fromCutPath cfg idsPath
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
  , fTypeDesc  = mkTypeDesc  name [og, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck [og, ListOf faa] sll
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
  , fRules     = rOrthologFilterStr "min" pickAny
  }

pickAny :: Int -> Int
pickAny _ = 1

rOrthologFilterStr :: String -> PickerFn -> RulesFn
rOrthologFilterStr fnName pickerFn st@(_, cfg, ref, _) e@(CutFun _ _ _ _ [groupLists, idLists]) = do
  (ExprPath groupListsPath) <- rExpr st groupLists
  (ExprPath idListsPath   ) <- rExpr st idLists
  let out     = exprPath st e
      out'    = debugRules cfg "rOrthologFilterStr" e $ fromCutPath cfg out
      ogDir   = fromCutPath cfg $ ogCache cfg
      ogsPath = ogDir </> digest groupListsPath <.> "txt"
      idsPath = ogDir </> digest idListsPath    <.> "txt"
  liftIO $ createDirectoryIfMissing True ogDir
  ogsPath %> absolutizePaths cfg ref groupListsPath
  idsPath %> absolutizePaths cfg ref idListsPath
  out' %> \_ -> do
    -- TODO is there a way to avoid reading this?
    nIDs  <- fmap length $ readPaths cfg ref idListsPath
    let fnArg' = show $ pickerFn nIDs
    debugNeed cfg "orthogroups.py" [ogsPath, idsPath] -- TODO shouldn't cmdInPatterns pick that up?
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

orthologInAll :: CutFunction
orthologInAll = let name = "ortholog_in_all" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [og, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck [og, ListOf faa] sll
  , fFixity    = Prefix
  , fRules     = mkOrthologsStrRules "ortholog_in_all"
  }

orthologInAllStr :: CutFunction
orthologInAllStr = let name = "ortholog_in_all_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [sll, sll] sll
  , fTypeCheck = defaultTypeCheck [sll, sll] sll
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStr "min" pickAll
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

orthologInMinStr :: CutFunction
orthologInMinStr = let name = "ortholog_in_min_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [num, sll, sll] sll
  , fTypeCheck = defaultTypeCheck [num, sll, sll] sll
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStrFrac "min" pickMin
  }

-- read a scientific and print again as a string
-- toDecimal :: String -> String
-- toDecimal s = formatScientific Fixed Nothing (read s)

ogCache :: CutConfig -> CutPath
ogCache cfg = cacheDir cfg "orthogroups"

rOrthologFilterStrFrac :: String -> PickerFn2 -> RulesFn
rOrthologFilterStrFrac fnName pickerFn st@(_, cfg, ref, _) e@(CutFun _ _ _ _ [frac, groupLists, idLists]) = do
  (ExprPath fracPath      ) <- rExpr st frac
  (ExprPath groupListsPath) <- rExpr st groupLists
  (ExprPath idListsPath   ) <- rExpr st idLists
  let out     = exprPath st e
      out'    = debugRules cfg "rOrthologFilterStr" e $ fromCutPath cfg out
      ogDir   = fromCutPath cfg $ ogCache cfg
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
    debugNeed cfg "orthogroups.py" [ogsPath, idsPath] -- TODO shouldn't cmdInPatterns pick that up?
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

orthologInMin :: CutFunction
orthologInMin = let name = "ortholog_in_min" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [num, og, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck [num, og, ListOf faa] sll
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
  , fTypeDesc  = mkTypeDesc  name [num, og, ListOf faa] sll
  , fTypeCheck = defaultTypeCheck [num, og, ListOf faa] sll
  , fFixity    = Prefix
  , fRules     = mkOrthologsStrFracRules name
  }

orthologInMaxStr :: CutFunction
orthologInMaxStr = let name = "ortholog_in_max_str" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [num, sll, sll] sll
  , fTypeCheck = defaultTypeCheck [num, sll, sll] sll
  , fFixity    = Prefix
  , fRules     = rOrthologFilterStrFrac "max" pickMax
  }

pickMax :: (RealFrac a, Integral b) => a -> b -> b
pickMax userNum nGroups
  | userNum == 0 = 0
  | userNum > -1 && userNum < 1 = floor (userNum * fromIntegral nGroups)
  | userNum > fromIntegral nGroups = nGroups
  | userNum < 0 = pickMax (fromIntegral nGroups + userNum) nGroups
  | otherwise = floor userNum
