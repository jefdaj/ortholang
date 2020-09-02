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
import OrthoLang.Types
import OrthoLang.Interpreter

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
import Data.Maybe (fromJust)

-- this is just shorthand
sll :: Type
sll = ListOf (ListOf str)

type PickerFn = Int -> Int
type PickerFn2 = Double -> Int -> Int

olModule :: Module
olModule = Module
  { mName = "OrthoGroups"
  , mDesc = "Common interface for working with the results of OrthoFinder, SonicParanoid, etc."
  , mTypes = [ofr, spr, gcr]
  , mGroups = [og]
  , mRules = return ()
  , mEncodings = []
  , mFunctions =
      [ orthogroups
      , orthogroupContaining
      , orthogroupsContaining
      -- these four are user-facing macros that expand to the Str versions
      , orthologInAny
      , orthologInAll
      , orthologInMin
      , orthologInMax
      -- and these four expose the implementation for easier testing
      , orthologInAnyStr
      , orthologInAllStr
      , orthologInMinStr
      , orthologInMaxStr
      ]
  }

-- TODO should there be single and plural versions?
og :: TypeGroup
og = TypeGroup
  { tgExt = "og"
  , tgDesc = "orthogroup list"
  , tgMembers = [Exactly ofr, Exactly spr, Exactly gcr]
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
orthogroups = newFnA1
  "orthogroups"
  (Some og "orthogroup")
  (Exactly sll)
  aOrthogroups -- TODO detect arg type inside
  []

-- TODO move parse fns to their respective modules for easier maintenance

-- TODO why are we unhashing the ids here?
parseOrthoFinder :: Path -> Action [[String]]
parseOrthoFinder ofrPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.orthogroups.parseOrthoFinder"
      resDir = fromPath loc cfg $ upBy 2 ofrPath
      orthoPath = resDir </> "Orthogroups" </> "Orthogroups.txt"
  -- ids <- liftIO $ readIORef idref
  -- txt <- fmap (unhashIDs False ids) $ readFileStrict' orthoPath -- TODO openFile error during this?
  txt <- readFileStrict' orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  return groups

parseSonicParanoid :: Path -> Action [[String]]
parseSonicParanoid ogPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.orthogroups.parseSonicParanoid"
      resDir = takeDirectory $ fromPath loc cfg ogPath
      grpPath = resDir </> "ortholog_groups.tsv"
  -- ids <- liftIO $ readIORef idref -- TODO why are we unhashing here again?
  -- txt <- fmap (unhashIDs cfg ids) $ readFileStrict' grpPath -- TODO openFile error during this?
  txt <- readFileStrict' grpPath -- TODO openFile error during this?
  let groups = map parseLine $ tail $ lines txt -- TODO be safe about tail
  -- liftIO $ putStrLn $ "groups: " ++ show groups
      --groups' = map (unhashIDs cfg) groups
  -- let groups = map (words . drop 11) (lines txt)
  return groups
  where
    parseLine l = concat (l =~ "seqid_[a-zA-Z0-9]*?" :: [[String]])

parseGreenCut :: Path -> Action [[String]]
parseGreenCut ogPath = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.orthogroups.parseGreenCut"
  txt <- readFileStrict' $ fromPath loc cfg ogPath
  let groups = map parseLine $ lines txt
  return groups
  where
    parseLine l = filter (/= ":") $ split "\t" l

writeOrthogroups :: Path -> [[String]] -> Action ()
writeOrthogroups out groups = do
  -- let groups' = (map . map) (unhashIDs cfg ids) groups
  -- ids   <- liftIO $ readIORef idsref
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.orthogroups.writeOrthogroups"
  paths <- forM groups $ \group -> do
    let path = cachedLinesPath cfg group -- TODO should this use group'?
        -- group' = map (unhashIDs cfg ids) group
    -- liftIO $ putStrLn $ "group': " ++ show group'
    writeLits loc path group
    return $ toPath loc cfg path
  writePaths loc (fromPath loc cfg out) paths

-- TODO something wrong with the paths/lits here, and it breaks parsing the script??
-- TODO separate haskell fn to just list groups, useful for extracting only one too?
-- TODO translate hashes back into actual seqids here?
aOrthogroups :: NewAction1
aOrthogroups (ExprPath out') ogPath' = do
  -- liftIO $ putStrLn $ "ogPath: " ++ show ogPath
  -- TODO extract this into a parseOrthogroups function
  cfg  <- fmap fromJust getShakeExtra
  dRef <- fmap fromJust getShakeExtra
  rtn <- liftIO $ decodeNewRulesType cfg dRef (ExprPath out')
  let parser = if      rtn == spr then parseSonicParanoid
               else if rtn == ofr then parseOrthoFinder
               else if rtn == gcr then parseGreenCut
               else error $ "bad type for aOrthogroups: " ++ show rtn
      loc = "modules.orthogroups.aOrthogroups"
      out = toPath loc cfg out'
      ogPath = toPath loc cfg ogPath'
  groups <- parser ogPath
  writeOrthogroups out groups

---------------------------
-- orthogroup_containing --
---------------------------

orthogroupContaining :: Function
orthogroupContaining = newFnA2
  "orthogroup_containing"
  (Some og "any orthogroup", Exactly str)
  (Exactly $ ListOf str)
  aOrthogroupContaining
  []

aOrthogroupContaining :: NewAction2
aOrthogroupContaining (ExprPath out) ofrPath' idPath = do
  ids  <- fmap fromJust getShakeExtra
  ids' <- liftIO $ readIORef ids
  cfg  <- fmap fromJust getShakeExtra
  let loc = "modules.orthogroups.aOrthogroupContaining"
      ofrPath = toPath loc cfg ofrPath'
  partialID <- readLit loc idPath
  -- TODO should there be a separate case for multiple matches?
  let geneId = case lookupID ids' partialID of
                 Just k -> k
                 Nothing -> error $ "ERROR: id \"" ++ partialID ++ "' not found"
  groups' <- fmap (filter $ elem geneId) $ parseOrthoFinder ofrPath -- TODO handle the others!
  let group = if null groups' then [] else headOrDie "aOrthogroupContaining failed" groups' -- TODO check for more?
  writeLits loc out group

----------------------------
-- orthogroups_containing --
----------------------------

-- TODO think of a better name for this

orthogroupsContaining :: Function
orthogroupsContaining = newFnA2
  "orthogroups_containing"
  (Some og "any orthogroup", Exactly $ ListOf str)
  (Exactly sll)
  (aOrthogroupsFilter containsOneOf)
  []

type FilterFn = [[String]] -> [String] -> [[String]]

-- see https://stackoverflow.com/a/13271723
containsOneOf :: FilterFn
containsOneOf lists elems = filter (flip any elems . flip elem) lists

-- TODO should this error when not finding one too, like aOrthogroupContaining?
aOrthogroupsFilter :: FilterFn -> NewAction2
aOrthogroupsFilter filterFn (ExprPath out') ofrPath' idsPath = do
  ids  <- fmap fromJust getShakeExtra
  ids' <- liftIO $ readIORef ids
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.orthogroups.aOrthogroupsFilter"
      out = toPath loc cfg out'
      ofrPath = toPath loc cfg ofrPath'
  lookups <- fmap (map $ lookupID ids') $ readLits loc idsPath
  when (not $ all isJust lookups) $ error "unable to find some seqids! probably a programming error"
  let geneIds = catMaybes lookups
  groups <- parseOrthoFinder ofrPath -- TODO handle the others!
  let groups' = filterFn groups geneIds
  writeOrthogroups out groups'

----------------------------
-- ortholog_in_{any,all}* --
----------------------------

orthologInAny :: Function
orthologInAny =
  let name = "ortholog_in_any"
  in newExprExpansion
       name
       [Some og "any orthogroup", Exactly $ ListOf faa]
       (Exactly sll)
       (mOrthologInStr name)
       []

mOrthologInStr :: String -> ExprExpansion
mOrthologInStr name _ _ (Fun rType seed deps _  [groups , faas]) =
  Fun rType seed deps (name ++ "_str")   [groups', faas']
  where
    groups' = Fun sll seed (depsOf groups) "orthogroups"      [groups]
    faas'   = Fun sll seed (depsOf faas  ) "extract_ids_each" [faas]
mOrthologInStr _ _ _ _ = error "bad arguments to mOrthologInStr"

-- TODO can this be removed somehow?
-- TODO flip args so it reads more naturally?

-- TODO generate this?
orthologInAnyStr :: Function
orthologInAnyStr = newFnA2
  "ortholog_in_any_str"
  (Exactly sll, Exactly sll)
  (Exactly sll)
  (aOrthologFilterStr "min" pickAny)
  [Hidden]

pickAny :: Int -> Int
pickAny _ = 1

-- TODO flip args so it reads more naturally?

-- TODO fn to generate this + orthologInAny
orthologInAll :: Function
orthologInAll =
  let name = "ortholog_in_all"
  in newExprExpansion
       name
       [Some og "any orthogroup", Exactly $ ListOf faa]
       (Exactly sll)
       (mOrthologInStr name)
       []

-- TODO generate this?
orthologInAllStr :: Function
orthologInAllStr = newFnA2
  "ortholog_in_all_str"
  (Exactly sll, Exactly sll)
  (Exactly sll)
  (aOrthologFilterStr "min" pickAll)
  [Hidden]

pickAll :: Int -> Int
pickAll nIDs = nIDs

aOrthologFilterStr :: String -> PickerFn -> NewAction2
aOrthologFilterStr fnName pickerFn (ExprPath out) groupListsPath idListsPath = do
  cfg  <- fmap fromJust getShakeExtra
  let loc = "modules.orthogroups.rOrthologFilterStr"
      out'    = debugRules loc cfg out
      ogDir   = fromPath loc cfg $ ogCache cfg
      ogsPath = ogDir </> digest loc groupListsPath <.> "txt"
      idsPath = ogDir </> digest loc idListsPath    <.> "txt"
  liftIO $ createDirectoryIfMissing True ogDir
  absolutizePaths loc groupListsPath ogsPath -- TODO separate function?
  absolutizePaths loc idListsPath idsPath -- TODO separate function?
  -- TODO is there a way to avoid reading this?
  nIDs  <- fmap length $ readPaths loc idListsPath
  let fnArg' = show $ pickerFn nIDs
  -- need' loc [ogsPath, idsPath] -- TODO shouldn't cmdInPatterns pick that up?
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out'
    , cmdInPatterns = [ogsPath, idsPath]
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [out'] -- TODO is this right?
    , cmdOptions =[]
    , cmdBinary = "orthogroups.py"
    , cmdArguments = [out', fnName, fnArg', ogsPath, idsPath]
    , cmdRmPatterns = []
    , cmdExitCode = ExitSuccess
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

-- TODO generate this?
orthologInMinStr :: Function
orthologInMinStr = newFnA3
  "ortholog_in_min_str"
  (Exactly num, Exactly sll, Exactly sll)
  (Exactly sll)
  (aOrthologFilterStrFrac "min" pickMin)
  [Hidden]

-- read a scientific and print again as a string
-- toDecimal :: String -> String
-- toDecimal s = formatScientific Fixed Nothing (read s)

ogCache :: Config -> Path
ogCache cfg = cacheDir cfg "orthogroups"

aOrthologFilterStrFrac :: String -> PickerFn2 -> NewAction3
aOrthologFilterStrFrac fnName pickerFn (ExprPath out) fracPath groupListsPath idListsPath = do
  cfg  <- fmap fromJust getShakeExtra
  let loc = "modules.orthogroups.rOrthologFilterStr"
      out'    = debugRules loc cfg out
      ogDir   = fromPath loc cfg $ ogCache cfg
      ogsPath = ogDir </> digest loc groupListsPath <.> "txt"
      idsPath = ogDir </> digest loc idListsPath    <.> "txt"
  liftIO $ createDirectoryIfMissing True ogDir
  absolutizePaths loc groupListsPath ogsPath -- TODO separate function?
  absolutizePaths loc idListsPath idsPath -- TODO separate function?
  -- TODO is there a way to avoid reading this?
  fnArg <- readLit loc fracPath
  nIDs  <- fmap length $ readPaths loc idListsPath
  -- let fnArg' = show $ pickerFn nIDs
  let fnArg' = show $ pickerFn (toRealFloat (read fnArg) :: Double) nIDs
  -- need' loc [ogsPath, idsPath] -- TODO shouldn't cmdInPatterns pick that up?
  runCmd $ CmdDesc
    { cmdParallel = False
    , cmdFixEmpties = True
    , cmdOutPath = out'
    , cmdInPatterns = [ogsPath, idsPath]
    , cmdNoNeedDirs = []
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = [out'] -- TODO is this right?
    , cmdOptions =[]
    , cmdBinary = "orthogroups.py"
    , cmdArguments = [out', fnName, fnArg', ogsPath, idsPath]
    , cmdRmPatterns = []
    , cmdExitCode = ExitSuccess
    }

-- TODO fn to generate this
orthologInMin :: Function
orthologInMin =
  let name = "ortholog_in_min"
  in newExprExpansion
       name
       [Exactly num, Some og "any orthogroup", Exactly $ ListOf faa]
       (Exactly sll)
       (mOrthologInStr name)
       []

mOrthologInStrFrac :: String -> ExprExpansion
mOrthologInStrFrac name _ _ (Fun rType seed deps _  [frac, groups , faas]) =
  Fun rType seed deps (name ++ "_str")   [frac, groups', faas']
  where
    groups' = Fun sll seed (depsOf groups) "orthogroups"      [groups]
    faas'   = Fun sll seed (depsOf faas  ) "extract_ids_each" [faas]
mOrthologInStrFrac _ _ _ _ = error "bad arguments to mOrthologInStrFrac"

---------------------
-- ortholog_in_max --
---------------------

-- TODO fn to generate this
orthologInMax :: Function
orthologInMax =
  let name = "ortholog_in_max"
  in newExprExpansion
       name
       [Exactly num, Some og "any orthogroup", Exactly $ ListOf faa]
       (Exactly sll)
       (mOrthologInStrFrac name)
       []

-- TODO generate this?
orthologInMaxStr :: Function
orthologInMaxStr = newFnA3
  "ortholog_in_max_str"
  (Exactly num, Exactly sll, Exactly sll)
  (Exactly sll)
  (aOrthologFilterStrFrac "max" pickMax)
  [Hidden]

pickMax :: (RealFrac a, Integral b) => a -> b -> b
pickMax userNum nGroups
  | userNum == 0 = 0
  | userNum > -1 && userNum < 1 = floor (userNum * fromIntegral nGroups)
  | userNum > fromIntegral nGroups = nGroups
  | userNum < 0 = pickMax (fromIntegral nGroups + userNum) nGroups
  | otherwise = floor userNum
