module ShortCut.Modules.OrthoFinder
  where

-- TODO write a function to extract_seqs from multiple fastas at once, useful here + elsewhere?
-- TODO can all "extract" functions be renamed with "list"?
-- TODO try DIAMOND, MMseqs2

import Development.Shake
import ShortCut.Core.Types

import Data.List                   (isPrefixOf)
import ShortCut.Core.Actions       (debugA, debugNeed, readPaths, symlink, wrappedCmd,
                                    writeLits, cachedLinesPath, writePaths)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Util          (digest, readFileStrict, unlessExists, resolveSymlinks)
import ShortCut.Modules.SeqIO      (faa)
import System.Directory            (createDirectoryIfMissing, renameDirectory)
import System.FilePath             ((</>), takeFileName, takeDirectory)
import Control.Monad               (forM)

cutModule :: CutModule
cutModule = CutModule
  { mName = "OrthoFinder"
  , mDesc = "Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree"
  , mTypes = [faa, ofr]
  , mFunctions = [orthofinder, extractGroups]
  }

ofr :: CutType
ofr = CutType
  { tExt  = "ofr"
  , tDesc = "OrthoFinder results"
  , tShow = \_ ref path -> do
      txt <- readFileStrict ref path
      return $ unlines $ take 16 $ lines txt
  }

-----------------
-- orthofinder --
-----------------

orthofinder :: CutFunction
orthofinder = let name = "orthofinder" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ListOf faa] ofr
  , fTypeCheck = defaultTypeCheck [ListOf faa] ofr
  , fDesc      = Just "Run OrthoFinder on a list of genomes in FASTA format.\n\
                      \It produces lots of result files! Use the extract_* functions\n\
                      \or look in the TMPDIR to find the specific info you want."
  , fFixity    = Prefix
  , fRules     = rSimple aOrthofinder
  }

-- TODO do blast separately and link to outputs from the WorkingDirectory dir, and check if same results
-- TODO what's diamond blast? do i need to add it?
aOrthofinder :: CutConfig -> Locks -> [CutPath] -> Action ()
aOrthofinder cfg ref [out, faListPath] = do
  let tmpDir' = cfgTmpDir cfg </> "cache" </> "orthofinder" </> digest faListPath
      resDir' = tmpDir' </> "result"
  unlessExists resDir' $ do
    liftIO $ createDirectoryIfMissing True tmpDir'
    faPaths <- readPaths cfg ref faListPath'
    let faPaths' = map (fromCutPath cfg) faPaths
    debugNeed cfg "aOrthofinder" faPaths'
    let faLinks = map (\p -> toCutPath cfg $ tmpDir' </> (takeFileName $ fromCutPath cfg p)) faPaths
    mapM_ (\(p, l) -> symlink cfg ref l p) $ zip faPaths faLinks
    (o, e, _) <- wrappedCmd True False cfg ref (Just out'') faPaths' [] "orthofinder"
      [ "-f", tmpDir'
      , "-S", "diamond" -- use DIAMOND instead of BLAST+
      , "-t", "8" -- TODO figure out with shake or ghc
      , "-a", "8" -- TODO figure out with shake or ghc
      ]
    putNormal $ unlines [o, e]
    resName <- fmap last $ fmap (filter $ \p -> "Results_" `isPrefixOf` p) $ getDirectoryContents tmpDir'
    liftIO $ renameDirectory (tmpDir' </> resName) resDir'
  symlink cfg ref out $ toCutPath cfg $ resDir' </> "Statistics_Overall.csv"
  where
    out'        = fromCutPath cfg out
    faListPath' = fromCutPath cfg faListPath
    out''       = debugA cfg "aOrthofinder" out' [out', faListPath']
aOrthofinder _ _ args = error $ "bad argument to aOrthofinder: " ++ show args

--------------------
-- extract_groups --
--------------------

-- TODO list_groups?
-- TODO separate module that works with multiple ortholog programs?
-- TODO version to get the group matching an ID
extractGroups :: CutFunction
extractGroups = let name = "extract_groups" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ofr] (ListOf (ListOf str))
  , fTypeCheck = defaultTypeCheck [ofr] (ListOf (ListOf str))
  , fDesc      = Just "List the genes in all orthogroups."
  , fFixity    = Prefix
  , fRules     = rSimple aExtractGroups
  }

-- TODO something wrong with the paths/lits here, and it breaks parsing the script??
-- TODO separate haskell fn to just list groups, useful for extracting only one too?
aExtractGroups :: CutConfig -> Locks -> [CutPath] -> Action ()
aExtractGroups cfg ref [out, ofrPath] = do
  resDir' <- fmap takeDirectory $ liftIO $ resolveSymlinks (Just $ cfgTmpDir cfg) (fromCutPath cfg ofrPath)
  let orthoPath = resDir' </> "Orthogroups.txt"
  txt <- readFile' orthoPath -- TODO openFile error during this?
  let groups = map (words . drop 11) (lines txt)
  paths <- forM groups $ \group -> do
    let path = cachedLinesPath cfg group
    writeLits cfg ref path group -- TODO maybe this should be strings? lits with str type?
    return $ toCutPath cfg path
  writePaths cfg ref (fromCutPath cfg out) paths
aExtractGroups _ _ args = error $ "bad argument to aExtractGroups: " ++ show args
