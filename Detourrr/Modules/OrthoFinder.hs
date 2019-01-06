module Detourrr.Modules.OrthoFinder
  where

-- TODO write a function to extract_seqs from multiple fastas at once, useful here + elsewhere?
-- TODO can all "extract" functions be renamed with "list"?
-- TODO try DIAMOND, MMseqs2

import Development.Shake
import Detourrr.Core.Types

import Data.List                   (isPrefixOf)
import Detourrr.Core.Actions       (debugA, debugNeed, readPaths, symlink, wrappedCmd)
import Detourrr.Core.Compile.Basic (defaultTypeCheck, rSimple)
import Detourrr.Core.Paths         (RrrPath, toRrrPath, fromRrrPath)
import Detourrr.Core.Util          (digest, readFileStrict, unlessExists)
import Detourrr.Modules.SeqIO      (faa)
import System.Directory            (createDirectoryIfMissing, renameDirectory)
import System.FilePath             ((</>), takeFileName)

rrrModule :: RrrModule
rrrModule = RrrModule
  { mName = "OrthoFinder"
  , mDesc = "Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree"
  , mTypes = [faa, ofr]
  , mFunctions =
      [ orthofinder
      ]
  }

ofr :: RrrType
ofr = RrrType
  { tExt  = "ofr"
  , tDesc = "OrthoFinder results"
  , tShow = \_ ref path -> do
      txt <- readFileStrict ref path
      return $ unlines $ take 17 $ lines txt
  }

-----------------
-- orthofinder --
-----------------

orthofinder :: RrrFunction
orthofinder = let name = "orthofinder" in RrrFunction
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
aOrthofinder :: RrrConfig -> Locks -> HashedSeqIDsRef -> [RrrPath] -> Action ()
aOrthofinder cfg ref _ [out, faListPath] = do

  let tmpDir = cfgTmpDir cfg </> "cache" </> "orthofinder" </> digest faListPath
      resDir = tmpDir </> "result"

  unlessExists resDir $ do
    liftIO $ createDirectoryIfMissing True tmpDir

    faPaths <- readPaths cfg ref faListPath'
    let faPaths' = map (fromRrrPath cfg) faPaths
    debugNeed cfg "aOrthofinder" faPaths'
    let faLinks = map (\p -> toRrrPath cfg $ tmpDir </> (takeFileName $ fromRrrPath cfg p)) faPaths
    mapM_ (\(p, l) -> symlink cfg ref l p) $ zip faPaths faLinks

    (o, e, _) <- wrappedCmd True False cfg ref (Just out'') faPaths' [] "orthofinder"
      [ "-f", tmpDir
      , "-S", "diamond" -- use DIAMOND instead of BLAST+
      , "-t", "8" -- TODO figure out with shake or ghc
      , "-a", "8" -- TODO figure out with shake or ghc
      ]
    putNormal $ unlines [o, e] -- TODO remove

    resName <- fmap last $ fmap (filter $ \p -> "Results_" `isPrefixOf` p) $ getDirectoryContents $ tmpDir </> "OrthoFinder"
    liftIO $ renameDirectory (tmpDir </> "OrthoFinder" </> resName) resDir

  symlink cfg ref out $ toRrrPath cfg $ resDir </> "Comparative_Genomics_Statistics" </> "Statistics_Overall.tsv"

  where
    out'        = fromRrrPath cfg out
    faListPath' = fromRrrPath cfg faListPath
    out''       = debugA cfg "aOrthofinder" out' [out', faListPath']

aOrthofinder _ _ _ args = error $ "bad argument to aOrthofinder: " ++ show args
