module ShortCut.Modules.OrthoFinder
  where

import Development.Shake
import ShortCut.Core.Types

import Data.List                   (isPrefixOf)
import ShortCut.Core.Actions       (debugA, debugNeed, readPaths, symlink, wrappedCmd)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Util          (digest, readFileStrict, unlessExists)
import ShortCut.Modules.SeqIO      (faa)
import System.Directory            (createDirectoryIfMissing, renameDirectory)
import System.FilePath             ((</>), takeFileName)

cutModule :: CutModule
cutModule = CutModule
  { mName = "OrthoFinder"
  , mDesc = "Inference of orthologs, orthogroups, the rooted species, gene trees and gene duplcation events tree"
  , mTypes = [faa, ofr]
  , mFunctions = [orthofinder]
  }

ofr :: CutType
ofr = CutType
  { tExt  = "ofr"
  , tDesc = "OrthoFinder results"
  , tShow = \_ ref path -> do
      txt <- readFileStrict ref path
      return $ unlines $ take 16 $ lines txt
  }

orthofinder :: CutFunction
orthofinder = let name = "orthofinder" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ListOf faa] ofr
  , fTypeCheck = defaultTypeCheck [ListOf faa] ofr
  , fDesc      = Nothing
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
