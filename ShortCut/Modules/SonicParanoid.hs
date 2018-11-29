module ShortCut.Modules.SonicParanoid
  where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Modules.SeqIO      (fna, faa)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)

cutModule :: CutModule
cutModule = CutModule
  { mName = "SonicParanoid"
  , mDesc = "Very fast, accurate, and easy orthology."
  , mTypes = [faa, fna, spr]
  , mFunctions =
      [
      ]
  }

spr :: CutType
spr = CutType
  { tExt  = "spr"
  , tDesc = "SonicParanoid results"
  , tShow = undefined
  -- , tShow = \_ ref path -> do
  --     txt <- readFileStrict ref path
  --     return $ unlines $ take 17 $ lines txt
  }

-------------------
-- sonicparanoid --
-------------------

orthofinder :: CutFunction
orthofinder = let name = "sonicparanoid" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ListOf faa] spr -- TODO or fna
  , fTypeCheck = defaultTypeCheck [ListOf faa] spr -- TODO or fna
  , fDesc      = Just "Run SonicParanoid on a list of genomes in FASTA format.\n\
                      \It produces lots of result files! Use the extract_* functions\n\
                      \or look in the TMPDIR to find the specific info you want."
  , fFixity    = Prefix
  , fRules     = rSimple aSonicParanoid
  }

-- TODO do blast separately and link to outputs from the WorkingDirectory dir, and check if same results
-- TODO what's diamond blast? do i need to add it?
aSonicParanoid :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aSonicParanoid cfg ref _ [out, faListPath] = do
  undefined
--   let tmpDir = cfgTmpDir cfg </> "cache" </> "orthofinder" </> digest faListPath
--       resDir = tmpDir </> "result"
--   unlessExists resDir $ do
--     liftIO $ createDirectoryIfMissing True tmpDir
--     faPaths <- readPaths cfg ref faListPath'
--     let faPaths' = map (fromCutPath cfg) faPaths
--     debugNeed cfg "aSonicParanoid" faPaths'
--     let faLinks = map (\p -> toCutPath cfg $ tmpDir </> (takeFileName $ fromCutPath cfg p)) faPaths
--     mapM_ (\(p, l) -> symlink cfg ref l p) $ zip faPaths faLinks
--     (o, e, _) <- wrappedCmd True False cfg ref (Just out'') faPaths' [] "orthofinder"
--       [ "-f", tmpDir
--       , "-S", "diamond" -- use DIAMOND instead of BLAST+
--       , "-t", "8" -- TODO figure out with shake or ghc
--       , "-a", "8" -- TODO figure out with shake or ghc
--       ]
--     putNormal $ unlines [o, e] -- TODO remove
--     resName <- fmap last $ fmap (filter $ \p -> "Results_" `isPrefixOf` p) $ getDirectoryContents $ tmpDir </> "OrthoFinder"
--     liftIO $ renameDirectory (tmpDir </> "OrthoFinder" </> resName) resDir
--   symlink cfg ref out $ toCutPath cfg $ resDir </> "Comparative_Genomics_Statistics" </> "Statistics_Overall.tsv"
--   where
--     out'        = fromCutPath cfg out
--     faListPath' = fromCutPath cfg faListPath
--     out''       = debugA cfg "aSonicParanoid" out' [out', faListPath']
aSonicParanoid _ _ _ args = error $ "bad argument to aSonicParanoid: " ++ show args
