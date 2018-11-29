module ShortCut.Modules.SonicParanoid
  where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Modules.SeqIO      (fna, faa)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import System.FilePath             ((</>), takeBaseName)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Actions       (debugA, debugNeed, readPaths, symlink, wrappedCmd)
import System.Directory            (createDirectoryIfMissing)
import ShortCut.Core.Util          (digest, unlessExists)

cutModule :: CutModule
cutModule = CutModule
  { mName = "SonicParanoid"
  , mDesc = "Very fast, accurate, and easy orthology."
  , mTypes = [faa, fna, spr]
  , mFunctions =
      [ sonicparanoid
      ]
  }

spr :: CutType
spr = CutType
  { tExt  = "spr"
  , tDesc = "SonicParanoid results"
  , tShow = defaultShow
  -- , tShow = \_ ref path -> do
  --     txt <- readFileStrict ref path
  --     return $ unlines $ take 17 $ lines txt
  }

-------------------
-- sonicparanoid --
-------------------

sonicparanoid :: CutFunction
sonicparanoid = let name = "sonicparanoid" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ListOf faa] spr -- TODO or fna
  , fTypeCheck = defaultTypeCheck [ListOf faa] spr -- TODO or fna
  , fDesc      = Just "Run SonicParanoid on a list of genomes in FASTA format.\n\
                      \It produces lots of result files! Use the extract_* functions\n\
                      \or look in the TMPDIR to find the specific info you want."
  , fFixity    = Prefix
  , fRules     = rSimple aSonicParanoid
  }

-- TODO run mmseqs2 separately and put the results in tmpDir first, then use -mo
--      (or let sonicparanoid run it and link from here to the mmseqs2 tmpdir)
aSonicParanoid :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aSonicParanoid cfg ref _ [out, faListPath] = do

  let tmpDir      = cfgTmpDir cfg </> "cache" </> "sonicparanoid" </> digest faListPath
      inDir       = tmpDir </> "input"
      sharedDir   = tmpDir </> "shared"
      mmseqsDir   = sharedDir </> "mmseqs2_db"
      outDir      = tmpDir </> "result"
      opPath'     = outDir </> "ortholog_relations" </> "ortholog_pairs.tsv"
      opPath      = toCutPath cfg opPath'
      faListPath' = fromCutPath cfg faListPath
      out'        = fromCutPath cfg out
      out''       = debugA cfg "aSonicParanoid" out' [out', faListPath']

  unlessExists outDir $ do
    liftIO $ createDirectoryIfMissing True inDir -- sonicparanoid will create the others

    faPaths <- readPaths cfg ref faListPath'
    let faPaths' = map (fromCutPath cfg) faPaths
    debugNeed cfg "aSonicParanoid" faPaths'
    let faLinks = map (\p -> toCutPath cfg $ inDir </> (takeBaseName $ fromCutPath cfg p)) faPaths
    mapM_ (\(p, l) -> symlink cfg ref l p) $ zip faPaths faLinks

    (o, e, _) <- wrappedCmd True False cfg ref (Just out'') faPaths' [] "sonicparanoid"
      [ "-sh", sharedDir
      , "-db", mmseqsDir -- TODO is this the best place?
      , "-i", inDir
      , "-o", outDir
      , "-m", "fast" -- TODO set this based on fn name
      , "-noidx" -- TODO optional?
      , "-d" -- TODO set based on cfgDebug
      , "-op" -- write ortholog pairs
      ]
    putNormal $ unlines [o, e] -- TODO remove

  symlink cfg ref out opPath

aSonicParanoid _ _ _ args = error $ "bad argument to aSonicParanoid: " ++ show args
