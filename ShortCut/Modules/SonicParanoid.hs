module ShortCut.Modules.SonicParanoid
  where

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Modules.SeqIO      (fna, faa)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import System.FilePath             ((</>), takeBaseName, takeDirectory)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Actions       (debugA, debugNeed, readPaths, symlink, runCmd, CmdDesc(..), readFileStrict)
import System.Directory            (createDirectoryIfMissing)
import ShortCut.Core.Util          (digest, unlessExists, resolveSymlinks)
import System.Exit                 (ExitCode(..))

cutModule :: CutModule
cutModule = CutModule
  { mName = "SonicParanoid" , mDesc = "Very fast, accurate, and easy orthology."
  , mTypes = [faa, fna, spr]
  , mFunctions =
      [ sonicparanoid
      ]
  }

spr :: CutType
spr = CutType
  { tExt  = "spr"
  , tDesc = "SonicParanoid results"
  -- , tShow = defaultShow
  , tShow = \cfg ref path -> do
      path' <- resolveSymlinks (Just $ cfgTmpDir cfg) path
      let dir = takeDirectory $ takeDirectory path'
          species = dir </> "species.txt"
      nSpecies <- fmap (length . lines) $ readFileStrict ref species
      return $ "sonicparanoid result " ++ takeBaseName dir ++ " (" ++ show nSpecies ++ " species)"
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
-- TODO should get all results as an unusable file first, then extract what you want explicitly
aSonicParanoid :: CutConfig -> Locks -> HashedSeqIDsRef -> [CutPath] -> Action ()
aSonicParanoid cfg ref _ [out, faListPath] = do

  let cacheDir    = cfgTmpDir cfg </> "cache" </> "sonicparanoid"
      sharedDir   = cacheDir </> "shared"
      tmpDir      = cacheDir </> digest faListPath
      -- mmseqsDir   = sharedDir </> "mmseqs2_db"
      dbDir       = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "createdb" -- this is shared with the MMSeqs module TODO make explicit
      -- outDir      = tmpDir </> "result" -- TODO copy input files here?
      inDir       = tmpDir </> "input_links" -- TODO can you prevent it duplicating this to input?
      opPath'     = tmpDir </> "ortholog_relations" </> "ortholog_pairs.tsv"
      opPath      = toCutPath cfg opPath'
      faListPath' = fromCutPath cfg faListPath
      out'        = fromCutPath cfg out
      opPath''    = debugA cfg "aSonicParanoid" out' [out', opPath', faListPath']

  unlessExists opPath' $ do
    liftIO $ createDirectoryIfMissing True inDir -- sonicparanoid will create the others

    faPaths <- readPaths cfg ref faListPath'
    let faPaths' = map (fromCutPath cfg) faPaths
    debugNeed cfg "aSonicParanoid" faPaths'
    let faLinks = map (\p -> toCutPath cfg $ inDir </> (takeBaseName $ fromCutPath cfg p)) faPaths
    mapM_ (\(p, l) -> symlink cfg ref l p) $ zip faPaths faLinks

    -- TODO decide mode based on fn name
    -- TODO decide -d (debug) based on cfg? or leave one way?
    runCmd cfg ref $ CmdDesc
      { cmdBinary = "sonicparanoid.sh"
      , cmdArguments = [opPath', tmpDir, sharedDir, dbDir, inDir, "fast", "-d"]
      , cmdFixEmpties = False -- TODO do that?
      , cmdParallel = True
      , cmdOptions = []
      , cmdInPatterns = faPaths'
      , cmdOutPath = opPath''
      , cmdExtraOutPaths = []
      , cmdSanitizePaths = []
      , cmdExitCode = ExitSuccess
      }

    -- (o, e, _) <- wrappedCmd True False cfg ref (Just out'') faPaths' [] "sonicparanoid"
    --   [ "-sh", sharedDir
    --   , "-db", dbDir -- TODO share this with the mmseqs2 module
    --   , "-i", inDir
    --   , "-o", tmpDir
    --   , "-m", "fast" -- TODO set this based on fn name
    --   , "-noidx" -- TODO optional?
    --   , "-d" -- TODO set based on cfgDebug
    --   , "-op" -- write ortholog pairs
    --   -- , "-ka" -- TODO is this good?
    --   ]
    -- putNormal $ unlines [o, e] -- TODO remove

  symlink cfg ref out opPath

aSonicParanoid _ _ _ args = error $ "bad argument to aSonicParanoid: " ++ show args
