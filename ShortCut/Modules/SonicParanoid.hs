module ShortCut.Modules.SonicParanoid
  where

-- TODO when sonicparanoid fails, delete the whole hash dir to avoid crashing next time

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Modules.SeqIO      (fna, faa)
import ShortCut.Core.Compile.Basic (defaultTypeCheck, rSimple)
import System.FilePath             ((</>), takeBaseName, takeDirectory)
import ShortCut.Core.Paths         (CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Actions       (debugA, debugNeed, readPaths, symlink, runCmd, CmdDesc(..), readFileStrict, debugTrackWrite)
import System.Directory            (createDirectoryIfMissing)
import ShortCut.Core.Util          (digest, unlessExists, resolveSymlinks)
import ShortCut.Core.Locks         (withWriteLock')
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
  , tShow = defaultShow
  -- , tShow = \cfg ref path -> do
  --     path' <- resolveSymlinks (Just $ cfgTmpDir cfg) path
  --     let dir = takeDirectory $ takeDirectory path'
  --         species = dir </> "species.txt"
  --     nSpecies <- fmap (length . lines) $ readFileStrict ref species
  --     return $ "sonicparanoid result " ++ takeBaseName dir ++ " (" ++ show nSpecies ++ " species)"
  }

-------------------
-- sonicparanoid --
-------------------

sonicparanoid :: CutFunction
sonicparanoid = let name = "sonicparanoid" in CutFunction
  { fName      = name
  , fTypeDesc  = mkTypeDesc  name [ListOf faa] spr -- TODO or fna
  , fTypeCheck = defaultTypeCheck [ListOf faa] spr -- TODO or fna
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

      -- TODO does this need some work to get consistently?
      statsPath'     = tmpDir </> "stats.tsv" -- this gets symlinked to the actual one, whose path varies
      statsPath      = toCutPath cfg statsPath'

      faListPath' = fromCutPath cfg faListPath
      out'        = fromCutPath cfg out
      statsPath''    = debugA cfg "aSonicParanoid" out' [out', statsPath', faListPath']
  liftIO $ createDirectoryIfMissing True sharedDir

  withWriteLock' ref tmpDir $ unlessExists statsPath' $ do
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
      , cmdArguments = [tmpDir, sharedDir, dbDir, inDir, "fast", "-d"]
      , cmdFixEmpties = False -- TODO do that?
      , cmdParallel = False -- TODO fix shake error associated with this
      , cmdOptions = []
      , cmdInPatterns = faPaths'
      , cmdOutPath = statsPath''
      , cmdExtraOutPaths = [] -- TODO what to do about this? there are a lot, sort of
      , cmdSanitizePaths = []
      , cmdExitCode = ExitSuccess
      , cmdRmPatterns = [tmpDir]
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

  -- TODO does this fix the "does not exist" issue?
  -- debugTrackWrite cfg [statsPath']
  symlink cfg ref out statsPath

aSonicParanoid _ _ _ args = error $ "bad argument to aSonicParanoid: " ++ show args
