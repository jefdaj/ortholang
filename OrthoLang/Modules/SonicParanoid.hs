module OrthoLang.Modules.SonicParanoid
  where

-- TODO when sonicparanoid fails, delete the whole hash dir to avoid crashing next time

import Development.Shake
import OrthoLang.Core
import OrthoLang.Locks

import OrthoLang.Modules.SeqIO (fna, faa)
import System.Directory        (createDirectoryIfMissing)
import System.Exit             (ExitCode(..))
import System.FilePath         ((</>), takeBaseName)
import Data.Maybe (fromJust)

olModule :: Module
olModule = Module
  { mName = "SonicParanoid" , mDesc = "Very fast, accurate, and easy orthology."
  , mTypes = [faa, fna, spr]
  , mFunctions =
      [ sonicparanoid
      ]
  }

spr :: Type
spr = Type
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

sonicparanoid :: Function
sonicparanoid = let name = "sonicparanoid" in Function
  { fOpChar = Nothing, fName = name
  -- , fTypeDesc  = mkTypeDesc  name [ListOf faa] spr -- TODO or fna
  -- , fTypeCheck = defaultTypeCheck name [ListOf faa] spr -- TODO or fna
  , fInputs = [Exactly $ ListOf faa]
  , fOutput = Exactly spr
  , fTags = []
  , fNewRules = NewNotImplemented
  , fOldRules = rSimple aSonicParanoid
  }

-- TODO run mmseqs2 separately and put the results in tmpDir first, then use -mo
--      (or let sonicparanoid run it and link from here to the mmseqs2 tmpdir)
-- TODO should get all results as an unusable file first, then extract what you want explicitly
aSonicParanoid :: [Path] -> Action ()
aSonicParanoid [out, faListPath] = do
  cfg <- fmap fromJust getShakeExtra
  let cDir    = cfgTmpDir cfg </> "cache" </> "sonicparanoid"
      sharedDir   = cDir </> "shared"
      tmpDir      = cDir </> digest faListPath
      -- mmseqsDir   = sharedDir </> "mmseqs2_db"
      dbDir       = cfgTmpDir cfg </> "cache" </> "mmseqs" </> "createdb" -- this is shared with the MMSeqs module TODO make explicit
      -- outDir      = tmpDir </> "result" -- TODO copy input files here?
      inDir       = tmpDir </> "input_links" -- TODO can you prevent it duplicating this to input?

      -- TODO does this need some work to get consistently?
      statsPath'     = tmpDir </> "stats.tsv" -- this gets symlinked to the actual one, whose path varies
      statsPath      = toPath cfg statsPath'

      faListPath' = fromPath cfg faListPath
      out'        = fromPath cfg out
      statsPath''    = traceA "aSonicParanoid" out' [out', statsPath', faListPath']
  liftIO $ createDirectoryIfMissing True sharedDir

  withWriteLock' tmpDir $ unlessExists statsPath' $ do
    liftIO $ createDirectoryIfMissing True inDir -- sonicparanoid will create the others

    faPaths <- readPaths faListPath'
    let faPaths' = map (fromPath cfg) faPaths
    need' "ortholang.moodules.sonicparanoid.aSonicParanoid" faPaths'
    let faLinks = map (\p -> toPath cfg $ inDir </> (takeBaseName $ fromPath cfg p)) faPaths
    mapM_ (\(p, l) -> symlink l p) $ zip faPaths faLinks

    -- TODO decide mode based on fn name
    -- TODO decide -d (debug) based on cfg? or leave one way?
    runCmd $ CmdDesc
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
  -- trackWrite' [statsPath']
  symlink out statsPath

aSonicParanoid args = error $ "bad argument to aSonicParanoid: " ++ show args
