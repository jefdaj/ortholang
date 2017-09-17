module ShortCut.Core.Compile.Actions where

import Development.Shake
import ShortCut.Core.Types

import Data.List                  (sort)
import Data.String.Utils          (strip)
import Development.Shake.FilePath ((</>), (<.>))
import ShortCut.Core.Config       (wrappedCmd)
import ShortCut.Core.Debug        (debugReadFile, debugWriteLines, debugWriteFile,
                                   debugTrackWrite, debugReadLines, debugAction)
import ShortCut.Core.Util         (absolutize, resolveSymlinks)
import ShortCut.Core.Util         (stripWhiteSpace)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            (takeBaseName, makeRelative)
import System.FilePath            (takeDirectory)


-- from Compile (now Rules) --

aSetEmpty :: CutConfig -> FilePath -> Action ()
aSetEmpty cfg link = do
  wrappedCmd cfg [link] [] "touch" [link] -- TODO quietly?
  debugTrackWrite cfg [link']
  where
    link' = debugAction cfg "aSetEmpty" link [link]

aSetLits :: CutConfig -> FilePath -> [FilePath] -> Action ()
aSetLits cfg outPath relPaths = do
  lits  <- mapM (\p -> debugReadFile cfg $ cfgTmpDir cfg </> p) relPaths
  let lits' = sort $ map stripWhiteSpace lits
      out'  = debugAction cfg "aSetLits" outPath relPaths
  debugWriteLines cfg out' lits'

aVar :: CutConfig -> FilePath -> FilePath -> Action ()
aVar cfg dest link = do
  let destr  = ".." </> (makeRelative (cfgTmpDir cfg) dest)
      linkr  = ".." </> (makeRelative (cfgTmpDir cfg) link)
      link'  = debugAction cfg "aVar" link [dest]
  alwaysRerun
  need [dest]
  liftIO $ createDirectoryIfMissing True $ takeDirectory link
  wrappedCmd cfg [linkr] [] "ln" ["-fs", destr, link] -- TODO quietly?
  debugTrackWrite cfg [link']

aSetPaths :: CutConfig -> FilePath -> [FilePath] -> Action ()
aSetPaths cfg outPath paths = do
  need paths
  let out = debugAction cfg "aSetPaths" outPath paths
  -- TODO yup bug was here! any reason to keep it?
  -- paths' <- liftIO $ mapM resolveSymlinks paths
  debugWriteLines cfg out paths

aLit :: CutConfig -> CutExpr -> FilePath -> Action ()
aLit cfg expr out = debugWriteFile cfg out' $ ePath ++ "\n"
  where
    paths :: CutExpr -> FilePath
    paths (CutLit _ _ p) = p
    paths _ = error "bad argument to paths"
    ePath = paths expr
    out' = debugAction cfg "aLit" out [ePath]

-- from ModuleAPI --

aOneArgScript :: CutConfig -> String
              -> FilePath -> FilePath -> FilePath -> Action ()
aOneArgScript cfg oPath script tmpDir argPath = do
  need [argPath]
  liftIO $ createDirectoryIfMissing True tmpDir
  quietly $ unit $ wrappedCmd cfg [oPath] [] script [tmpDir, oPath, argPath]
  let oPath' = debugAction cfg "aOneArgScript" oPath [script,tmpDir,argPath]
  trackWrite [oPath']

aOneArgListScript :: CutConfig -> FilePath
                  -> String -> FilePath -> FilePath -> Action ()
aOneArgListScript cfg outPath script tmpDir faPath = do
  need [faPath]
  liftIO $ createDirectoryIfMissing True tmpDir
  wrappedCmd cfg [outPath] [Cwd tmpDir] script [outPath, faPath]
  -- debugWriteFile cfg outPath out
  let out = debugAction cfg "aOneArgListScript" outPath [script, tmpDir, faPath]
  debugTrackWrite cfg [out]

aLink :: CutConfig -> FilePath -> FilePath -> Action ()
aLink cfg outPath strPath = do
  pth <- fmap strip $ readFile' strPath
  src <- liftIO $ absolutize pth -- TODO also follow symlinks here?
  need [src]
  unit $ quietly $ wrappedCmd cfg [outPath] [] "ln" ["-fs", src, outPath]
  let out = debugAction cfg "aLink" outPath [strPath]
  debugTrackWrite cfg [out]

aLoadListOne :: CutConfig -> FilePath -> FilePath -> Action ()
aLoadListOne cfg outPath litsPath = do
  lits  <- debugReadLines cfg litsPath -- TODO strip?
  lits' <- liftIO $ mapM absolutize lits -- TODO does this mess up non-paths?
  let out = debugAction cfg "aLoadListOne" outPath [litsPath]
  debugWriteLines cfg out lits'

aLoadListMany :: CutConfig -> FilePath -> FilePath -> Action ()
aLoadListMany cfg outPath pathsPath = do
    paths <- fmap (map (cfgTmpDir cfg </>)) (debugReadLines cfg pathsPath)
    need paths
    paths' <- liftIO $ mapM resolveSymlinks paths
    -- need paths'
    let out = debugAction cfg "aLoadListMany" outPath [pathsPath]
    debugWriteLines cfg out paths'

aSimpleTmp :: CutConfig -> FilePath -> ActionFn -> FilePath -> [ExprPath] -> Action ()
aSimpleTmp cfg outPath actFn tmpDir argPaths = do
  let argPaths' = map (\(ExprPath p) -> p) argPaths
  need argPaths'
  liftIO $ createDirectoryIfMissing True tmpDir
  actFn cfg (CacheDir tmpDir) ([ExprPath outPath] ++ argPaths)
  let out = debugAction cfg "aSimpleTmp" outPath (tmpDir:argPaths') -- TODO actFn?
  trackWrite [out]

aMapLastArgs :: CutConfig -> FilePath -> [FilePath]
             -> FilePath -> FilePath -> Action ()
aMapLastArgs cfg outPath inits mapTmp lastsPath = do
  lastPaths <- readFileLines lastsPath
  -- this writes the .args files for use in the rule above
  (flip mapM_) lastPaths $ \p -> do
    -- TODO write the out path here too so all the args are together?
    let argsPath = mapTmp </> takeBaseName p <.> "args" -- TODO use a hash here?
        argPaths = inits ++ [cfgTmpDir cfg </> p]
    liftIO $ createDirectoryIfMissing True $ mapTmp
    debugWriteLines cfg argsPath argPaths
  -- then we just trigger them and write to the overall outPath
  let outPaths = map (\p -> mapTmp </> takeBaseName p) lastPaths
  need outPaths
  let out = debugAction cfg "aMapLastArgs" outPath (inits ++ [mapTmp, lastsPath])
  debugWriteLines cfg out outPaths

-- TODO rename this something less confusing
aMapLastMapTmp :: CutConfig
               -> ([FilePath] -> CacheDir)
               -> (CutConfig -> CacheDir -> [ExprPath] -> Action a)
               -> FilePath -> Action ()
aMapLastMapTmp cfg tmpFn actFn out = do
  let argsPath = out <.> ".args" -- TODO clean up
  -- args <- debugReadLines cfg argsPath
  args <- fmap lines $ liftIO $ readFile argsPath
  let args' = map (cfgTmpDir cfg </>) args
      rels  = map (makeRelative $ cfgTmpDir cfg) args
  need args'
  let (CacheDir dir) = tmpFn rels -- relative paths for determinism!
      args'' = out:args'
      out'   = debugAction cfg "aMapLastMapTmp" out args'
  liftIO $ createDirectoryIfMissing True dir
  liftIO $ putStrLn $ "args passed to actFn: " ++ show args''
  _ <- actFn cfg (CacheDir dir) (map ExprPath args'')
  trackWrite [out']
