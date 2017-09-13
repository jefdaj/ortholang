module ShortCut.Core.Actions where

import Development.Shake
import ShortCut.Core.Types

import Data.List                  (sort)
import Development.Shake.FilePath ((</>))
import ShortCut.Core.Config       (wrappedCmd)
import ShortCut.Core.Debug        (debugReadFile, debugWriteLines, debugWriteFile)
import ShortCut.Core.Util         (stripWhiteSpace)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            (makeRelative, takeDirectory)

aSetEmpty :: CutConfig -> FilePath -> Action ()
aSetEmpty cfg link = wrappedCmd cfg [link] [] "touch" [link] -- TODO quietly?

aSetLits :: CutConfig -> FilePath -> [FilePath] -> Action ()
aSetLits cfg outPath relPaths = do
  lits  <- mapM (\p -> debugReadFile cfg $ cfgTmpDir cfg </> p) relPaths
  let lits' = sort $ map stripWhiteSpace lits
  debugWriteLines cfg outPath lits'

aVar :: CutConfig -> FilePath -> FilePath -> Action ()
aVar cfg dest link = do
  let destr  = ".." </> (makeRelative (cfgTmpDir cfg) dest)
      linkr  = ".." </> (makeRelative (cfgTmpDir cfg) link)
  alwaysRerun
  need [dest]
  liftIO $ createDirectoryIfMissing True $ takeDirectory link
  wrappedCmd cfg [linkr] [] "ln" ["-fs", destr, link] -- TODO quietly?

aSetPaths :: CutConfig -> FilePath -> [FilePath] -> Action ()
aSetPaths cfg outPath paths' = do
  need paths'
  -- TODO yup bug was here! any reason to keep it?
  -- paths'' <- liftIO $ mapM resolveSymlinks paths'
  debugWriteLines cfg outPath paths'

aLit :: CutConfig -> CutExpr -> FilePath -> Action ()
aLit cfg expr out = debugWriteFile cfg out $ paths expr ++ "\n"
  where
    paths :: CutExpr -> FilePath
    paths (CutLit _ _ p) = p
    paths _ = error "bad argument to paths"
