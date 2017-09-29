module ShortCut.Core.Debug
  ( debug
  , debugShow
  , debugParser
  , debugRules
  , debugAction
  , debugHash
  , debugPath
  , debugReadFile
  , debugReadLines
  , debugWriteFile
  , debugWriteLines
  , debugWriteChanged
  , debugTrackWrite
  , readFileStrict
  , readLinesStrict
  -- paths
  , debugWritePaths
  , debugReadPaths
  )
  where

import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Pretty ()
import Text.PrettyPrint.HughesPJClass

import Debug.Trace            (trace, traceShow)
import Control.Monad              (unless)
import System.IO.Error (isAlreadyInUseError, ioError, catchIOError)
import System.IO              (IOMode(..), withFile)
import System.IO.Strict       (hGetContents)
import Development.Shake.FilePath (makeRelative, (</>))

-- TODO add tags/description for filtering the output? (plus docopt to read them)
-- TODO rename to Shake.hs or something if it gathers more than debugging? combine with Eval.hs?

---------------------------------
-- basic wrappers around trace --
---------------------------------

debug :: CutConfig -> String -> a -> a
debug cfg msg rtn = if cfgDebug cfg then trace msg rtn else rtn

-- TODO stop exporting this in favor of the ones below?
debugShow :: Show a => CutConfig -> a -> b -> b
debugShow cfg shw rtn = if cfgDebug cfg then traceShow shw rtn else rtn

--------------------------------
-- debuggers for core modules --
--------------------------------

-- TODO are these all kind of the same fn? "debugExpr" or something

debugParser :: Pretty a => CutConfig -> String -> a -> a
debugParser cfg name res = debug cfg msg res
  where
    ren = render $ pPrint res
    msg = name ++ " parsed '" ++ ren ++ "'"

debugHash :: CutConfig -> String -> CutExpr -> String -> String
debugHash cfg name expr hash = debug cfg msg hash
  where
    ren = render $ pPrint expr
    msg = name ++ " hashed '" ++ ren ++ "' to " ++ hash

debugPath :: Show a => CutConfig -> String -> CutExpr -> a -> a
debugPath cfg name expr path = debug cfg msg path
  where
    ren = render $ pPrint expr
    msg = name ++ " for '" ++ ren ++ "' is " ++ show path -- TODO include types?

-- TODO restrict to CutExpr?
-- TODO put in rExpr to catch everything at once? but misses which fn was called
debugRules :: (Pretty a, Show b) => CutConfig -> String -> a -> b -> b
debugRules cfg name input output = debug cfg msg output
  where
    ren = render $ pPrint input
    msg = name ++ " compiled '" ++ ren ++ "' to " ++ show output

-- TODO put outPath last, here and in actual action fns
debugAction :: CutConfig -> String -> FilePath -> [String] -> FilePath
debugAction cfg name outPath args = debug cfg msg outPath
  where
    msg = name ++ " creating " ++ show outPath ++ " from " ++ show args

-----------------------------
-- handle duplicate writes --
-----------------------------

{- Turns out there's a race condition during `repeat` calls, because the same
 - literals are being compiled in each thread at roughly the same time. The way
 - I solved it was 1) check if the file as written already, and 2) if there's a
 - conflict in the middle of the operation anyway, ignore the error. Whichever
 - thread got there first will be writing the same exact text anyway.
 -}

-- TODO call this module something besides Debug now that it also handles errors?

unlessExists :: FilePath -> Action () -> Action ()
unlessExists path act = do
  e <- doesFileExist path
  unless e act

writeFileSafe :: FilePath -> String -> Action ()
writeFileSafe name x = liftIO $ catchIOError (writeFile name x) handler
  where
    handler e = if isAlreadyInUseError e
                  then return ()
                  else ioError e

writeLinesSafe :: FilePath -> [String] -> Action ()
writeLinesSafe name = writeFileSafe name . unlines

-----------------------------------
-- handle large numbers of reads --
-----------------------------------

{- Lazy IO turns out not to work well for printing large lists of literals
 - (couple hundred thousand at once). The solution is just to use strict IO.
 - See: https://github.com/ndmitchell/shake/issues/37
 -}

-- TODO don't use this since you should be needing whole groups of files?
readFileStrict :: FilePath -> Action String
readFileStrict path = need [path] >> liftIO (withFile path ReadMode hGetContents)
{-# INLINE readFileStrict #-}

readLinesStrict :: FilePath -> Action [String]
readLinesStrict = fmap lines . readFileStrict

-------------------------------------------------------
-- re-export Shake functions with new stuff attached --
-------------------------------------------------------

-- TODO rename like myReadFile, myReadLines?

debugReadFile :: CutConfig -> FilePath -> Action String
debugReadFile cfg f = debug cfg ("read '" ++ f ++ "'") (readFileStrict f)

debugWriteFile :: CutConfig -> FilePath -> String -> Action ()
debugWriteFile cfg f s = unlessExists f
                       $ debug cfg ("write '" ++ f ++ "'")
                       $ writeFileSafe f s

debugReadLines :: CutConfig -> FilePath -> Action [String]
debugReadLines cfg f = debug cfg ("read: " ++ f) (readLinesStrict f)

-- TODO remote in favor of only the Paths version?
debugWriteLines :: CutConfig -> FilePath -> [String] -> Action ()
debugWriteLines cfg f ss = unlessExists f
                         $ debug cfg ("write '" ++ f ++ "'")
                         $ writeLinesSafe f ss

debugWriteChanged :: CutConfig -> FilePath -> String -> Action ()
debugWriteChanged cfg f s = unlessExists f
                          $ debug cfg ("write '" ++ f ++ "'")
                          $ writeFileChanged f s

debugTrackWrite :: CutConfig -> [FilePath] -> Action ()
debugTrackWrite cfg fs = debug cfg ("write " ++ show fs) (trackWrite fs)

--------------------
-- path utilities --
--------------------

-- TODO remove the "debug" names and move to Paths2?

-- TODO take Path Abs File and convert them... or Path Rel File?
debugWritePaths :: CutConfig -> FilePath -> [FilePath] -> Action ()
debugWritePaths cfg out paths = debugWriteLines cfg out relPaths
  where
    relPaths = map (makeRelative $ cfgTmpDir cfg) paths

-- TODO convert these to Path Abs File
debugReadPaths :: CutConfig -> FilePath -> Action [FilePath]
debugReadPaths cfg path = (fmap . map) (cfgTmpDir cfg </>) (debugReadLines cfg path)

-- TODO debugReadLit(s)
-- TODO debugWriteLit(s)
