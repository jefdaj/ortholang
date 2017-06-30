module ShortCut.Core.Debug
  ( debug
  , debugShow
  , debugReadFile
  , debugReadLines
  , debugWriteFile
  , debugWriteLines
  , debugWriteChanged
  , debugTrackWrite
  )
  where

import Development.Shake

import Control.Monad.IO.Class (MonadIO)
import Debug.Trace            (trace, traceShow)
import ShortCut.Core.Types    (CutConfig(..))

-- TODO add tags/description for filtering the output? (plus docopt to read them)

---------------------------------
-- basic wrappers around trace --
---------------------------------

debug :: CutConfig -> String -> a -> a
debug cfg str rtn = if cfgDebug cfg then trace str rtn else rtn

debugShow :: Show a => CutConfig -> a -> b -> b
debugShow cfg shw rtn = if cfgDebug cfg then traceShow shw rtn else rtn

-------------------------------------------------------
-- re-export Shake functions with debugging attached --
-------------------------------------------------------

-- TODO is there a more elegant way?

debugReadFile :: CutConfig -> FilePath -> Action String
debugReadFile cfg f = debug cfg ("read: " ++ f) (readFile' f)

debugWriteFile :: MonadIO m => CutConfig -> FilePath -> String -> m ()
debugWriteFile cfg f s = debug cfg ("write: " ++ f) (writeFile' f s)

debugReadLines :: CutConfig -> FilePath -> Action [String]
debugReadLines cfg f = debug cfg ("read: " ++ f) (readFileLines f)

debugWriteLines :: MonadIO m => CutConfig -> FilePath -> [String] -> m ()
debugWriteLines cfg f ss = debug cfg ("write: " ++ f) (writeFileLines f ss)

debugWriteChanged :: MonadIO m => CutConfig -> FilePath -> String -> m ()
debugWriteChanged cfg f s = debug cfg ("write: " ++ f) (writeFileChanged f s)

debugTrackWrite :: CutConfig -> [FilePath] -> Action ()
debugTrackWrite cfg fs = debug cfg ("write: " ++ show fs) (trackWrite fs)
