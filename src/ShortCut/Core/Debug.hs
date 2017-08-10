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
import Debug.Trace            (trace, traceShow)
import ShortCut.Core.Types    (CutConfig(..))
import Control.Monad              (unless)

-- TODO add tags/description for filtering the output? (plus docopt to read them)

---------------------------------
-- basic wrappers around trace --
---------------------------------

debug :: CutConfig -> String -> a -> a
debug cfg str rtn = if cfgDebug cfg then trace str rtn else rtn

debugShow :: Show a => CutConfig -> a -> b -> b
debugShow cfg shw rtn = if cfgDebug cfg then traceShow shw rtn else rtn


------------------------------------
-- add checks for duplicate files --
------------------------------------

unlessExists :: FilePath -> Action () -> Action ()
unlessExists path act = do
  e <- doesFileExist path
  unless e act

-------------------------------------------------------
-- re-export Shake functions with new stuff attached --
-------------------------------------------------------

-- TODO is there a more elegant way?

debugReadFile :: CutConfig -> FilePath -> Action String
debugReadFile cfg f = debug cfg ("read: " ++ f) (readFile' f)

debugWriteFile :: CutConfig -> FilePath -> String -> Action ()
debugWriteFile cfg f s = unlessExists f
                       $ debug cfg ("write: " ++ f)
                       $ writeFile' f s

debugReadLines :: CutConfig -> FilePath -> Action [String]
debugReadLines cfg f = debug cfg ("read: " ++ f) (readFileLines f)

debugWriteLines :: CutConfig -> FilePath -> [String] -> Action ()
debugWriteLines cfg f ss = unlessExists f
                         $ debug cfg ("write: " ++ f)
                         $ writeFileLines f ss

debugWriteChanged :: CutConfig -> FilePath -> String -> Action ()
debugWriteChanged cfg f s = unlessExists f
                          $ debug cfg ("write: " ++ f)
                          $ writeFileChanged f s

debugTrackWrite :: CutConfig -> [FilePath] -> Action ()
debugTrackWrite cfg fs = debug cfg ("write: " ++ show fs) (trackWrite fs)
