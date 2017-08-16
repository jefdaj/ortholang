module ShortCut.Core.Debug
  ( debug
  , debugShow
  , debugReadFile
  , debugReadLines
  , debugWriteFile
  , debugWriteLines
  , debugWriteChanged
  , debugTrackWrite
  , readFileStrict
  , readLinesStrict
  )
  where

import Development.Shake
import Debug.Trace            (trace, traceShow)
import ShortCut.Core.Types    (CutConfig(..))
import Control.Monad              (unless)
import System.IO.Error (isAlreadyInUseError, ioError, catchIOError)
import System.IO              (IOMode(..), withFile)
import System.IO.Strict       (hGetContents)

-- TODO add tags/description for filtering the output? (plus docopt to read them)
-- TODO rename to Shake.hs or something if it gathers more than debugging? combine with Eval.hs?

---------------------------------
-- basic wrappers around trace --
---------------------------------

debug :: CutConfig -> String -> a -> a
debug cfg str rtn = if cfgDebug cfg then trace str rtn else rtn

debugShow :: Show a => CutConfig -> a -> b -> b
debugShow cfg shw rtn = if cfgDebug cfg then traceShow shw rtn else rtn


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
debugReadFile cfg f = debug cfg ("read: " ++ f) (readFileStrict f)

debugWriteFile :: CutConfig -> FilePath -> String -> Action ()
debugWriteFile cfg f s = unlessExists f
                       $ debug cfg ("write: " ++ f)
                       $ writeFileSafe f s

debugReadLines :: CutConfig -> FilePath -> Action [String]
debugReadLines cfg f = debug cfg ("read: " ++ f) (readLinesStrict f)

debugWriteLines :: CutConfig -> FilePath -> [String] -> Action ()
debugWriteLines cfg f ss = unlessExists f
                         $ debug cfg ("write: " ++ f)
                         $ writeLinesSafe f ss

debugWriteChanged :: CutConfig -> FilePath -> String -> Action ()
debugWriteChanged cfg f s = unlessExists f
                          $ debug cfg ("write: " ++ f)
                          $ writeFileChanged f s

debugTrackWrite :: CutConfig -> [FilePath] -> Action ()
debugTrackWrite cfg fs = debug cfg ("write: " ++ show fs) (trackWrite fs)
