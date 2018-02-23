{-# LANGUAGE ScopedTypeVariables #-}

{- Some Shake Actions wrapped with ShortCut-specific additions,
 - and some other stuff. Eventually, it would be nice if all IO happened here.
 -
 - TODO rename to be shorter (no "safe", "wrapped", etc)
 -}

module ShortCut.Core.Actions

  -- read files
  ( readLit
  , readLits
  , readPath
  , readPaths
  , readLitPaths
  , readString
  , readStrings

  -- write files
  , writeLit
  , writeLits
  , writePath
  , writePaths
  , writeString
  , writeStrings
  , debugTrackWrite

  -- run system commands
  , wrappedCmd
  , wrappedCmdError
  , wrappedCmdExit
  , wrappedCmdOut
  , wrappedCmdWrite

  -- misc
  , digestFile  -- TODO what's the difference with hashContent?
  , hashContent -- TODO what's the difference with digestFile?
  , symlink

  )
  where

import Development.Shake
import ShortCut.Core.Types

import Control.Monad              (when)
import Data.List.Split            (splitOneOf)
import Development.Shake.FilePath ((</>), isAbsolute, pathSeparators, makeRelative)
import ShortCut.Core.Debug        (debug)
import ShortCut.Core.Paths        (CutPath, toCutPath, fromCutPath, checkLits,
                                   cacheDir, cutPathString, stringCutPath)
import ShortCut.Core.Util         (digest, digestLength, withLock, rmAll,
                                   unlessExists, ignoreExistsError, digest)
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))
import System.FileLock            (SharedExclusive(..))
import System.FilePath            ((<.>))
import System.IO                  (IOMode(..), withFile)
import System.IO.Strict           (hGetContents)
import System.Posix.Files         (createSymbolicLink)

----------------
-- read files --
----------------

{- Lazy IO turns out not to work well for printing large lists of literals
 - (couple hundred thousand at once). The solution is to use strict IO. And
 - also to write literal lists as single files, which is part of why there are
 - so many read/write functions.
 - See: https://github.com/ndmitchell/shake/issues/37
 - All reads should eventually go through this function to be safe!
 -}
readFileStrict :: FilePath -> Action String
readFileStrict path = do
  need [path]
  withLock Shared (path <.> "lock") $
    liftIO $ withFile path ReadMode hGetContents
{-# INLINE readFileStrict #-}

-- TODO something safer than head!
-- TODO error if they contain $TMPDIR or $WORKDIR?
readLit :: CutConfig -> FilePath -> Action String
readLit cfg path = readLits cfg path >>= return . head

readLits :: CutConfig -> FilePath -> Action [String]
readLits cfg path = debugReadLines cfg path >>= return . checkLits

-- TODO something safer than head!
readPath :: CutConfig -> FilePath -> Action CutPath
readPath cfg path = readPaths cfg path >>= return . head

readPaths :: CutConfig -> FilePath -> Action [CutPath]
readPaths cfg path = (fmap . map) stringCutPath (debugReadLines cfg path)

-- read a file as lines, convert to absolute paths, then parse those as cutpaths
-- used by the load_* functions to convert user-friendly relative paths to absolute
readLitPaths :: CutConfig -> FilePath -> Action [CutPath]
readLitPaths cfg path = do
  ls <- debugReadLines cfg path
  return $ map (toCutPath cfg . toAbs) ls
  where
    toAbs line = if isAbsolute line
                   then line
                   else cfgWorkDir cfg </> line

-- TODO something safer than head!
readString :: CutType -> CutConfig -> FilePath -> Action String
readString etype cfg path = readStrings etype cfg path >>= return . head

{- Read a "list of whatever". Mostly for generic set operations. You include
 - the CutType (of each element, not the list!) so it knows how to convert from
 - String, and then within the function you treat them as Strings.
 -}
readStrings :: CutType -> CutConfig -> FilePath -> Action [String]
readStrings etype cfg path = if etype' `elem` [str, num]
  then readLits cfg path
  else (fmap . map) (fromCutPath cfg) (readPaths cfg path)
  where
    etype' = debug cfg ("readStrings (each "
          ++ extOf etype ++ ") from " ++ path) etype

debugReadFile :: CutConfig -> FilePath -> Action String
debugReadFile cfg f = debug cfg ("read '" ++ f ++ "'") (readFileStrict f)

debugReadLines :: CutConfig -> FilePath -> Action [String]
debugReadLines cfg f = debug cfg ("read: " ++ f) 
                     $ (fmap lines . readFileStrict) f

-----------------
-- write files --
-----------------

{- This ensures that when two lists have the same content, their expression
 - paths will be links to the same cached path. That causes them to get
 - properly deduplicated when used in a set operation. It also makes the .tree
 - test files much stricter, since they'll change if any list element changes.
 -
 - TODO switch to md5sum/hashContent?
 - TODO does it need to handle a race condition when writing to the cache?
 - TODO any reason to keep original extensions instead of all using .txt?
 -      oh, if we're testing extensions anywhere. lets not do that though
 -}
writeCachedLines :: CutConfig -> FilePath -> [String] -> Action ()
writeCachedLines cfg outPath content = do
  let cDir  = fromCutPath cfg $ cacheDir cfg "lines" -- TODO make relative to expr
      cache = cDir </> digest content <.> "txt"
      lock  = cache <.> "lock"
  liftIO $ createDirectoryIfMissing True cDir
  withLock Exclusive lock
    $ unlessExists cache
    $ debug cfg ("writing '" ++ cache ++ "'")
    $ writeFile' cache (unlines content)
  symlink cfg (toCutPath cfg outPath) (toCutPath cfg cache)

-- TODO take a CutPath for the out file too
-- TODO take Path Abs File and convert them... or Path Rel File?
writePaths :: CutConfig -> FilePath -> [CutPath] -> Action ()
writePaths cfg out cpaths = writeCachedLines cfg out paths >> trackWrite paths
  where
    paths = map cutPathString cpaths

writePath :: CutConfig -> FilePath -> CutPath -> Action ()
writePath cfg out path = writePaths cfg out [path]

-- TODO error if they contain $TMPDIR or $WORKDIR?
writeLits :: CutConfig -> FilePath -> [String] -> Action ()
writeLits cfg path lits = writeCachedLines cfg path $ checkLits lits

writeLit :: CutConfig -> FilePath -> String -> Action ()
writeLit cfg path lit = writeLits cfg path [lit]

{- Write a "list of whatever". Mostly for generic set operations. You include
 - the CutType (of each element, not the list!) so it knows how to convert
 - to/from String, and then within the function you treat them as Strings.
 -}
writeStrings :: CutType -> CutConfig -> FilePath -> [String] -> Action ()
writeStrings etype cfg out whatevers = if etype' `elem` [str, num]
  then writeLits cfg out whatevers
  else writePaths cfg out $ map (toCutPath cfg) whatevers
  where
    etype' = debug cfg ("writeStrings (each " ++ extOf etype ++ "): " ++ show (take 3 whatevers)) etype

writeString :: CutType -> CutConfig -> FilePath -> String -> Action ()
writeString etype cfg out whatever = writeStrings etype cfg out [whatever]

{- Turns out there's a race condition during `repeat` calls, because the same
 - literals are being compiled in each thread at roughly the same time. The way
 - I solved it was 1) check if the file as written already, and 2) if there's a
 - conflict in the middle of the operation anyway, ignore the error. Whichever
 - thread got there first will be writing the same exact text anyway.
 -}

-- TODO need to delete the entire dir if given one?
-- This is safe in two ways:
-- 1. It skips writing if the file is already being written by another thread
-- 2. If some other error occurs it deletes the file, which is important
--    because it prevents it being interpreted as an empty list later
-- withErrorHandling :: (MonadIO m, MonadCatch m) => FilePath -> a -> m a -> m a
-- withErrorHandling path def fn = fn `catchIOError` (\e -> liftIO (handler e) >> return def)
--   where
--     handler e = if      isAlreadyInUseError  e then return ()
--                 else if isAlreadyExistsError e then return ()
--                 else    removeIfExists path >> ioError e

-- TODO rename like myReadFile, myReadLines?
debugTrackWrite :: CutConfig -> [FilePath] -> Action ()
debugTrackWrite cfg fs = debug cfg ("write " ++ show fs) (trackWrite fs)

-------------------------
-- run system commands --
-------------------------

-- TODO is the a type a good way to do this? it never actually gets evaluated
wrappedCmdError :: String -> Int -> [String] -> Action a
wrappedCmdError bin n files = do
  liftIO $ rmAll files
  error $ unlines $
    [ "Oh no! " ++ bin ++ " failed with error code " ++ show n ++ "."
    , "The files it was working on have been deleted:"
    ] ++ files

-- TODO call this when exiting nonzero and/or exception thrown
-- TODO take a list of globs and resolve them to files
-- TODO delete the files, telling shake if possible
-- TODO print a message for the user
-- TODO raise/re-raise an exception

-- Shake's command_ adapted to work with wrapperScript and wrapperLimit if used
-- TODO gather shake stuff into a Shake.hs module?
--      could have config, debug, wrappedCmd, eval...
-- ptns is a list of patterns for files to delete in case the cmd fails
wrappedCmd :: CutConfig -> FilePath -> [CmdOption] -> String -> [String]
           -> Action (String, String, Int)
-- TODO withErrorHandling2 is blocking on some MVar related thing :(
-- wrappedCmd cfg path opts bin args = withErrorHandling2 path $ withLockFile path $
-- TODO separate wrappedReadCmd with a shared lock?
wrappedCmd cfg path opts bin args = withLock Exclusive (path <.> "lock") $ do
  liftIO $ putStrLn $ "wrappedCmd args: " ++ show args -- TODO remove
  (Stdout out, Stderr err, Exit code) <- case cfgWrapper cfg of
    Nothing -> command opts bin args
    Just w  -> command opts w (bin:args)
  let code' = case code of
                ExitSuccess   -> 0
                ExitFailure n -> n
  -- TODO does this properly go somewhere else? what handles multiple output files?
  when (code' == 0) (debugTrackWrite cfg [path])
  return (out, err, code')

{- OK I think this is an issue of immediately returning rather than waiting for
 - another thread to write the same output file? then it hasn't been tracked as
 - written yet or something?
 -}

-- Note that this one doesn't have wrappedCmdError,
-- because it's used for when you expect a nonzero exit code.
-- TODO write some other error checking to go along with it!
-- TODO track writes?
-- TODO just return the output + exit code directly and let the caller handle it
-- TODO issue with not re-raising errors here?
wrappedCmdExit :: CutConfig -> FilePath -> [CmdOption] -> FilePath -> [String]
               -> Action Int
wrappedCmdExit c p os b as = do
  (_, _, code) <- wrappedCmd c p os b as
  return code

{- wrappedCmd specialized for commands that write one or more files. If the
 - command succeeds it tells Shake which files were written, and if it fails
 - they get deleted.
 - TODO skip the command if the files already exist?
 -}
wrappedCmdWrite :: CutConfig -> FilePath -> [String] -> [CmdOption] -> FilePath
                -> [String] -> Action ()
wrappedCmdWrite c p ps os b as = do -- TODO why the "failed to build" errors?
  code <- wrappedCmdExit c p os b as
  case code of
    0 -> debugTrackWrite c ps
    n -> wrappedCmdError b n (ps ++ [p])

{- wrappedCmd specialized for commands that return their output as a string.
 - TODO remove this? it's only used to get columns from blast hit tables
 -}
wrappedCmdOut :: CutConfig -> FilePath -> [String] -> [CmdOption] -> FilePath
              -> [String] -> Action String
wrappedCmdOut c p ps os b as = do
  (out, err, code) <- wrappedCmd c p os b as
  case code of
    0 -> return out
    n -> do
      liftIO $ putStrLn $ unlines [out, err]
      wrappedCmdError b n (ps ++ [p])

----------
-- misc --
----------

digestFile :: CutConfig -> FilePath -> Action String
digestFile cfg path = debugReadFile cfg path >>= return . digest

hashContent :: CutConfig -> CutPath -> Action String
hashContent cfg path = do
  need [path']
  Stdout out <- command [] "md5sum" [path']
  let md5 = take digestLength $ head $ words out
  return md5
  where
    path' = fromCutPath cfg path

{- Takes source and destination paths in the tmpdir and makes a path between
 - them with the right number of dots.
 - TODO check that the CutPath is in TMPDIR, not WORKDIR!
 -}
tmpLink :: CutConfig -> FilePath -> FilePath -> FilePath
tmpLink cfg src dst = dots </> tmpRel dst
  where
    tmpRel  = makeRelative $ cfgTmpDir cfg
    dots    = foldr1 (</>) $ take (nSeps - 1) $ repeat ".."
    nSeps   = length $ splitOneOf pathSeparators $ tmpRel src

{- Note that src here means what's sometimes called the destination. The first
 - arg should be the symlink path and the second the file it points to. (it
 - was going to be kind of confusing either way)
 -}
symlink :: CutConfig -> CutPath -> CutPath -> Action ()
symlink cfg src dst = do
  liftIO $ ignoreExistsError $ createSymbolicLink dstr src'
  debugTrackWrite cfg [src'] -- TODO anything wrong with duplicate calls?
  where
    src' = fromCutPath cfg src
    dst' = fromCutPath cfg dst
    dstr = tmpLink cfg src' dst' -- TODO use cutpaths here too?
