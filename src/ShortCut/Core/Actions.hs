{-# LANGUAGE ScopedTypeVariables #-}

{- Some Shake Actions wrapped with ShortCut-specific additions,
 - and some other stuff. Eventually, it would be nice if all IO happened here.
 -
 - TODO rename to be shorter (no "safe", "wrapped", etc)
 -}

module ShortCut.Core.Actions
  ( wrappedCmd
  , wrappedCmdWrite
  , wrappedCmdExit
  , wrappedCmdOut
  , wrappedCmdError -- for calling when a cmd is found to have failed
  , debugReadFile
  , debugReadLines
  , debugWriteFile
  , debugWriteLines
  , debugWriteChanged
  , debugTrackWrite
  , readFileStrict
  , readLinesStrict
  -- , withErrorHandling
  -- , unlessExists
  -- , unlessAnyExist
  , removeIfExists
  , readPath
  , readPaths
  , readString
  , readStrings
  , readLitPaths
  , readLit
  , readLits
  , writeLit
  , writeLits
  , writeString
  , writeStrings
  , writePath
  , writePaths
  , hashContent
  , symlink
  , writeDeduped
  -- , withLock
  -- , tryIO -- TODO move to Util
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
import ShortCut.Core.Util         (digest, digestLength, removeIfExists, withLock,
                                   writeAllOnce, unlessExists, rmAll)
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))
import System.FilePath            ((<.>))
import System.IO                  (IOMode(..), withFile)
import System.IO.Strict           (hGetContents)
import System.Posix.Files         (createSymbolicLink)
-- import System.FileLock            (withFileLock, SharedExclusive(..))

------------------------
-- write files safely --
------------------------

-- instance MonadThrow Action where
  -- throwM = liftIO . throwM

-- instance MonadCatch Action where
  -- catch = catch

-- TODO combine this with withErrorHandling into one big "safe write" fn
-- TODO if the action fails, still remove the lockfile (and the outfile patterns!)

-- TODO put these around every write operation! will probably help a ton
-- TODO what happens if the process is inturrupted? should delete both
-- TODO are they being removed properly afterward?
-- TODO should you use the outfiles themselves as the lockfiles too? might simplify a bit
-- withLockFile :: (MonadIO m, MonadCatch m) => FilePath -> m a -> m a
-- withLockFile path act = do
--   let lockPath  = path <.> "lock"
--   liftIO $ createDirectoryIfMissing True $ takeDirectory lockPath
--   lock <- liftIO $ lockFile lockPath Exclusive -- TODO oh no! need to remove lock on errors
--   -- case lock of
--     -- Nothing -> return () -- TODO also need to unlock here? seems an odd way
--     -- Just l  -> do
--   res <- act
--   -- res <- E.catch act (\(e :: IOError) -> liftIO (unlockFile lock))
--   -- res <- catchIOError act (\e -> liftIO (tryIO (removeFile lockPath)) >> throwM e)
--   -- TODO why does removing the lockfiles after cause errors in the OTHER withLockFile (Test/Scripts)??
--   -- liftIO (unlockFile l >> tryIO (removeIfExists lockPath))
--   liftIO (unlockFile lock >> removeIfExists lockPath)
--   return res
  
-- try an IO action and ignore if it fails
-- TODO is there a standard name for this?
-- TODO use catchIOError instead?
-- tryIO :: (MonadIO m, MonadCatch m) => m () -> m ()
-- tryIO fn = fn `catchIOError` (\_ -> return ())

-- TODO failing tests:
--        blast_db_each (parallelblast.py race? also fails to delete files)
--        no rule available for final outfile:
--          crb_blast_each.lock "Cought IO exception: removeLink: does not exist (No such file or directory)"
--          best_hits (924 vs 926 hits)
--          extract_ids
--          concat_fastas
--          extract_seqs
--          gba_to_faa
--          gba_to_fna
--          gba_to_fna_concat
--          translate
--          repeat_recursive "openFile: resource busy (file is locked)"

---------
-- cmd --
---------

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
wrappedCmd cfg path opts bin args = withLock (path <.> "lock") $ do
  (Stdout out, Stderr err, Exit code) <- case cfgWrapper cfg of
    Nothing -> command opts bin args
    Just w  -> command opts w (bin:args)
  let code' = case code of
                ExitSuccess   -> 0
                ExitFailure n -> n
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

-----------------------------
-- handle duplicate writes --
-----------------------------

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

-- TODO remove after integrating new withLockFile code
-- withErrorHandling2 :: (MonadIO m, MonadCatch m) => FilePath -> m a -> m a
-- withErrorHandling2 path fn = fn `catchIOError` (\e -> liftIO (handler e))
--   where
--     handler e = removeIfExists (path <.> "lock") >> ioError e

-- TODO debugTrackWrite after?
writeFileSafe :: CutConfig -> FilePath -> String -> Action ()
writeFileSafe cfg path x =
  writeAllOnce (path <.> "lock") [path] $ writeFile'' path x -- TODO lock error here?
  where
    writeFile'' p = debug cfg ("writing '" ++ p ++ "'") (writeFile' p)

-- TODO debugTrackWrite after?
writeLinesSafe :: CutConfig -> FilePath -> [String] -> Action ()
writeLinesSafe cfg path = writeFileSafe cfg path . unlines

-----------------------------------
-- handle large numbers of reads --
-----------------------------------

{- Lazy IO turns out not to work well for printing large lists of literals
 - (couple hundred thousand at once). The solution is just to use strict IO.
 - See: https://github.com/ndmitchell/shake/issues/37
 -}

-- TODO don't use this since you should be needing whole groups of files?
readFileStrict :: FilePath -> Action String
readFileStrict path = do
  need [path]
  -- liftIO $ withFileLock (path <.> "lock") Shared $ \_ ->
  liftIO $ withFile path ReadMode hGetContents
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
                       $ writeFileSafe cfg f s

debugReadLines :: CutConfig -> FilePath -> Action [String]
debugReadLines cfg f = debug cfg ("read: " ++ f) (readLinesStrict f)

-- TODO track written in these!
-- TODO remote in favor of only the Paths version?
debugWriteLines :: CutConfig -> FilePath -> [String] -> Action ()
debugWriteLines cfg f ss = unlessExists f
                         $ debug cfg ("write '" ++ f ++ "'")
                         $ writeLinesSafe cfg f ss

debugWriteChanged :: CutConfig -> FilePath -> String -> Action ()
debugWriteChanged cfg f s = unlessExists f
                          $ debug cfg ("write '" ++ f ++ "'")
                          $ writeFileSafe cfg f s

debugTrackWrite :: CutConfig -> [FilePath] -> Action ()
debugTrackWrite cfg fs = debug cfg ("write " ++ show fs) (trackWrite fs)


-------------
-- file io --
-------------

readPaths :: CutConfig -> FilePath -> Action [CutPath]
readPaths cfg path = (fmap . map) stringCutPath (debugReadLines cfg path)

-- TODO something safer than head!
readPath :: CutConfig -> FilePath -> Action CutPath
readPath cfg path = readPaths cfg path >>= return . head

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

-- TODO take a CutPath for the out file too
-- TODO take Path Abs File and convert them... or Path Rel File?
writePaths :: CutConfig -> FilePath -> [CutPath] -> Action ()
writePaths cfg out cpaths = debugWriteLines cfg out paths >> trackWrite paths
  where
    paths = map cutPathString cpaths

writePath :: CutConfig -> FilePath -> CutPath -> Action ()
writePath cfg out path = writePaths cfg out [path]

readLits :: CutConfig -> FilePath -> Action [String]
readLits cfg path = debugReadLines cfg path >>= return . checkLits

-- TODO something safer than head!
-- TODO error if they contain $TMPDIR or $WORKDIR?
readLit :: CutConfig -> FilePath -> Action String
readLit cfg path = readLits cfg path >>= return . head

-- TODO error if they contain $TMPDIR or $WORKDIR?
writeLits :: CutConfig -> FilePath -> [String] -> Action ()
writeLits cfg path lits = debugWriteLines cfg path $ checkLits lits

writeLit :: CutConfig -> FilePath -> String -> Action ()
writeLit cfg path lit = writeLits cfg path [lit]

----------------------------------------
-- read and write tmpfiles as strings --
----------------------------------------

-- These are useful for generic functions like in Sets.hs which operate on
-- "lists of whatever". You include the CutType (of each element, not the
-- list!) so it knows how to convert to/from String, and then within the
-- function you treat them as Strings.

readStrings :: CutType -> CutConfig -> FilePath -> Action [String]
readStrings etype cfg path = if etype' `elem` [str, num]
  then readLits cfg path
  else (fmap . map) (fromCutPath cfg) (readPaths cfg path)
  where
    etype' = debug cfg ("readStrings (each " ++ extOf etype ++ ") from " ++ path) etype

readString :: CutType -> CutConfig -> FilePath -> Action String
readString etype cfg path = readStrings etype cfg path >>= return . head

-- TODO if given paths and writing lits, should this read them?
writeStrings :: CutType -> CutConfig -> FilePath -> [String] -> Action ()
writeStrings etype cfg out whatevers = if etype' `elem` [str, num]
  then writeLits cfg out whatevers
  else writePaths cfg out $ map (toCutPath cfg) whatevers
  where
    etype' = debug cfg ("writeStrings (each " ++ extOf etype ++ "): " ++ show (take 3 whatevers)) etype

writeString :: CutType -> CutConfig -> FilePath -> String -> Action ()
writeString etype cfg out whatever = writeStrings etype cfg out [whatever]

----------------
-- hash files --
----------------

hashContent :: CutConfig -> CutPath -> Action String
hashContent cfg path = do
  need [path']
  -- liftIO $ putStrLn $ "hashing " ++ path'
  -- Removed wrappedCmdOut because it caused a permissions error during tests,
  -- probably by accessing the data dir to make a lock. Besides no lock is actually needed.
  -- out <- wrappedCmdOut cfg (path' <.> "md5") [] [] "md5sum" [path']
  Stdout out <- command [] "md5sum" [path']
  let md5 = take digestLength $ head $ words out -- TODO adapt failGracfully to work here
  -- liftIO $ putStrLn $ "md5sum of " ++ path' ++ " is " ++ md5
  return md5
  where
    path' = fromCutPath cfg path

-------------
-- symlink --
-------------

-- TODO separate module like Core.Actions?
-- TODO which is src and which dst in this, and which in the rest of the code?

-- takes source and destination paths in the tmpdir and makes a path between
-- them with the right number of dots
-- TODO check that the CutPath is in TMPDIR, not WORKDIR!
tmpLink :: CutConfig -> FilePath -> FilePath -> FilePath
tmpLink cfg src dst = dots </> tmpRel dst
  where
    tmpRel  = makeRelative $ cfgTmpDir cfg
    dots    = foldr1 (</>) $ take (nSeps - 1) $ repeat ".."
    nSeps   = length $ splitOneOf pathSeparators $ tmpRel src

-- Note that src here means what's sometimes called the destination.
-- The first arg should be the symlink path and the second the file it points to.
-- (it was going to be kind of confusing either way)
--
-- TODO fix error on race condition: if src exists already, just skip it
symlink :: CutConfig -> CutPath -> CutPath -> Action ()
symlink cfg src dst =
  writeAllOnce (src' <.> "lock") [src'] $ do
    liftIO $ createSymbolicLink dstr src'
    -- debugTrackWrite cfg [src']) TODO add this and remove elsewhere?
  -- liftIO $ createSymbolicLink dstr src'
  where
    src' = fromCutPath cfg src
    dst' = fromCutPath cfg dst
    dstr = tmpLink cfg src' dst' -- TODO use cutpaths here too?

-------------------
-- write deduped --
-------------------

{- This ensures that when two lists have the same content, their expression
 - paths will be links to the same cached path. That causes them to get
 - properly deduplicated when used in a set operation. It also makes the .tree
 - test files much stricter, since they'll change if any list element changes.
 -
 - TODO switch to md5sum/hashContent?
 - TODO does it need to handle a race condition when writing to the cache?
 - TODO any reason to keep original extensions instead of all using .txt?
 -      oh, if we're testing extensions anywhere. lets not do that though
 -
 - TODO move to new Actions module
 -}
writeDeduped :: Show a => CutConfig
             -> (CutConfig -> FilePath -> a -> Action ())
             -> FilePath -> a -> Action ()
writeDeduped cfg writeFn outPath content = do
  let cDir     = fromCutPath cfg $ cacheDir cfg "list" -- TODO make relative to expr
      cache    = cDir </> digest content <.> "txt"
      cache'   = toCutPath cfg cache
      out'     = toCutPath cfg outPath
      -- cacheRel = ".." </> ".." </> makeRelative (cfgTmpDir cfg) cache
  liftIO $ createDirectoryIfMissing True cDir
  done1 <- doesFileExist cache
  done2 <- doesFileExist outPath
  -- when (not done1) (writeFn cfg cache content >> debugTrackWrite cfg [cache])
  -- when (not done2) (symlink cfg out' cache' >> debugTrackWrite cfg [outPath])
  when (not done1) (writeFn cfg cache content)
  when (not done2) (symlink cfg out' cache')
