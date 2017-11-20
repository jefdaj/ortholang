{- Some Shake Actions wrapped with ShortCut-specific additions,
 - and some other stuff. Eventually, it would be nice if all IO happened here.
 -
 - TODO rename to be shorter (no "safe", "wrapped", etc)
 -}

module ShortCut.Core.Actions
  ( wrappedCmd
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
  , withErrorHandling
  , unlessExists
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
  )
  where

import Development.Shake
import ShortCut.Core.Types
import Development.Shake.FilePath ((</>), isAbsolute, pathSeparators,
                                   makeRelative)

import qualified Control.Monad.TaggedException as TE

import Data.Default.Class (Default(def))
import System.Exit        (ExitCode(..))
import System.FilePath    (takeDirectory, takeFileName, (<.>))
import System.IO.LockFile (withLockFile)
import System.IO         (IOMode(..), withFile)
import Control.Monad     (unless, when)
import Control.Exception (catch, throwIO)
import System.Directory  (removeFile)
import System.IO.Error   (isAlreadyInUseError, isAlreadyExistsError,
                          isDoesNotExistError, ioError)
import System.IO.Strict  (hGetContents)
import ShortCut.Core.Debug (debug)
import ShortCut.Core.Paths (CutPath, toCutPath, fromCutPath, checkLits,
                            cacheDir, cutPathString, stringCutPath)
import ShortCut.Core.Util  (digest, digestLength)
import Data.List.Split            (splitOneOf)
import System.Directory           (createDirectoryIfMissing)
import System.Posix.Files         (createSymbolicLink)

---------
-- cmd --
---------

listPrefixFiles :: FilePattern -> Action [FilePath]
listPrefixFiles prefix = do
  let pDir  = takeDirectory prefix
      pName = takeFileName  prefix
  e1 <- doesDirectoryExist pDir
  if e1
    then getDirectoryFiles pDir [pName] >>= return . map (pDir </>)
    else return []

rmPrefixFiles :: FilePattern -> Action ()
rmPrefixFiles ptn = do
  files <- listPrefixFiles ptn
  liftIO $ putStrLn $ "deleting " ++ show files ++ " (pattern '" ++ show ptn ++ "')"
  mapM_ rmFile files
  where
    rmFile p = liftIO $ removeFiles (takeDirectory p) [takeFileName p]

-- TODO any particular corner cases to be aware of? (what if inturrupted?)
withLock :: CutConfig -> FilePath -> IO a -> IO a
withLock _ path act = withErr $ withLockFile def (path <.> "lock") act
  where
    withErr = TE.handle $ fail . ("Locking failed with: " ++) . show

wrappedCmdError :: String -> Int -> [String] -> Action a
wrappedCmdError bin n ptns = do
  -- toDel <- globs dir ptns -- TODO any better dir? absolute?
  -- liftIO $ removeFiles dir ptns
  -- liftIO $ mapM_ (\p -> removeFiles (takeDirectory p) [takeFileName p]) ptns
  mapM_ rmPrefixFiles ptns
  error $ unlines $
    [ "Oh no! " ++ bin ++ " failed with error code " ++ show n ++ "."
    , "The files it was working on have been deleted:"
    ] ++ ptns

-- TODO call this when exiting nonzero and/or exception thrown
-- TODO take a list of globs and resolve them to files
-- TODO delete the files, telling shake if possible
-- TODO print a message for the user
-- TODO raise/re-raise an exception

-- Shake's command_ adapted to work with wrapperScript and wrapperLimit if used
-- TODO gather shake stuff into a Shake.hs module?
--      could have config, debug, wrappedCmd, eval...
-- ptns is a list of patterns for files to delete in case the cmd fails
-- TODO any way to propogate Shake cmd's cool stdout, stderr, exit feature?
wrappedCmd' :: CutConfig
            -> [CmdOption] -> FilePath -> [String]
            -> Action (String, ExitCode)
wrappedCmd' cfg opts bin args = do
  let fn = case cfgWrapper cfg of
             Nothing -> command opts bin args
             Just w  -> command opts w (bin:args)
  (Stdouterr out, Exit code) <- fn
  return (out, code)

wrappedCmd :: CutConfig -> [String]
           -> [CmdOption] -> FilePath -> [String]
           -> Action ()
wrappedCmd c ps os b as = do
  -- TODO would this help anything?
  -- liftIO $ mapM_ (createDirectoryIfMissing True . takeDirectory) ps
  (_, code) <- wrappedCmd' c os b as
  case code of
    ExitFailure n -> wrappedCmdError b n ps
    ExitSuccess   -> return ()

wrappedCmdOut :: CutConfig -> [String]
              -> [CmdOption] -> FilePath -> [String]
              -> Action String
wrappedCmdOut c ps os b as = do
  (out, code) <- wrappedCmd' c os b as
  case code of
    ExitFailure n -> liftIO (putStrLn out) >> wrappedCmdError b n ps
    ExitSuccess   -> return out

-- Note that this one doesn't have wrappedCmdError,
-- because it's used for when you expect a nonzero exit code.
-- TODO write some other error checking to go along with it!
wrappedCmdExit :: CutConfig
               -> [CmdOption] -> FilePath -> [String]
               -> Action ExitCode
wrappedCmdExit c os b as = wrappedCmd' c os b as >>= return . snd

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

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

unlessExists :: FilePath -> Action () -> Action ()
unlessExists path act = do
  e <- doesFileExist path
  unless e act

-- This is safe in two ways:
-- 1. It skips writing if the file is already being written by another thread
-- 2. If some other error occurs it deletes the file, which is important
--    because it prevents it being interpreted as an empty list later
withErrorHandling :: FilePath -> IO () -> IO ()
withErrorHandling path fn = catch fn handler
  where
    handler e = if      isAlreadyInUseError  e then return ()
                else if isAlreadyExistsError e then return ()
                else    removeIfExists path >> ioError e

-- TODO debugTrackWrite after?
writeFileSafe :: FilePath -> String -> Action ()
writeFileSafe path x = liftIO $ withErrorHandling path $ writeFile path x

-- TODO debugTrackWrite after?
writeLinesSafe :: FilePath -> [String] -> Action ()
writeLinesSafe path = writeFileSafe path . unlines

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

-- TODO track written in these!
-- TODO remote in favor of only the Paths version?
debugWriteLines :: CutConfig -> FilePath -> [String] -> Action ()
debugWriteLines cfg f ss = unlessExists f
                         $ debug cfg ("write '" ++ f ++ "'")
                         $ writeLinesSafe f ss

debugWriteChanged :: CutConfig -> FilePath -> String -> Action ()
debugWriteChanged cfg f s = unlessExists f
                          $ debug cfg ("write '" ++ f ++ "'")
                          $ writeFileSafe f s

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
  out <- wrappedCmdOut cfg [] [] "md5sum" [path']
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
symlink cfg src dst = do
  need [dst'] -- TODO wrapper that uses cutpaths
  liftIO $ do
    createDirectoryIfMissing True $ takeDirectory dst'
    withErrorHandling src' $ createSymbolicLink dstr src' -- TODO handle dups!
  debugTrackWrite cfg [src'] -- TODO wrapper that uses cutpaths
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
  when (not done1) (writeFn cfg cache content)
  when (not done2) (symlink cfg out' cache')
