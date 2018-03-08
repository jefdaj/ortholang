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
  , writeCachedLines
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

import Prelude hiding (readFile)
import Development.Shake
import ShortCut.Core.Types

import Data.List                  (sort, nub)
import Data.List.Split            (splitOneOf)
import Development.Shake.FilePath ((</>), isAbsolute, pathSeparators, makeRelative)
import ShortCut.Core.Debug        (debug)
import ShortCut.Core.Paths        (CutPath, toCutPath, fromCutPath, checkLits,
                                   cacheDir, cutPathString, stringCutPath)
import ShortCut.Core.Util         (digest, digestLength, rmAll,
                                   ignoreExistsError, digest, globFiles)
import ShortCut.Core.Locks        (withReadLock, withReadLock', withReadLocks',
                                   withWriteLock', withWriteOnce)
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))
import System.FilePath            ((<.>), takeDirectory)
-- import System.IO                  (IOMode(..), withFile)
import System.IO.Strict           (readFile)
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
readFileStrict :: Locks -> FilePath -> Action String
readFileStrict ref path = do
  need [path]
  -- withReadLock' ref path $ liftIO $ readFile path -- this is a strict readFile
  liftIO $ withReadLock ref path $ readFile path -- this is a strict readFile
-- {-# INLINE readFileStrict #-}

-- TODO something safer than head!
-- TODO error if they contain $TMPDIR or $WORKDIR?
readLit :: CutConfig -> Locks -> FilePath -> Action String
readLit cfg ref path = readLits cfg ref path >>= return . head

readLits :: CutConfig -> Locks -> FilePath -> Action [String]
readLits cfg ref path = debugReadLines cfg ref path >>= return . checkLits

-- TODO something safer than head!
readPath :: CutConfig -> Locks -> FilePath -> Action CutPath
readPath cfg ref path = readPaths cfg ref path >>= return . head

readPaths :: CutConfig -> Locks -> FilePath -> Action [CutPath]
readPaths cfg ref path = (fmap . map) stringCutPath (debugReadLines cfg ref path)

-- read a file as lines, convert to absolute paths, then parse those as cutpaths
-- used by the load_* functions to convert user-friendly relative paths to absolute
readLitPaths :: CutConfig -> Locks -> FilePath -> Action [CutPath]
readLitPaths cfg ref path = do
  ls <- debugReadLines cfg ref path
  return $ map (toCutPath cfg . toAbs) ls
  where
    toAbs line = if isAbsolute line
                   then line
                   else cfgWorkDir cfg </> line

-- TODO something safer than head!
readString :: CutType -> CutConfig -> Locks -> FilePath -> Action String
readString etype cfg ref path = readStrings etype cfg ref path >>= return . head

{- Read a "list of whatever". Mostly for generic set operations. You include
 - the CutType (of each element, not the list!) so it knows how to convert from
 - String, and then within the function you treat them as Strings.
 -}
readStrings :: CutType -> CutConfig -> Locks -> FilePath -> Action [String]
readStrings etype cfg ref path = if etype' `elem` [str, num]
  then readLits cfg ref path
  else (fmap . map) (fromCutPath cfg) (readPaths cfg ref path)
  where
    etype' = debug cfg ("readStrings (each "
          ++ extOf etype ++ ") from " ++ path) etype

debugReadFile :: CutConfig -> Locks -> FilePath -> Action String
debugReadFile cfg ref f = debug cfg ("read '" ++ f ++ "'") (readFileStrict ref f)

debugReadLines :: CutConfig -> Locks -> FilePath -> Action [String]
debugReadLines cfg ref f = debug cfg ("read: " ++ f) 
                         $ (fmap lines . readFileStrict ref) f

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
writeCachedLines :: CutConfig -> Locks -> FilePath -> [String] -> Action ()
writeCachedLines cfg ref outPath content = do
  let cDir  = fromCutPath cfg $ cacheDir cfg "lines" -- TODO make relative to expr
      cache = cDir </> digest content <.> "txt"
      -- lock  = cache <.> "lock"
  liftIO $ createDirectoryIfMissing True cDir
  withWriteOnce ref cache
    $ debug cfg ("writing '" ++ cache ++ "'")
    $ writeFile' cache (unlines content) -- TODO is this strict?
  symlink cfg ref (toCutPath cfg outPath) (toCutPath cfg cache)

-- TODO take a CutPath for the out file too
-- TODO take Path Abs File and convert them... or Path Rel File?
writePaths :: CutConfig -> Locks -> FilePath -> [CutPath] -> Action ()
writePaths cfg ref out cpaths = writeCachedLines cfg ref out paths >> trackWrite paths
  where
    paths = map cutPathString cpaths

writePath :: CutConfig -> Locks -> FilePath -> CutPath -> Action ()
writePath cfg ref out path = writePaths cfg ref out [path]

-- TODO error if they contain $TMPDIR or $WORKDIR?
writeLits :: CutConfig -> Locks -> FilePath -> [String] -> Action ()
writeLits cfg ref path lits = writeCachedLines cfg ref path $ checkLits lits

writeLit :: CutConfig -> Locks -> FilePath -> String -> Action ()
writeLit cfg ref path lit = writeLits cfg ref path [lit]

{- Write a "list of whatever". Mostly for generic set operations. You include
 - the CutType (of each element, not the list!) so it knows how to convert
 - to/from String, and then within the function you treat them as Strings.
 -}
writeStrings :: CutType -> CutConfig -> Locks
             -> FilePath -> [String] -> Action ()
writeStrings etype cfg ref out whatevers = if etype' `elem` [str, num]
  then writeLits cfg ref out whatevers
  else writePaths cfg ref out $ map (toCutPath cfg) whatevers
  where
    etype' = debug cfg ("writeStrings (each " ++ extOf etype ++ "): " ++ show (take 3 whatevers)) etype

writeString :: CutType -> CutConfig -> Locks
            -> FilePath -> String -> Action ()
writeString etype cfg ref out whatever = writeStrings etype cfg ref out [whatever]

{- Turns out there's a race condition during `repeat` calls, because the same
 - literals are being compiled in each thread at roughly the same time. The way
 - I solved it was 1) check if the file as written already, and 2) if there's a
 - conflict in the middle of the operation anyway, ignore the error. Whichever
 - thread got there first will be writing the same exact text anyway.
 -}

-- TODO rename like myReadFile, myReadLines?
debugTrackWrite :: CutConfig -> [FilePath] -> Action ()
debugTrackWrite cfg fs = debug cfg ("write " ++ show fs) (trackWrite fs)

-------------------------
-- run system commands --
-------------------------

-- TODO is the a type a good way to do this? it never actually gets evaluated
wrappedCmdError :: String -> Int -> [String] -> Action a
wrappedCmdError bin n files = do
  let files' = sort $ nub files
  liftIO $ rmAll files' -- TODO should these be patterns to match first?
  error $ unlines $
    [ "Oh no! " ++ bin ++ " failed with error code " ++ show n ++ "."
    , "The files it was working on have been deleted:"
    ] ++ files'

-- TODO call this when exiting nonzero and/or exception thrown
-- TODO take a list of globs and resolve them to files
-- TODO delete the files, telling shake if possible
-- TODO print a message for the user
-- TODO raise/re-raise an exception

-- Shake's command_ adapted to work with wrapperScript and wrapperLimit if used.
-- ptns is a list of patterns for files to delete in case the cmd fails.
-- TODO gather shake stuff into a Shake.hs module?
--      could have config, debug, wrappedCmd, eval...
-- TODO separate wrappedReadCmd with a shared lock?
wrappedCmd :: CutConfig -> Locks -> FilePath -> [String]
           -> [CmdOption] -> String -> [String]
           -> Action (String, String, Int)
wrappedCmd cfg ref outPath inPtns opts bin args = do
  inPaths <- fmap concat $ liftIO $ mapM globFiles inPtns
  liftIO $ createDirectoryIfMissing True $ takeDirectory outPath
  withWriteLock' ref outPath $ withReadLocks' ref inPaths $ do
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
wrappedCmdExit :: CutConfig -> Locks -> FilePath -> [String]
               -> [CmdOption] -> FilePath -> [String] -> Action Int
wrappedCmdExit c r p inPtns os b as = do
  (_, _, code) <- wrappedCmd c r p inPtns os b as
  return code

{- wrappedCmd specialized for commands that write one or more files. If the
 - command succeeds it tells Shake which files were written, and if it fails
 - they get deleted.
 - TODO skip the command if the files already exist?
 - TODO should outPaths be outPatterns??
 -}
wrappedCmdWrite :: CutConfig -> Locks -> FilePath -> [String] -> [FilePath]
                -> [CmdOption] -> FilePath -> [String] -> Action ()
wrappedCmdWrite cfg ref lockPath inPtns outPaths opts bin args = do
  code <- wrappedCmdExit cfg ref lockPath inPtns opts bin args
  case code of
    0 -> debugTrackWrite cfg outPaths
    n -> wrappedCmdError bin n (outPaths ++ [lockPath])

{- wrappedCmd specialized for commands that return their output as a string.
 - TODO remove this? it's only used to get columns from blast hit tables
 -}
wrappedCmdOut :: CutConfig -> Locks -> FilePath -> [String]
              -> [String] -> [CmdOption] -> FilePath
              -> [String] -> Action String
wrappedCmdOut cfg ref outLock inPtns outPaths os b as = do
  (out, err, code) <- wrappedCmd cfg ref outLock inPtns os b as
  case code of
    0 -> return out
    n -> do
      liftIO $ putStrLn $ unlines [out, err]
      wrappedCmdError b n (outPaths ++ [outLock])

----------
-- misc --
----------

digestFile :: CutConfig -> Locks -> FilePath -> Action String
digestFile cfg ref path = debugReadFile cfg ref path >>= return . digest

hashContent :: CutConfig -> Locks -> CutPath -> Action String
hashContent cfg ref path = do
  need [path']
  Stdout out <- withReadLock' ref path' $ command [] "md5sum" [path']
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
symlink :: CutConfig -> Locks -> CutPath -> CutPath -> Action ()
symlink cfg ref src dst = withWriteOnce ref src' $ do
  liftIO $ createDirectoryIfMissing True $ takeDirectory src'
  liftIO $ ignoreExistsError $ createSymbolicLink dstr src'
  debugTrackWrite cfg [src']
  where
    src' = fromCutPath cfg src
    dst' = fromCutPath cfg dst
    dstr = tmpLink cfg src' dst' -- TODO use cutpaths here too?
