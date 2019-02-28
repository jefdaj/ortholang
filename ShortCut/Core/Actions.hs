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

  -- debugging
  , debugNeed
  , debugTrackWrite -- TODO should this not be needed?
  , debugA
  , debugIO
  , debugL

  -- run system commands
  , wrappedCmd
  , wrappedCmdError
  , wrappedCmdExit
  , wrappedCmdOut
  , wrappedCmdWrite

  -- misc
  -- , digestFile  -- TODO what's the difference with hashContent?
  , hashContent
  , withBinHash
  , symlink
  , cachedLinesPath

  )
  where

import Prelude hiding (readList)
import Development.Shake
import ShortCut.Core.Types
import ShortCut.Core.Config (debug)

import Data.Maybe                 (maybeToList)
import Control.Monad              (when)
import Data.List                  (sort, nub)
import Data.List.Split            (splitOneOf)
import Development.Shake.FilePath ((</>), isAbsolute, pathSeparators, makeRelative)
-- import ShortCut.Core.Debug        (debug)
import ShortCut.Core.Paths        (CutPath, toCutPath, fromCutPath, checkLit,
                                   checkLits, cacheDir, cutPathString,
                                   stringCutPath)
import ShortCut.Core.Util         (digest, digestLength, rmAll, readFileStrict, unlessExists,
                                   ignoreExistsError, digest, globFiles, isEmpty)
import ShortCut.Core.Locks        (withReadLock', withReadLocks',
                                   withWriteLock', withWriteOnce)
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))
import System.FilePath            ((<.>), takeDirectory, takeExtension)
-- import System.IO                  (IOMode(..), withFile)
import System.Posix.Files         (createSymbolicLink)
import System.Posix.Escape         (escape)
-- import System.IO.Temp (emptyTempFile)
-- import Control.Concurrent.Thread.Delay (delay)
-- import Debug.Trace       (trace)

---------------
-- debugging --
---------------

debugIO :: CutConfig -> String -> IO ()
debugIO cfg msg = when (cfgDebug cfg) (putStrLn msg)

debugL :: CutConfig -> String -> Action ()
debugL cfg msg = liftIO $ debugIO cfg msg

debugA :: Show a => CutConfig -> String -> a -> [String] -> a
debugA cfg name out args = debug cfg msg out
  where
    msg = name ++ " creating " ++ show out ++ " from " ++ show args

debugNeed :: CutConfig -> String -> [FilePath] -> Action ()
debugNeed cfg fnName paths = debug cfg msg $ need paths
  where
    msg = fnName ++ " needs " ++ show paths

----------------
-- read files --
----------------

-- Action version of readFileStrict. This is used for all reads during a cut;
-- the raw one is just for showing results, reading script files etc.
readFileStrict' :: CutConfig -> Locks -> FilePath -> Action String
readFileStrict' cfg ref path = do
  debugNeed cfg "readFileStrict'" [path]
  liftIO (readFileStrict ref path)
-- {-# INLINE readFileStrict' #-}

{- ShortCut requires empty strings to contain the text <<emptystr>> so we
 - can distinguish them from the empty files that might result from a script
 - failing, and from empty lists in case of an error in the typechecker.  This
 - also gives empty lists and strings distinct hashes.
 -}
readLit :: CutConfig -> Locks -> FilePath -> Action String
readLit cfg locks path = do
  need [path] -- Note isEmpty also does this
  empty <- isEmpty locks path
  debug cfg ("read lit '" ++ path ++ "'") $
    if empty
      then return ""
      else fmap (checkLit . init) -- TODO safe? already checked if empty
         $ readFileStrict' cfg locks path

readLits :: CutConfig -> Locks -> FilePath -> Action [String]
readLits cfg ref path = readList cfg ref path >>= return . checkLits

-- TODO something safer than head!
readPath :: CutConfig -> Locks -> FilePath -> Action CutPath
readPath cfg ref path = readPaths cfg ref path >>= return . head

-- TODO should this have checkPaths?
readPaths :: CutConfig -> Locks -> FilePath -> Action [CutPath]
readPaths cfg ref path = (fmap . map) stringCutPath (readList cfg ref path)

-- read a file as lines, convert to absolute paths, then parse those as cutpaths
-- used by the load_* functions to convert user-friendly relative paths to absolute
readLitPaths :: CutConfig -> Locks -> FilePath -> Action [CutPath]
readLitPaths cfg ref path = do
  ls <- readList cfg ref path
  return $ map (toCutPath cfg . toAbs) ls
  where
    toAbs line = if isAbsolute line
                   then line
                   else cfgWorkDir cfg </> line

-- TODO something safer than head!
-- TODO how should this relate to readLit and readStr?
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

{- ShortCut requires empty lists to contain the text <<emptylist>> so we can
 - distinguish them from the empty files that might result from a script
 - failing, and from empty strings in case of an error in the typechecker.
 - This also gives empty lists and strings distinct hashes.
 -
 - Note that strict reading is important to avoid "too many open files" on long lists.
 -}
readList :: CutConfig -> Locks -> FilePath -> Action [String]
readList cfg locks path = do
  debugNeed cfg "readList" [path] -- Note isEmpty also does this
  empty <- isEmpty locks path
  if empty
    then return []
    else debug cfg ("read list '" ++ path ++ "'")
       $ fmap lines
       $ withReadLock' locks path
       $ liftIO
       $ readFileStrict locks path


-----------------
-- write files --
-----------------

cachedLinesPath :: CutConfig -> [String] -> FilePath
cachedLinesPath cfg content = cDir </> digest content <.> "txt"
  where
    cDir = fromCutPath cfg $ cacheDir cfg "lines"

{- This ensures that when two lists have the same content, their expression
 - paths will be links to the same cached path. That causes them to get
 - properly deduplicated when used in a set operation. It also makes the .tree
 - test files much stricter, since they'll change if any list element changes.
 - The Maybe is in case you only need to write lists as part of a larger list
 - and don't want individual outpaths.
 -
 - TODO switch to md5sum/hashContent?
 - TODO does it need to handle a race condition when writing to the cache?
 - TODO any reason to keep original extensions instead of all using .txt?
 -      oh, if we're testing extensions anywhere. lets not do that though
 -}
writeCachedLines :: CutConfig -> Locks -> FilePath -> [String] -> Action ()
writeCachedLines cfg ref outPath content = do
  let cache = cachedLinesPath cfg content
      -- lock  = cache <.> "lock"
  -- liftIO $ createDirectoryIfMissing True cDir
  withWriteOnce ref cache
    $ debug cfg ("writing '" ++ cache ++ "'")
    $ writeFile' cache (unlines content) -- TODO is this strict?
  unlessExists outPath $
    symlink cfg ref (toCutPath cfg outPath) (toCutPath cfg cache)

-- TODO take a CutPath for the out file too
-- TODO take Path Abs File and convert them... or Path Rel File?
-- TODO explicit case for empty lists that isn't just an empty file!
writePaths :: CutConfig -> Locks -> FilePath -> [CutPath] -> Action ()
writePaths cfg ref out cpaths = writeCachedLines cfg ref out paths >> trackWrite paths
  where
    paths = if null cpaths then ["<<emptylist>>"] else map cutPathString cpaths

writePath :: CutConfig -> Locks -> FilePath -> CutPath -> Action ()
writePath cfg ref out path = do
  debugL cfg ("writePath path: " ++ show path)
  writePaths cfg ref out [path]

writeLits :: CutConfig -> Locks -> FilePath -> [String] -> Action ()
writeLits cfg ref path lits = writeCachedLines cfg ref path lits'
  where
    lits' = if null lits then ["<<emptylist>>"] else checkLits lits

-- TODO any need to prevent writing <<emptystr>> in a .num?
--      (seems almost certain to be caught on reading later)
writeLit :: CutConfig -> Locks -> FilePath -> String -> Action ()
writeLit cfg ref path lit = do
  debugL cfg $ "writeLit lit: '" ++ lit ++ "'"
  debugL cfg $ "writeLit lit': '" ++ lit' ++ "'"
  writeLits cfg ref path [lit']
  where
    lit' = if null lit then "<<emptystr>>" else lit

{- Write a "list of whatever". Mostly for generic set operations. You include
 - the CutType (of each element, not the list!) so it knows how to convert
 - to/from String, and then within the function you treat them as Strings.
 -}
writeStrings :: CutType -> CutConfig -> Locks
             -> FilePath -> [String] -> Action ()
writeStrings etype cfg ref out whatevers = if etype' `elem` [str, num]
  then writeLits  cfg ref out whatevers
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
debugTrackWrite cfg fs = do
  -- mapM_ (assertNonEmptyFile cfg ref) fs
  debug cfg ("wrote " ++ show fs) (trackWrite fs)

-------------------------
-- run system commands --
-------------------------

-- TODO is the a type a good way to do this? it never actually gets evaluated
wrappedCmdError :: String -> Int -> [String] -> Action a
wrappedCmdError bin n files = do
  let files' = sort $ nub files
  liftIO $ rmAll files' -- TODO should these be patterns to match first?
  -- TODO does this get caught by recoverAll in eval? make sure it does!
  -- TODO also try adding a manual flush before each external command in case it's an io delay thing
  error $ unlines $
    [ "Oh no! " ++ bin ++ " failed with error code " ++ show n ++ "."
    , "The files it was working on have been deleted:"
    ] ++ files'

{- ShortCut requires explicit empty files with contents like "<<emptylist>>" to
 - distinguish them from runtime errors. This function replaces those with
 - actual empty files before passing them to a script, so logic for that
 - doesn't have to be duplicated over and over.
 -}
fixEmptyText :: CutConfig -> Locks -> FilePath -> Action FilePath
fixEmptyText cfg ref path = do
  debugNeed cfg "fixEmptyText" [path] -- Note isEmpty does this too
  empty <- isEmpty ref path
  return $ if empty then "/dev/null" else path -- TODO will /dev/null work?

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

wrappedCmd :: Bool -> Bool -> CutConfig -> Locks -> Maybe FilePath -> [String]
           -> [CmdOption] -> String -> [String]
           -> Action (String, String, Int)
wrappedCmd parCmd fixEmpties cfg ref mOut inPtns opts bin args = actionRetry 5 $ do
  -- liftIO $ delay 1000000
  inPaths  <- fmap concat $ liftIO $ mapM globFiles inPtns
  inPaths' <- if fixEmpties
                then mapM (fixEmptyText cfg ref) inPaths
                else return inPaths
  -- liftIO $ createDirectoryIfMissing True $ takeDirectory outPath
  debugL cfg $ "wrappedCmd acquiring read locks on " ++ show inPaths'
  -- debugL cfg $ "wrappedCmd cfg: " ++ show cfg
  let writeLockFn = case mOut of
                      Nothing -> id
                      Just o  -> \fn -> do
                        debugL cfg $ "wrappedCmd acquiring write lock on '" ++ o ++ "'"
                        withWriteLock' ref o fn
      parLockFn = if parCmd
                    then \f -> withResource (cfgParLock cfg) 1 $ writeLockFn f
                    else writeLockFn

  -- TODO is 5 a good number of times to retry? can there be increasing delay or something?
  parLockFn $ withReadLocks' ref inPaths' $ do
    (Stdout out, Stderr err, Exit code) <- case cfgWrapper cfg of
      Nothing -> command opts bin args
      Just w  -> command (Shell:opts) w [escape $ unwords (bin:args)]
    let code' = case code of
                  ExitSuccess   -> 0
                  ExitFailure n -> n
    -- This is disabled because it can make the logs really big
    -- debugL cfg $ "wrappedCmd: " ++ bin ++ " " ++ show args ++ " -> " ++ show (out, err, code')
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
wrappedCmdExit :: Bool -> Bool -> CutConfig -> Locks -> Maybe FilePath -> [String]
               -> [CmdOption] -> FilePath -> [String] -> [Int] -> Action Int
wrappedCmdExit parCmd fixEmpties cfg r mOut inPtns opts bin as allowedExitCodes = do
  (_, _, code) <- wrappedCmd parCmd fixEmpties cfg r mOut inPtns opts bin as
  case mOut of
    Nothing -> return ()
    Just o  -> debugTrackWrite cfg [o] -- >> assertNonEmptyFile cfg r outPath
  if code `elem` allowedExitCodes
    then return code
    else wrappedCmdError bin code $ maybeToList mOut

{- wrappedCmd specialized for commands that write one or more files. If the
 - command succeeds it tells Shake which files were written, and if it fails
 - they get deleted. Note that outPaths should *not* include the main outPath,
 - or you'll get a recursive Rules error from Shake.
 - TODO skip the command if the files already exist?
 - TODO shouloutPath:outPaths)d outPaths be outPatterns??
 - TODO if assertNonEmptyFile fails, will other outfiles be deleted?
 - TODO rename wrappedCmdMulti because it has multiple outfiles?
 -}
wrappedCmdWrite :: Bool -> Bool -> CutConfig -> Locks -> FilePath -> [String] -> [FilePath]
                -> [CmdOption] -> FilePath -> [String] -> Action ()
wrappedCmdWrite parCmd fixEmpties cfg ref outPath inPtns outPaths opts bin args = do
  debugL cfg $ "wrappedCmdWrite outPath: "  ++ outPath
  debugL cfg $ "wrappedCmdWrite inPtns: "   ++ show inPtns
  debugL cfg $ "wrappedCmdWrite outPaths: " ++ show outPaths
  debugL cfg $ "wrappedCmdWrite args: "     ++ show args
  code <- wrappedCmdExit parCmd fixEmpties cfg ref (Just outPath) inPtns opts bin args [0]
  -- TODO can this be handled in wrappedCmdExit too?
  -- liftIO $ delay 10000
  case code of
    0 -> do
      -- debugTrackWrite has to come first here, because the assert will need them!
      debugTrackWrite cfg outPaths
      -- liftIO $ delay 10000
      -- mapM_ (assertNonEmptyFile cfg ref) outPaths
    n -> wrappedCmdError bin n (outPath:outPaths)

{- wrappedCmd specialized for commands that return their output as a string.
 - TODO remove this? it's only used to get columns from blast hit tables
 -}
wrappedCmdOut :: Bool -> Bool -> CutConfig -> Locks -> [String]
              -> [String] -> [CmdOption] -> FilePath
              -> [String] -> Action String
wrappedCmdOut parCmd fixEmpties cfg ref inPtns outPaths opts bin args = do
  (out, err, code) <- wrappedCmd parCmd fixEmpties cfg ref Nothing inPtns opts bin args
  case code of
    0 -> return out
    n -> do
      debugL cfg $ unlines [out, err]
      wrappedCmdError bin n outPaths

----------
-- misc --
----------

-- This is the only function that should access readFileStrict' directly;
-- all others go through readStr and readList.
-- TODO use a CutPath here?
-- digestFile :: CutConfig -> Locks -> FilePath -> Action String
-- digestFile cfg ref path = readFileStrict' cfg ref path >>= return . digest

-- TODO fixEmpties should be False here, but don't want to break existing tmpdir just yet
hashContent :: CutConfig -> Locks -> CutPath -> Action String
hashContent cfg ref path = do
  debugNeed cfg "hashContent" [path']
  -- Stdout out <- withReadLock' ref path' $ command [] "md5sum" [path']
  out <- wrappedCmdOut False True cfg ref [path'] [] [] "md5sum" [path']
  let md5 = take digestLength $ head $ words out
  return md5
  where
    path' = fromCutPath cfg path

{- Hashing doesn't save any space here, but it puts the hashes in
 - src/tests/plots/*.tree so we can test that the plots don't change.
 -
 - What it does:
 -   1. make a random temporary path
 -   2. pass that to actFn to make the actual output
 -   3. hash the output to determine cache path
 -   4. symlink hash path -> tmp path, actual outPath -> hash path
 -
 - TODO remove if ggplot turns out to be nondeterministic
 - TODO use hash of expr + original ext instead so it looks nicer?
 -}
withBinHash :: CutConfig -> Locks -> CutExpr -> CutPath
            -> (CutPath -> Action ()) -> Action ()
withBinHash cfg ref expr outPath actFn = do
  let binDir'  = fromCutPath cfg $ cacheDir cfg "bin"
      outPath' = fromCutPath cfg outPath
  liftIO $ createDirectoryIfMissing True binDir'
  let binTmp' = binDir' </> digest expr <.> takeExtension outPath'
      binTmp  = toCutPath cfg binTmp'
  debugL cfg $ "withBinHash binTmp': " ++ show binTmp'
  _ <- actFn binTmp
  md5 <- hashContent cfg ref binTmp
  let binOut' = binDir' </> md5 <.> takeExtension outPath'
      binOut  = toCutPath cfg binOut'
  debugL cfg $ "withBinHash binOut: "  ++ show binOut
  debugL cfg $ "withBinHash binOut': " ++ show binOut'
  symlink cfg ref binOut  binTmp
  symlink cfg ref outPath binOut

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
