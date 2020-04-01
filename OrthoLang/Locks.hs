module OrthoLang.Locks
  ( LocksRef
  , initLocks
  , withReadLock
  , withReadLock'
  , withReadLocks'
  , withWriteLock
  , withWriteLock'
  , withWriteLocks'
  , withWriteOnce
  )
  where

{- As far as I can tell there's no way in Shake to name cache files by their
 - content hashes, because you can't know the filenames beforehand. So I
 - started "cheating" by keeping my own cache that Shake doesn't know about.
 - Which means I also need my own supplemental read/write locks to prevent
 - conflicts in those files. Use initLocks to create it at the top level of the
 - program and withWriteLock'or withWriteOnce when reading and writing files
 - respectively. It should handle the rest.
 -}

import qualified Data.Map.Strict                  as Map
import qualified Control.Concurrent.ReadWriteLock as RWLock

import Development.Shake hiding (doesFileExist)
import Control.Concurrent.ReadWriteLock (RWLock)
import Control.Monad                    (when)
import Data.List                        (nub)
import Data.Maybe                       (fromJust)
import Data.IORef                       (IORef, newIORef, atomicModifyIORef')
import Data.Map.Strict                  (Map)
import Control.Exception.Safe     (bracket_)
import System.Directory           (createDirectoryIfMissing, doesFileExist,
                                   pathIsSymbolicLink)
import System.FilePath            (takeDirectory)
import System.Posix.Files         (setFileMode)
import Control.Exception.Safe     (catch, throwM)
import System.IO.Error            (isDoesNotExistError)

-- import Control.Concurrent.Thread.Delay (delay)

-- TODO parametarize FilePath and re-export with Path in Types.hs?
-- TODO can this go into ActionState/GlobalEnv in any saner way?
type Locks    = Map FilePath RWLock
type LocksRef = (Resource, IORef Locks)

{- A horrible hack to avoid import the import cycle caused by using a Config
 - here. For now, just change the code to enable.
 - TODO think of something better
 -}
debugLock :: String -> IO ()
-- debugLock = putStrLn
debugLock = const $ return ()

debugLock' :: String -> Action ()
debugLock' = liftIO . debugLock

initLocks :: IO LocksRef
{-# NOINLINE initLocks #-}
initLocks = do
  -- only approximately related to the number of files open,
  -- but keeps them in check at least
  -- TODO how high is high enough to allow all sonicparanoid but not hit the OS limit?
  disk  <- newResourceIO "disk" 1000
  locks <- newIORef Map.empty
  return (disk, locks)

getLock :: LocksRef -> FilePath -> IO RWLock
getLock (_, ref) path = do
  l <- RWLock.new -- TODO how to avoid creating extra locks here?
  debugLock $ "getLock getting lock for \"" ++ path ++ "\""
  atomicModifyIORef' ref $ \c -> case Map.lookup path c of
    Nothing -> (Map.insert path l c, l)
    Just l' -> (c, l')

withReadLock :: LocksRef -> FilePath -> IO a -> IO a
withReadLock ref path ioFn = do -- TODO IO issue here?
  l <- liftIO $ getLock ref path
  bracket_
    (debugLock ("withReadLock acquiring \"" ++ path ++ "\"") >> RWLock.acquireRead l)
    (debugLock ("withReadLock releasing \"" ++ path ++ "\"") >> RWLock.releaseRead l)
    ioFn

withReadLock' :: FilePath -> Action a -> Action a
withReadLock' path actFn = do
  ref <- fmap fromJust getShakeExtra
  l <- liftIO $ getLock ref path
  debugLock' $ "withReadLock' acquiring \"" ++ path ++ "\""
  (liftIO (RWLock.acquireRead l) >> actFn)
    `actionFinally`
    (debugLock ("withReadLock' releasing \"" ++ path ++ "\"") >> RWLock.releaseRead l)

withReadLocks' :: [FilePath] -> Action a -> Action a
withReadLocks' paths actFn = do
  ref <- fmap fromJust getShakeExtra
  locks <- liftIO $ mapM (getLock ref) (nub paths)
  debugLock' $ "withReadLocks' acquiring " ++ show paths
  (liftIO (mapM_ RWLock.acquireRead locks) >> actFn)
    `actionFinally`
    (debugLock ("withReadLocks' releasing " ++ show paths) >> mapM_ RWLock.releaseRead locks)

-- TODO should this be strict too, even though just need one char?
isActuallyEmpty :: LocksRef -> FilePath -> IO Bool
isActuallyEmpty ref path = do
  txt <- withReadLock ref path $ readFile path
  return $ null txt

{- Checks that the script actually wrote a non-empty outfile.
 - Hopefully this will catch both runtime errors and badly written scripts.
 - TODO not much of the file has to be read to test this right?
 - TODO make sure this doesn't force evaluation when it otherwise wouldn't
 -}
assertNonEmptyFile :: LocksRef -> FilePath -> IO ()
assertNonEmptyFile ref path = do
  -- debugA $ "assertNonNull checking \"" ++ path ++ "\""
  empty <- isActuallyEmpty ref path
  when empty $ error $ "script wrote actual empty file: \"" ++ path ++ "\""

withWriteLock :: LocksRef -> FilePath -> IO a -> IO a
withWriteLock ref path ioFn = do
  createDirectoryIfMissing True $ takeDirectory path
  l <- liftIO $ getLock ref path
  res <- bracket_
    (debugLock ("withWriteLock acquiring \"" ++ path ++ "\"") >> RWLock.acquireWrite l)
    (debugLock ("withWriteLock releasing \"" ++ path ++ "\"") >> RWLock.releaseWrite l)
    ioFn
  assertNonEmptyFile ref path
  return res

withWriteLocks' :: [FilePath] -> Action a -> Action a
withWriteLocks' paths actFn = do
  liftIO $ mapM_ (\p -> createDirectoryIfMissing True $ takeDirectory p) paths
  ref <- fmap fromJust getShakeExtra
  locks <- liftIO $ mapM (getLock ref) (nub paths)
  debugLock' $ "withWriteLocks' acquiring " ++ show paths
  (liftIO (mapM_ RWLock.acquireWrite locks) >> actFn)
    `actionFinally`
    (debugLock ("withWriteLocks' releasing " ++ show paths) >> mapM_ RWLock.releaseWrite locks)

withWriteLock' :: FilePath -> Action a -> Action a
withWriteLock' path actFn = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory path
  debugLock' $ "withWriteLock' acquiring \"" ++ path ++ "\""
  ref <- fmap fromJust getShakeExtra
  l <- liftIO $ getLock ref path
  (liftIO (RWLock.acquireWrite l) >> actFn)
    `actionFinally`
    (debugLock ("withWriteLock' releasing \"" ++ path ++ "\"") >> RWLock.releaseWrite l)

{- Not sure why, but there's a problem with using Shake's version of
 - doesFileExist here! It will work, but only if I add a short delay (at least
 - 1/2 second) between locking the file and writing it. Otherwise occasional
 - "file is locked" errors. Maybe this is something to do with how I "cheat" by
 - keeping track of some files outside Shake?
 -
 - This also re-implements some of debugTrackWrite to avoid an import cycle.
 -
 - TODO can this be removed once newrules work?
 -}
withWriteOnce :: FilePath -> Action () -> Action ()
withWriteOnce path actFn = withWriteLock' path $ do
  -- liftIO $ delay 500000 -- 1/2 second
  exists <- liftIO $ doesFileExist path -- Do not use Shake's version here
  when (not exists) actFn
  when exists $ liftIO $ catch (do
    isLink <- liftIO $ pathIsSymbolicLink path
    when (not isLink)
      (setFileMode path 444)) handleExists -- TODO resolve symlinks without cfg?
  when exists $ trackWrite [path]
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwM e
