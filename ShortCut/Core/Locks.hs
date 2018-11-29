module ShortCut.Core.Locks
  ( Locks
  , initLocks
  , withReadLock
  , withReadLock'
  , withReadLocks'
  , withWriteLock
  , withWriteLock'
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
import Data.IORef                       (IORef, newIORef, atomicModifyIORef)
import Data.Map.Strict                  (Map)
import Control.Exception (bracket_)
import System.Directory           (createDirectoryIfMissing, doesFileExist)
import System.FilePath            (takeDirectory)
-- import Control.Concurrent.Thread.Delay (delay)

-- TODO parametarize FilePath and re-export with CutPath in Types.hs?
type Locks = IORef (Map FilePath RWLock)

{- A horrible hack to avoid import the import cycle caused by using a CutConfig
 - here. For now, just change the code to enable.
 - TODO think of something better
 -}
debugLock :: String -> IO ()
-- debugLock = putStrLn
debugLock = const $ return ()

debugLock' :: String -> Action ()
debugLock' = liftIO . debugLock

initLocks :: IO Locks
initLocks = newIORef Map.empty

getLock :: Locks -> FilePath -> IO RWLock
getLock ref path = do
  l <- RWLock.new -- TODO how to avoid creating extra locks here?
  debugLock $ "getLock getting lock for '" ++ path ++ "'"
  atomicModifyIORef ref $ \c -> case Map.lookup path c of
    Nothing -> (Map.insert path l c, l)
    Just l' -> (c, l')

withReadLock :: Locks -> FilePath -> IO a -> IO a
withReadLock ref path ioFn = do
  l <- liftIO $ getLock ref path
  bracket_
    (debugLock ("withReadLock acquiring '" ++ path ++ "'") >> RWLock.acquireRead l)
    (debugLock ("withReadLock releasing '" ++ path ++ "'") >> RWLock.releaseRead l)
    ioFn

withReadLock' :: Locks -> FilePath -> Action a -> Action a
withReadLock' ref path actFn = do
  l <- liftIO $ getLock ref path
  debugLock' $ "withReadLock' acquiring '" ++ path ++ "'"
  (liftIO (RWLock.acquireRead l) >> actFn)
    `actionFinally`
    (debugLock ("withReadLock' releasing '" ++ path ++ "'") >> RWLock.releaseRead l)

withReadLocks' :: Locks -> [FilePath] -> Action a -> Action a
withReadLocks' ref paths actFn = do
  locks <- liftIO $ mapM (getLock ref) (nub paths)
  debugLock' $ "withReadLocks' acquiring " ++ show paths
  (liftIO (mapM_ RWLock.acquireRead locks) >> actFn)
    `actionFinally`
    (debugLock ("withReadLocks' releasing " ++ show paths) >> mapM_ RWLock.releaseRead locks)

isActuallyEmpty :: Locks -> FilePath -> IO Bool
isActuallyEmpty ref path = do
  txt <- withReadLock ref path $ readFile path
  return $ null txt

{- Checks that the script actually wrote a non-empty outfile.
 - Hopefully this will catch both runtime errors and badly written scripts.
 - TODO not much of the file has to be read to test this right?
 - TODO make sure this doesn't force evaluation when it otherwise wouldn't
 -}
assertNonEmptyFile :: Locks -> FilePath -> IO ()
assertNonEmptyFile ref path = do
  -- debugL cfg $ "assertNonNull checking '" ++ path ++ "'"
  empty <- isActuallyEmpty ref path
  when empty $ error $ "script wrote actual empty file: '" ++ path ++ "'"

withWriteLock :: Locks -> FilePath -> IO a -> IO a
withWriteLock ref path ioFn = do
  createDirectoryIfMissing True $ takeDirectory path
  l <- liftIO $ getLock ref path
  res <- bracket_
    (debugLock ("withWriteLock acquiring '" ++ path ++ "'") >> RWLock.acquireWrite l)
    (debugLock ("withWriteLock releasing '" ++ path ++ "'") >> RWLock.releaseWrite l)
    ioFn
  assertNonEmptyFile ref path
  return res

withWriteLock' :: Locks -> FilePath -> Action a -> Action a
withWriteLock' ref path actFn = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory path
  debugLock' $ "withWriteLock' acquiring '" ++ path ++ "'"
  l <- liftIO $ getLock ref path
  (liftIO (RWLock.acquireWrite l) >> actFn)
    `actionFinally`
    (debugLock ("withWriteLock' releasing '" ++ path ++ "'") >> RWLock.releaseWrite l)

{- Not sure why, but there's a problem with using Shake's version of
 - doesFileExist here! It will work, but only if I add a short delay (at least
 - 1/2 second) between locking the file and writing it. Otherwise occasional
 - "file is locked" errors. Maybe this is something to do with how I "cheat" by
 - keeping track of some files outside Shake?
 -}
withWriteOnce :: Locks -> FilePath -> Action () -> Action ()
withWriteOnce ref path actFn = withWriteLock' ref path $ do
  -- liftIO $ delay 500000 -- 1/2 second
  exists <- liftIO $ doesFileExist path -- Do not use Shake's version here
  when (not exists) actFn
