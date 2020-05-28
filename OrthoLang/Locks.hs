-- TODO remove any unneccessary locks that aren't withWriteOnce* from the other modules!
-- TODO wait do that, but also in a separate branch rewrite the locking simpler with whole expr dirs at once!

module OrthoLang.Locks
  ( Locks
  , LocksRef
  , initLocks
  , withReadLock
  , withReadLock'
  , withReadLocks'
  , withWriteLock
  , withWriteLockEmpty
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

import Prelude hiding (log, error)
import OrthoLang.Debug

import qualified Data.Map.Strict                  as Map
import qualified Control.Concurrent.ReadWriteLock as RWLock

import Development.Shake hiding (doesFileExist, doesDirectoryExist)
import Control.Concurrent.ReadWriteLock (RWLock)
import Control.Monad                    (when)
import Data.List                        (nub)
import Data.Maybe                       (fromJust)
import Data.IORef                       (IORef, newIORef, atomicModifyIORef')
import Data.Map.Strict                  (Map)
import Control.Exception.Safe     (bracket_)
import System.Directory           (createDirectoryIfMissing, doesFileExist,
                                   doesDirectoryExist, pathIsSymbolicLink)
import System.FilePath            (takeDirectory)
import System.Posix.Files         (setFileMode)
import Control.Exception.Safe     (catch, throwM)
import System.IO.Error            (isDoesNotExistError)
import Text.Regex.Posix           ((=~))

-- import qualified Control.Logging as L
-- import qualified Data.Text as T

-- import Data.IORef (readIORef)

-- import Control.Concurrent.Thread.Delay (delay)

-- TODO move these to Types?

-- TODO parametarize FilePath and re-export with OrthoLangPath in Types.hs?
-- type Locks = (Resource, IORef (Map FilePath (RWLock, FileProgress)))
type Locks    = Map FilePath (RWLock, FileProgress)

type LocksRef = IORef (Resource, Resource, Locks) -- TODO put in a Locks record

-- TODO should there also be Failed?
data FileProgress = ReadOnly | Success Int | Attempt Int
  deriving (Read, Show, Eq)

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
  rDisk <- newResourceIO "disk" 1000
  rPar  <- newResourceIO "parallel" 8 -- TODO set to number of nodes
  lRef  <- newIORef (rDisk, rPar, Map.empty)
  return lRef

getReadLock :: LocksRef -> FilePath -> IO RWLock
getReadLock lRef path = do
  l <- RWLock.new -- TODO how to avoid creating extra locks here?
  debugLock $ "getReadLock getting lock for '" ++ path ++ "'"
  atomicModifyIORef' lRef $ \(d,p,c) -> case Map.lookup path c of
    Nothing -> ((d,p,Map.insert path (l, ReadOnly) c), l) -- TODO error here too sometimes?
    Just (l', Attempt n) ->
      trace "locks.getReadLock"
        ("Attempt to read file not written successfully yet: '" ++ path ++ "'")
        ((d,p,Map.insert path (l', Attempt (n+1)) c), l')
    Just (l', ReadOnly ) -> ((d,p,c), l')
    Just (l', Success _) -> ((d,p,c), l')

getWriteLock :: LocksRef -> FilePath -> IO RWLock
getWriteLock lRef path = do
  l <- RWLock.new -- TODO how to avoid creating extra locks here?
  debugLock $ "getReadLock getting lock for '" ++ path ++ "'"
  atomicModifyIORef' lRef $ \(d,p,c) -> case Map.lookup path c of
    Nothing -> ((d,p,Map.insert path (l, Attempt 1) c), l)
    -- TODO hey should this be l' in the insert???
    -- Just (l', Success n) -> if whitelisted path
    --                           then (Map.insert path (l, Success (n+1)) c, l')
    --                           else warn "getWriteLock" $ "Attempt to re-write successful file: '" ++ path ++ "'"
    Just (l', Success n) -> ((d,p,Map.insert path (l', Attempt (n+1)) c), l')
    Just (l', Attempt n) -> ((d,p,Map.insert path (l', Attempt (n+1)) c), l')
    Just (l', ReadOnly ) -> trace "getWriteLock"
                              ("Attempt to write read-only file: '" ++ path ++ "'")
                              ((d,p,c), l')

-- TODO milder error that doesn't crash here
markDone :: LocksRef -> FilePath -> IO ()
markDone lRef path = do
  debugLock $ "markDone '" ++ path ++ "'"
  atomicModifyIORef' lRef $ \(d,p,c) -> case Map.lookup path c of
    Nothing -> trace "markDone"
                 ("markDone called on nonexistent lock path '" ++ path ++ "'")
                 ((d,p,c), ())
    Just (_, ReadOnly ) -> trace "markDone"
                             ("markDone called on read-only lock path '" ++ path ++ "'")
                             ((d,p,c), ())
    Just (l, Success n) -> if whitelisted path
                             then ((d,p,Map.insert path (l, Success (n+1)) c), ())
                             else trace "markDone"
                                    ("markDone called on already-finished lock path '" ++ path ++ "'")
                                    ((d,p,Map.insert path (l, Success (n+1)) c), ())
    Just (l, Attempt n) -> ((d,p,Map.insert path (l, Success (n+1)) c), ())

-- describes some paths we don't want to see duplicate write errors for
whitelisted :: FilePath -> Bool
whitelisted path = any (\p -> path =~ p) regexes
  where
    regexes =
      [

      -- test files, which legitimately can be locked multiple times
        "\\.show$"

      -- mistakes which should be removed when possible
      -- TODO fix these
      , ".*load_.*"
      , ".*cache\\/lines.*"

      ]

withMarkDone :: LocksRef -> [FilePath] -> IO a -> IO a
withMarkDone ref paths act = do
  res <- act
  mapM_ (markDone ref) paths
  return res

-- TODO use MonadIO or something to unify these?
withMarkDone' :: LocksRef -> [FilePath] -> Action a -> Action a
withMarkDone' ref paths act = do
  res <- act
  liftIO $ mapM_ (markDone ref) paths
  return res

withReadLock :: LocksRef -> FilePath -> IO a -> IO a
withReadLock ref path ioFn = do -- TODO IO issue here?
  l <- liftIO $ getReadLock ref path
  bracket_
    (debugLock ("withReadLock acquiring \"" ++ path ++ "\"") >> RWLock.acquireRead l)
    (debugLock ("withReadLock releasing \"" ++ path ++ "\"") >> RWLock.releaseRead l)
    ioFn

withReadLock' :: FilePath -> Action a -> Action a
withReadLock' path actFn = do
  ref <- fmap fromJust getShakeExtra
  l <- liftIO $ getReadLock ref path
  debugLock' $ "withReadLock' acquiring '" ++ path ++ "'"
  (liftIO (RWLock.acquireRead l) >> actFn)
    `actionFinally`
    (debugLock ("withReadLock' releasing \"" ++ path ++ "\"") >> RWLock.releaseRead l)

withReadLocks' :: [FilePath] -> Action a -> Action a
withReadLocks' paths actFn = do
  ref <- fmap fromJust getShakeExtra
  locks <- liftIO $ mapM (getReadLock ref) (nub paths)
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
  when empty $ error "assertNonEmptyFile" $ "script wrote actual empty file: \"" ++ path ++ "\""

withWriteLock :: LocksRef -> FilePath -> IO a -> IO a
withWriteLock ref path ioFn = do
  res <- withWriteLockEmpty ref path ioFn
  assertNonEmptyFile ref path
  return res

{-|
Usually we want to make sure the script didn't write an empty file,
but sometimes a lock is just a lock. Then you want this function instead.
-}
withWriteLockEmpty :: LocksRef -> FilePath -> IO a -> IO a
withWriteLockEmpty ref path ioFn = do
  createDirectoryIfMissing True $ takeDirectory path
  l <- liftIO $ getWriteLock ref path
  res <- bracket_
    (debugLock ("withWriteLock acquiring '" ++ path ++ "'") >> RWLock.acquireWrite l)
    (debugLock ("withWriteLock releasing '" ++ path ++ "'") >> RWLock.releaseWrite l)
    (withMarkDone ref [path] ioFn)
  return res

withWriteLocks' :: [FilePath] -> Action a -> Action a
withWriteLocks' paths actFn = do
  liftIO $ mapM_ (\p -> createDirectoryIfMissing True $ takeDirectory p) paths
  ref <- fmap fromJust getShakeExtra
  locks <- liftIO $ mapM (getWriteLock ref) (nub paths)
  debugLock' $ "withWriteLocks' acquiring " ++ show paths
  (liftIO (mapM_ RWLock.acquireWrite locks) >> withMarkDone' ref (nub paths) actFn)
    `actionFinally`
    (debugLock ("withWriteLocks' releasing " ++ show paths) >> mapM_ RWLock.releaseWrite locks)

withWriteLock' :: FilePath -> Action a -> Action a
withWriteLock' path actFn = do
  liftIO $ createDirectoryIfMissing True $ takeDirectory path
  debugLock' $ "withWriteLock' acquiring '" ++ path ++ "'"
  ref <- fmap fromJust getShakeExtra
  l <- liftIO $ getWriteLock ref path
  (liftIO (RWLock.acquireWrite l) >> withMarkDone' ref [path] actFn)
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
 - TODO rewrite to return immediately after locking if the FileProgress is Success
 -      ... which means getting the lock itself shouldn't throw an error right?
 -      or should this be removed entirely for runCmd and assume Shake knows what it's doing instead?
 -}
withWriteOnce :: FilePath -> Action () -> Action ()
withWriteOnce path actFn = withWriteLock' path $ do
  -- liftIO $ delay 500000 -- 1/2 second
  fBefore <- liftIO $ doesFileExist path      -- Do not use Shake's version here
  dBefore <- liftIO $ doesDirectoryExist path -- Do not use Shake's version here
  let before = fBefore || dBefore
  when (not before) actFn

  fAfter <- liftIO $ doesFileExist path      -- Do not use Shake's version here
  dAfter <- liftIO $ doesDirectoryExist path -- Do not use Shake's version here
  let after = fAfter || dAfter

  when fAfter $ liftIO $ catch (do
    isLink <- liftIO $ pathIsSymbolicLink path
    when (fAfter && not isLink)
      (setFileMode path 444)) handleExists -- TODO resolve symlinks without cfg?

  when (after && not before) $ trackWrite [path] -- TODO only for files?

  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwM e
