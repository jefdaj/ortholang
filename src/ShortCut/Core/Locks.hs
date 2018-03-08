module ShortCut.Core.Locks
  ( Locks
  , initLocks
  , withReadLock
  , withReadLock'
  , withReadLocks'
  , withWriteLock
  , withWriteOnce
  )
  where

{- As far as I can tell there's no way in Shake to name cache files by their
 - content hashes, because you can't know the filenames beforehand. So I
 - started "cheating" by keeping my own cache that Shake doesn't know about.
 - Which means I also need my own supplemental read/write locks to prevent
 - conflicts in those files. Use initLocks to create it at the top level of the
 - program and withWriteLockor withWriteOnce when reading and writing files
 - respectively. It should handle the rest.
 -}

import qualified Data.Map.Strict                  as Map
import qualified Control.Concurrent.ReadWriteLock as RWLock

import Development.Shake -- (Action, liftIO)
import Control.Concurrent.ReadWriteLock (RWLock)
import Control.Monad                    (when)
import Data.List                        (nub)
import Data.IORef                       (IORef, newIORef, atomicModifyIORef)
import Data.Map.Strict                  (Map)
import Control.Exception (bracket_)

-- TODO parametarize FilePath and re-export with CutPath in Types.hs?
type Locks = IORef (Map FilePath RWLock)

initLocks :: IO Locks
initLocks = newIORef Map.empty

getLock :: Locks -> FilePath -> IO RWLock
getLock ref path = do
  l <- RWLock.new -- TODO how to avoid creating extra locks here?
  atomicModifyIORef ref $ \c -> case Map.lookup path c of
    Nothing -> (Map.insert path l c, l)
    Just l' -> (c, l')

withReadLock :: Locks -> FilePath -> IO a -> IO a
withReadLock ref path ioFn = do
  l <- liftIO $ getLock ref path
  bracket_
    (RWLock.acquireRead l)
    (RWLock.releaseRead l)
    ioFn

withReadLock' :: Locks -> FilePath -> Action a -> Action a
withReadLock' ref path actFn = do
  l <- liftIO $ getLock ref path
  liftIO $ RWLock.acquireRead l
  actFn `actionFinally` RWLock.releaseRead l

withReadLocks' :: Locks -> [FilePath] -> Action a -> Action a
withReadLocks' ref paths actFn = do
  locks <- liftIO $ mapM (getLock ref) (nub paths)
  liftIO $ mapM_ RWLock.acquireRead locks
  actFn `actionFinally` (mapM_ RWLock.releaseRead locks)

withWriteLock :: Locks -> FilePath -> Action a -> Action a
withWriteLock ref path actFn = do
  l <- liftIO $ getLock ref path
  liftIO $ RWLock.acquireWrite l
  actFn `actionFinally` (RWLock.releaseWrite l)

withWriteOnce :: Locks -> FilePath -> Action () -> Action ()
withWriteOnce ref path actFn = withWriteLock ref path $ do
  exists <- doesFileExist path
  when (not exists) actFn
