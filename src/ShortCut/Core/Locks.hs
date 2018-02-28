module ShortCut.Core.Locks
  ( CutLocks
  , initLocks
  , withReadLock
  , withWriteLock
  , withWriteOnce
  )
  where

{- As far as I can tell there's no way in Shake to name cache files by their
 - content hashes, because you can't know the filenames beforehand. So I
 - started "cheating" by keeping my own cache that Shake doesn't know about.
 - Which means I also need my own supplemental read/write locks to prevent
 - conflicts in those files. Use initLocks to create it at the top level of the
 - program and withReadLock or withWriteOnce when reading and writing files
 - respectively. It should handle the rest.
 -}

import ShortCut.Core.Types

import qualified Data.Map.Strict                  as Map
import qualified Control.Concurrent.ReadWriteLock as RWLock

import Control.Concurrent.ReadWriteLock (RWLock)
import Control.Monad                    (when)
import Data.IORef                       (IORef, newIORef, atomicModifyIORef)
import Data.Map.Strict                  (Map)
import ShortCut.Core.Paths              (CutPath, fromCutPath)
import System.Directory                 (doesFileExist)

type CutLocks = Map CutPath RWLock

initLocks :: IO (IORef CutLocks)
initLocks = newIORef Map.empty

getLock :: CutPath -> IORef CutLocks -> IO RWLock
getLock path ref = do
  l <- RWLock.new -- TODO how to avoid creating extra locks here?
  atomicModifyIORef ref $ \c -> case Map.lookup path c of
    Nothing -> (Map.insert path l c, l)
    Just l' -> (c, l')

withReadLock :: IORef CutLocks -> CutPath -> IO a -> IO a
withReadLock ref path action = do
  l <- getLock path ref
  RWLock.withRead l action

withWriteLock :: IORef CutLocks -> CutPath -> IO a -> IO a
withWriteLock ref path action = do
  l <- getLock path ref
  RWLock.withRead l action

withWriteOnce :: CutConfig -> IORef CutLocks -> CutPath -> IO () -> IO ()
withWriteOnce cfg ref path action = withWriteLock ref path $ do
  exists <- doesFileExist $ fromCutPath cfg path
  when (not exists) action
