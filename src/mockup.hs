module Main where

import Control.Monad (when)
import System.Directory (doesFileExist)

import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map

import Data.IORef (IORef, newIORef, atomicModifyIORef)

import Control.Exception.Base (bracket_)

import           Control.Concurrent.Lock   (Lock)
import qualified Control.Concurrent.Lock as Lock

import Control.Concurrent.ParallelIO.Global

type Cache = Map FilePath Lock

getLock :: FilePath -> IORef Cache -> IO Lock
getLock path ref = do
  l <- Lock.new -- if one exists, this will be ignored
  atomicModifyIORef ref $ \c -> case Map.lookup path c of
    Nothing -> (Map.insert path l c, l)
    Just l' -> (c, l')

-- client code just needs to:
-- 1. do a global newIORef to start the cache
-- 2. write anything that writes files in this
withLock :: IORef Cache -> FilePath -> IO a -> IO a
withLock ref path action = do
  l <- getLock path ref
  bracket_ (Lock.acquire l) (Lock.release l) action

-- same as withLock, except skips when the file has been written already
writeOnce :: IORef Cache -> FilePath -> IO () -> IO ()
writeOnce ref path action = withLock ref path $ do
  exists <- doesFileExist path
  when (not exists) action

testWrite :: FilePath -> Int -> IO ()
testWrite path n = appendFile path $ "test written by thread " ++ show n ++ "\n"

main :: IO ()
main = do
  ref <- newIORef Map.empty
  -- test writing 1000 files with 100 threads each at the same time
  let behavior = writeOnce -- switch to writeOnce and only thread 1 writes!
      writes p = map (\n -> behavior ref p $ testWrite p n) [1..100]
      writes2  = concat $ map (\n -> writes $ "/tmp/test" ++ show (n :: Int) ++ ".txt") [1..1000]
  parallel_ writes2
  stopGlobalPool
