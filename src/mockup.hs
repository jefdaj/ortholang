module Main where

import Control.Monad (when)
import System.Directory (doesFileExist)

import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map

import Data.IORef (IORef, newIORef, atomicModifyIORef)

import           Control.Concurrent.ReadWriteLock   (RWLock)
import qualified Control.Concurrent.ReadWriteLock as RWLock

import Control.Concurrent.ParallelIO.Global

type Cache = Map FilePath RWLock

getLock :: FilePath -> IORef Cache -> IO RWLock
getLock path ref = do
  l <- RWLock.new -- if one exists, this will be ignored
  atomicModifyIORef ref $ \c -> case Map.lookup path c of
    Nothing -> (Map.insert path l c, l)
    Just l' -> (c, l')

-- client code just needs to:
-- 1. do a global newIORef to start the cache
-- 2. write anything that writes files in this
withWriteLock :: IORef Cache -> FilePath -> IO a -> IO a
withWriteLock ref path action = do
  l <- getLock path ref
  RWLock.withRead l action

-- same as withWriteLock, except skips when the file has been written already
writeOnce :: IORef Cache -> FilePath -> IO () -> IO ()
writeOnce ref path action = withWriteLock ref path $ do
  exists <- doesFileExist path
  when (not exists) action

withReadLock :: IORef Cache -> FilePath -> IO a -> IO a
withReadLock ref path action = do
  l <- getLock path ref
  RWLock.withRead l action

testWrite :: FilePath -> Int -> IO ()
testWrite path n = appendFile path $ "test written by thread " ++ show n ++ "\n"

testRead :: FilePath -> Int -> IO ()
testRead path n = do
  txt <- readFile path
  let line = last $ lines txt
  putStrLn $ "last line of " ++ path ++ " read by thread " ++ show n ++ ": " ++ line

main :: IO ()
main = do
  ref <- newIORef Map.empty
  -- test reading + writing 100 files with 100 threads each at the same time
  -- TODO randomize the order?
  let behavior = withWriteLock -- switch to writeOnce and only thread 1 writes!
      ws   p = map (\n -> behavior ref p $ testWrite p n) [1..100]
      rs   p = map (\n -> testRead p n) [1..100]
      wrs  p = ws p ++ rs p
      name n = "/tmp/test" ++ show (n :: Int) ++ ".txt"
      wr2    = concat $ map (\n -> wrs $ name n) [1..100]
  parallel_ wr2
  stopGlobalPool
