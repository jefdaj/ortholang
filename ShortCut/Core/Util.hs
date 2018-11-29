module ShortCut.Core.Util

  -- read files
  ( readFileStrict
  , readFileLazy

  -- hashing
  , digestLength
  , digest

  -- paths
  , ignoreExistsError
  , unlessExists
  , unlessMatch
  , removeIfExists
  , rmAll
  , resolveSymlinks
  , absolutize
  , expandTildes
  , globFiles

  -- misc
  , stripWhiteSpace
  , isEmpty
  , isReallyEmpty
  , popFrom
  , insertAt
  )
  where

import Development.Shake
import qualified System.IO.Strict as Strict

import Data.Monoid ((<>))
import Control.Monad          (unless)
import Control.Monad.Catch    (MonadCatch, catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash            (hash, Digest, MD5)
import Data.ByteString.Char8  (pack)
import Data.Char              (isSpace)
import Data.List              (dropWhileEnd, isPrefixOf, isInfixOf, splitAt)
import Data.List.Utils        (replace)
import Data.Maybe             (fromJust)
import System.Directory       (doesPathExist, removePathForcibly,
                               getHomeDirectory, makeAbsolute, removeFile,
                               pathIsSymbolicLink)
import System.FilePath        ((</>), takeDirectory, addTrailingPathSeparator,
                               normalise)
import System.IO.Error        (isDoesNotExistError, catchIOError,
                               isAlreadyExistsError, ioError)
import System.Path.NameManip  (guess_dotdot, absolute_path)
import System.Posix           (getFileStatus, fileSize)
import System.Posix.Files     (readSymbolicLink)
import System.FilePath.Glob   (glob)
-- import Data.Time.LocalTime (getZonedTime)
-- import Data.Time.Format    (formatTime, defaultTimeLocale)
import ShortCut.Core.Locks    (Locks, withReadLock, withReadLock')

---------------
-- debugging --
---------------

-- TODO put this in Util
-- TODO this works, but isn't used yet
-- getTimeStamp :: IO String
-- getTimeStamp = getZonedTime >>= return . formatTime defaultTimeLocale fmt
--   where
--     fmt = "[%Y-%m-%d %H:%M:%S %q]"

-- TODO should this go in Util too?
-- TODO remove if you can figure out a way to put stamps in regular debug
-- debugIO :: CutConfig -> String -> a -> IO a
-- debugIO cfg msg rtn = do
--   stamp <- getTimeStamp
--   return $ debug cfg (stamp ++ " " ++ msg) rtn

-- TODO ok this goes in Util
--debug :: CutConfig -> String -> a -> a
--debug cfg msg rtn = if cfgDebug cfg then trace msg rtn else rtn

-- TODO and this one 
-- TODO stop exporting this in favor of the ones below?
-- debugShow :: Show a => CutConfig -> a -> b -> b
-- debugShow cfg shw rtn = if cfgDebug cfg then traceShow shw rtn else rtn

----------------
-- read files --
----------------

{- Lazy IO turns out not to work well for printing large lists of literals
 - (couple hundred thousand at once). The solution is to use strict IO. And
 - also to write literal lists as single files, which is part of why there are
 - so many read/write functions. This is the IO verion, which shouldn't be used
 - in the actual evaluation of cuts. Use one of the read* functions from
 - Actions.hs instead.
 - See: https://github.com/ndmitchell/shake/issues/37
 - TODO All (haskell) reads should eventually go through this function
 -}
readFileStrict :: Locks -> FilePath -> IO String
readFileStrict ref path = withReadLock ref path $ Strict.readFile path

-- sometimes you do want the lazy version though, like when showing big files
readFileLazy :: Locks -> FilePath -> IO String
readFileLazy ls p = withReadLock ls p $ readFile p

-------------
-- hashing --
-------------

digestLength :: Int
digestLength = 10

-- Note that MD5 is no longer considered secure
-- But for our purposes (checking for updated files) it doesn't matter.
-- See https://en.wikipedia.org/wiki/MD5
digest :: (Show a) => a -> String
digest val = take digestLength $ show (hash asBytes :: Digest MD5)
  where
    asBytes = (pack . show) val

-----------
-- paths --
-----------

ignoreExistsError :: IO () -> IO ()
ignoreExistsError act = catchIOError act $ \e ->
  if isAlreadyExistsError e then return () else ioError e

unlessExists :: FilePath -> Action () -> Action ()
unlessExists path act = do
  e <- liftIO $ doesPathExist path
  unless e act

-- TODO use FilePatterns here rather than plain FilePaths
unlessMatch :: [FilePath] -> Action () -> Action ()
unlessMatch paths act = do
  tests <- liftIO $ mapM doesPathExist paths
  unless (any id tests) act

-- TODO call this module something besides Debug now that it also handles errors?
-- TODO can you remove the liftIO part? does the monadcatch part help vs just io?
removeIfExists :: (MonadIO m, MonadCatch m) => FilePath -> m ()
removeIfExists fileName = (liftIO (removeFile fileName)) `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwM e

rmAll :: [FilePath] -> IO ()
rmAll = mapM_ removePathForcibly

-- TODO fn to makeRelative to config dir

{- Follows zero or more symlinks until it finds the original file.
 - Takes an optional prefix not to follow outside, which will usually be
 - Nothing or the main tmpdir.
 -
 - TODO why would there ever be a "does not exist" error?
 -}
resolveSymlinks :: Maybe FilePath -> FilePath -> IO FilePath
resolveSymlinks mPrefix path = do
  -- liftIO $ putStrLn $ "resolveSymlinks path: '" ++ path ++ "'"
  isLink <- pathIsSymbolicLink path
  if not isLink
    then return path
    else do
      relPath <- readSymbolicLink path
      absPath <- absolutize $ takeDirectory path </> relPath
      -- putStrLn $ "resolveSymlinks absPath: " ++ absPath
      case mPrefix of
        Nothing -> resolveSymlinks mPrefix absPath
        Just p  -> if p `isPrefixOf` absPath
                     then resolveSymlinks mPrefix absPath
                     else return path

-- kind of silly that haskell doesn't have this built in, but whatever
-- TODO also follow symlinks? is there a time that would be bad?
-- https://www.schoolofhaskell.com/user/dshevchenko/cookbook/transform-relative-path-to-an-absolute-path
absolutize :: String -> IO String
absolutize aPath = do
  aPath' <- if "~" `isPrefixOf` aPath
    then do
      homePath <- getHomeDirectory
      return $ normalise $ addTrailingPathSeparator homePath
                        ++ tail aPath
    else do
      pathMaybeWithDots <- absolute_path aPath
      return $ fromJust $ guess_dotdot pathMaybeWithDots
  aPath'' <- makeAbsolute aPath'
  return aPath''
  -- resolveSymlink aPath''

expandTildes :: String -> IO String
expandTildes s = do
  home <- getHomeDirectory
  return $ replace "~" home s

globFiles :: String -> IO [FilePath]
globFiles ptn = expandTildes ptn >>= glob >>= mapM absolutize

----------
-- misc --
----------

stripWhiteSpace :: String -> String
stripWhiteSpace = dropWhile isSpace . dropWhileEnd isSpace

-- Note that this is the only lazy read function. Will it mess anything up?
-- TODO should readLit and readList be based on this?
isEmpty :: Locks -> FilePath -> Action Bool
isEmpty ref path = do
  -- TODO remove? prevents "invalid byte sequence" error reading binary files
  if "cache/bin" `isInfixOf` path
    then return False
    else do
      txt <- withReadLock' ref path $ readFile' path
      return $ "<<empty" `isPrefixOf` txt

isReallyEmpty :: FilePath -> Action Bool
isReallyEmpty path = do
  if "cache/bin" `isInfixOf` path
    then return False
    else liftIO $ do
      stat <- getFileStatus path
      return (fileSize stat == 0)

popFrom :: Int -> [a] -> (a, [a])
popFrom _ [] = error "attempt to pop elem from empty list"
popFrom n xs
  | n < length xs = (xs !! n, take n xs ++ drop (n+1) xs)
  | otherwise = error $ "attempt to pop elem "
                  ++ show n ++ " from a list with "
                  ++ show (length xs) ++ " elements"

-- based on https://stackoverflow.com/a/43291593
-- should reconstruct a list after using popFrom with the same index
insertAt :: Int -> a -> [a] -> [a]
insertAt i newElement as
  | null as && i /= 0 = error "Cannot insert into empty list other than position 0."
  | null as && i == 0 = [newElement]
  | i >= 0 = let (prefix, suffix) = splitAt i as
             in prefix <> [newElement] <> suffix
insertAt _ _ _ = error "bad arg to insertAt"
