module OrthoLang.Util

  -- debugging
  ( trace
  , traceShow
  , debug
  , time

  -- read files
  , readFileStrict
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
  -- , absolutizeListOfLitLists
  , expandTildes
  , globFiles

  -- error handling
  -- , ignoreErrors
  , retryIgnore

  -- misc
  , stripWhiteSpace
  , isEmpty
  , isReallyEmpty
  , popFrom
  , insertAt
  , justOrDie
  , headOrDie
  )
  where

import Development.Shake

import qualified System.IO.Strict as Strict
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

import Data.Monoid ((<>))
import Control.Monad          (unless)
import Control.Exception.Safe (MonadCatch, catch, throwM, catchAny)
import Control.Retry          (recoverAll, exponentialBackoff, limitRetries)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash            (hash, Digest, MD5)
import Data.Char              (isSpace)
import Data.List              (dropWhileEnd, isPrefixOf, isInfixOf, splitAt)
import Data.List.Utils        (replace)
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
import OrthoLang.Locks    (LocksRef, withReadLock, withReadLock', withWriteLock)

import Control.Logging (traceSL, debugS, timedDebugEndS, traceShowSL)

---------------
-- debugging --
---------------

trace :: String -> String -> a -> a
trace suffix msg = traceSL (T.pack $ "ortholang." ++ suffix) (T.pack msg)

traceShow :: Show a => String -> a -> a
traceShow suffix = traceShowSL (T.pack $ "ortholang." ++ suffix)

debug :: String -> String -> IO ()
debug suffix msg = debugS (T.pack $ "ortholang." ++ suffix) (T.pack msg)

-- TODO rearrange imports so you can make a debugA :: ActionR () too

time :: String -> String -> IO a -> IO a
time suffix msg act = timedDebugEndS (T.pack $ "ortholang." ++ suffix)
                                     (T.pack msg) act

-- TODO put this in Util
-- TODO this works, but isn't used yet
-- getTimeStamp :: IO String
-- getTimeStamp = getZonedTime >>= return . formatTime defaultTimeLocale fmt
--   where
--     fmt = "[%Y-%m-%d %H:%M:%S %q]"

-- TODO should this go in Util too?
-- TODO remove if you can figure out a way to put stamps in regular debug
-- debugA :: Config -> String -> a -> IO a
-- debugA msg rtn = do
--   stamp <- getTimeStamp
--   return $ debug cfg (stamp ++ " " ++ msg) rtn

-- TODO ok this goes in Util
--debug :: Config -> String -> a -> a
--debug cfg msg rtn = if cfgDebug cfg then trace msg rtn else rtn

-- TODO and this one 
-- TODO stop exporting this in favor of the ones below?
-- debugShow :: Show a => Config -> a -> b -> b
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
readFileStrict :: LocksRef -> FilePath -> IO String
readFileStrict ref path = withReadLock ref path $ Strict.readFile path

-- sometimes you do want the lazy version though, like when showing big files
readFileLazy :: LocksRef -> FilePath -> IO String
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
    asBytes = (C8.pack . show) val

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
removeIfExists :: (MonadIO m, MonadCatch m) => LocksRef -> FilePath -> m ()
removeIfExists ref fileName = (liftIO (withWriteLock ref fileName $ removeFile fileName)) `catch` handleExists
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
  -- liftIO $ putStrLn $ "resolveSymlinks path: \"" ++ path ++ "\""
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
      return $ justOrDie "guess_dotdot in absolutize failed!" $ guess_dotdot pathMaybeWithDots
  aPath'' <- makeAbsolute aPath'
  return aPath''
  -- resolveSymlink aPath''

-- makes a copy of a list of lists of lits, suitible for passing to a script
-- TODO separate version for a list of lists of paths?
-- absolutizeListOfLitLists :: Config -> LocksRef -> Path -> FilePath
-- absolutizeListOfLitLists cfg ref path = do
--   paths <- readPaths

expandTildes :: String -> IO String
expandTildes s = do
  home <- getHomeDirectory
  return $ replace "~" home s

globFiles :: String -> IO [FilePath]
globFiles ptn = expandTildes ptn >>= glob >>= mapM absolutize

--------------------
-- error handling --
--------------------

ignoreErrors fn = catchAny fn (\e -> putStrLn ("error! " ++ show e))

-- This one is as bad as it sounds, so remove it when able! It's the only
-- way I've managed to solve the occasional "openFile" lock conflicts.
-- TODO at least log when a retry happens for debugging
-- TODO ask Niel if individual actions can be retried instead
-- TODO could always fork Shake to put it in if needed too
retryIgnore fn = ignoreErrors $ recoverAll limitedBackoff fn
  where
    -- This isn't as bad as it sounds. It just prints an error message instead
    -- of crashing the rest of the program. The error will still be visible.
    limitedBackoff = exponentialBackoff 50 <> limitRetries 5

----------
-- misc --
----------

stripWhiteSpace :: String -> String
stripWhiteSpace = dropWhile isSpace . dropWhileEnd isSpace

-- Note that this is the only lazy read function. Will it mess anything up?
-- TODO should readLit and readList be based on this?
isEmpty :: LocksRef -> FilePath -> Action Bool
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

-- like fromJust, but at least this gives you something to debug
justOrDie :: String -> Maybe a -> a
justOrDie msg val = case val of
                 Nothing -> error msg
                 Just v -> v

-- like head, but at least this gives you something to debug
headOrDie :: String -> [a] -> a
headOrDie msg [] = error msg
headOrDie _ lst = head lst
