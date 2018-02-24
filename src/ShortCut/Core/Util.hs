module ShortCut.Core.Util

  -- locking
  ( ignoreExistsError
  , unlessExists
  , unlessMatch
  , withLock
  , withLocks
  -- , writeAllOnce
  , rmAll

  -- hashing
  , digestLength
  , digest

  -- paths
  , resolveSymlinks
  , absolutize
  , expandTildes
  , globFiles

  -- misc
  , stripWhiteSpace
  , removeIfExists
  )
  where

import Development.Shake

import Control.Monad          (unless)
import Control.Monad.Catch    (MonadCatch, catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash            (hash, Digest, MD5)
import Data.ByteString.Char8  (pack)
import Data.Char              (isSpace)
import Data.List              (dropWhileEnd, isPrefixOf, nub)
import Data.List.Utils        (replace)
import Data.Maybe             (fromJust)
import System.Directory       (createDirectoryIfMissing, doesPathExist,
                               removePathForcibly, getHomeDirectory,
                               makeAbsolute, removeFile, pathIsSymbolicLink)
import System.FileLock        (lockFile, unlockFile, SharedExclusive(..),
                               FileLock)
import System.FilePath        ((</>), takeDirectory, addTrailingPathSeparator,
                               normalise)
import System.IO.Error        (isDoesNotExistError, catchIOError,
                               isAlreadyExistsError, ioError)
import System.Path.NameManip  (guess_dotdot, absolute_path)
import System.Posix.Files     (readSymbolicLink)
import Test.Tasty             (testGroup, TestTree)
import System.FilePath.Glob   (glob)

-------------
-- locking --
-------------

ignoreExistsError :: IO () -> IO ()
ignoreExistsError act = catchIOError act $ \e ->
  if isAlreadyExistsError e then return () else ioError e

unlessExists :: FilePath -> Action () -> Action ()
unlessExists path act = do
  e <- doesFileExist path
  unless e act

-- TODO use FilePatterns here rather than plain FilePaths
unlessMatch :: [FilePath] -> Action () -> Action ()
unlessMatch paths act = do
  tests <- liftIO $ mapM doesPathExist paths
  unless (any id tests) act

withLock :: SharedExclusive -> FilePath -> Action a -> Action a
withLock lockType lockPath actFn = do
  -- liftIO $ createDirectoryIfMissing True $ takeDirectory lockPath
  -- lock <- liftIO $ lockFile lockPath lockType
  lock <- liftIO $ mkLock lockType lockPath
  actFn `actionFinally` rmLock lock lockPath

-- Mostly for locking all inputs and outputs used by a wrappedCmdWrite
withLocks :: SharedExclusive -> [FilePath] -> Action a -> Action a
withLocks lockType lockPaths actFn = do
  let lockPaths' = nub lockPaths -- TODO dedup these earlier if needed
  -- liftIO $ putStrLn $ "before nub: " ++ show (length lockPaths)
  -- liftIO $ putStrLn $ "after  nub: " ++ show (length lockPaths')
  locks <- liftIO $ mapM (mkLock lockType) lockPaths'
  actFn `actionFinally` (mapM_ (\(l, p) -> rmLock l p) $ zip locks lockPaths')

-- TODO need to not have the .lock on there until the actual file is found!
-- TODO rather than custom logic here, make the calls make sense!
mkLock :: SharedExclusive -> FilePath -> IO FileLock
mkLock lockType lockPath = do
  createDirectoryIfMissing True $ takeDirectory lockPath
  -- liftIO $ putStrLn $ "locking: (" ++ show lockType ++ ") "++ lockPath
  lockFile lockPath lockType

-- Keeps lockfiles from laying around cluttering up trees
rmLock :: FileLock -> FilePath -> IO ()
rmLock lockToken lockPath = do
  unlockFile lockToken
  removePathForcibly lockPath
  -- remains <- doesPathExist lockPath
  -- when remains $ removePathForcibly lockPath

-- -- TODO do these actually work infix?
-- -- TODO what if only some of the files exist? do we want to re-run the action?
-- -- TODO should this handle debugTrackWrite on all the outPaths?
-- writeAllOnce :: FilePath -> [FilePath] -> Action () -> Action ()
-- writeAllOnce lockPath outPaths writeFn = do
--   lock <- liftIO $ lockFile lockPath Exclusive
--   unlessMatch outPaths $
--     writeFn `actionOnException` liftIO (rmAll outPaths)
--             `actionFinally` rmLock lockPath lock
--   -- liftIO $ rmLock lockPath lock

rmAll :: [FilePath] -> IO ()
rmAll = mapM_ removePathForcibly

-- TODO fn to makeRelative to config dir

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

-- TODO call this module something besides Debug now that it also handles errors?
-- TODO can you remove the liftIO part? does the monadcatch part help vs just io?
removeIfExists :: (MonadIO m, MonadCatch m) => FilePath -> m ()
removeIfExists fileName = (liftIO (removeFile fileName)) `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwM e
