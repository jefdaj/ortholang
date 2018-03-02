module ShortCut.Core.Util

  -- hashing
  ( digestLength
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
  )
  where

import Development.Shake

import Control.Monad          (unless)
import Control.Monad.Catch    (MonadCatch, catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash            (hash, Digest, MD5)
import Data.ByteString.Char8  (pack)
import Data.Char              (isSpace)
import Data.List              (dropWhileEnd, isPrefixOf)
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
import System.Posix.Files     (readSymbolicLink)
import System.FilePath.Glob   (glob)

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
  e <- doesFileExist path
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
