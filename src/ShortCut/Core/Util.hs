module ShortCut.Core.Util where

import Data.Char             (isSpace)
import Data.List             (dropWhileEnd)
import Data.List             (isPrefixOf)
import Data.List.Utils       (replace)
import Data.Maybe            (fromJust)
import ShortCut.Core.Types   (CutConfig, CutExpr, CutVar, CutScript,)
import System.Directory      (getHomeDirectory, makeAbsolute,
                              pathIsSymbolicLink)
import System.FilePath       (addTrailingPathSeparator, normalise)
import System.Path.NameManip (guess_dotdot, absolute_path)
import Test.Tasty            (testGroup, TestTree)
import Crypto.Hash           (hash, Digest, MD5)
import Data.ByteString.Char8 (pack)
import System.Posix.Files    (readSymbolicLink)
import System.FilePath            ((</>), takeDirectory)

-- TODO fn to makeRelative to config dir

-- Note that MD5 is no longer considered secure
-- But for our purposes (checking for updated files) it doesn't matter.
-- See https://en.wikipedia.org/wiki/MD5
digest :: (Show a) => a -> String
digest val = take 10 $ show (hash asBytes :: Digest MD5)
  where
    asBytes = (pack . show) val

stripWhiteSpace :: String -> String
stripWhiteSpace = dropWhile isSpace . dropWhileEnd isSpace

-- follows zero or more symlinks until it finds the original file
-- TODO actually just one now... which should always be enough right?
resolveSymlinks :: FilePath -> IO FilePath
resolveSymlinks path = do
  isLink <- pathIsSymbolicLink path
  if isLink
    -- TODO is it OK/desirable to follow more than once?
    then fmap (takeDirectory path </>) (readSymbolicLink path) -- >>= resolveSymlinks
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

mkTestGroup :: CutConfig -> String -> [CutConfig -> IO TestTree] -> IO TestTree
mkTestGroup cfg name trees = do
  let trees' = mapM (\t -> t cfg) trees
  trees'' <- trees'
  return $ testGroup name trees''

lookupVar :: CutVar -> CutScript -> CutExpr
lookupVar var scr = fromJust $ lookup var scr
