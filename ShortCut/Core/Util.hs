module ShortCut.Core.Util where

import Data.Char             (isSpace)
import Data.List             (dropWhileEnd)
import Data.List             (isPrefixOf)
import Data.List.Utils       (replace)
import Data.Maybe            (fromJust)
import ShortCut.Core.Types   (CutConfig)
import System.Directory      (getHomeDirectory)
import System.FilePath       (addTrailingPathSeparator, normalise)
import System.Path.NameManip (guess_dotdot, absolute_path)
import Test.Tasty            (testGroup, TestTree)

stripWhiteSpace :: String -> String
stripWhiteSpace = dropWhile isSpace . dropWhileEnd isSpace

-- kind of silly that haskell doesn't have this built in, but whatever
-- https://www.schoolofhaskell.com/user/dshevchenko/cookbook/transform-relative-path-to-an-absolute-path
absolutize :: String -> IO String
absolutize aPath 
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath 
                             ++ tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots

expandTildes :: String -> IO String
expandTildes s = do
  home <- getHomeDirectory
  return $ replace "~" home s

mkTestGroup :: CutConfig -> String -> [CutConfig -> IO TestTree] -> IO TestTree
mkTestGroup cfg name trees = do
  let trees' = mapM (\t -> t cfg) trees
  trees'' <- trees'
  return $ testGroup name trees''
