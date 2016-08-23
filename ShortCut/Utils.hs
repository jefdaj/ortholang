module ShortCut.Utils where

import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import Data.Char                (isSpace)
import Data.List                (dropWhileEnd)

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
