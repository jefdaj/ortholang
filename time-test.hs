module Main where

-- TODO have cacheUser check the cfg and default to today if we're re-downloading everything
-- TODO add a warning message when we're about to re-download

import Data.Time
import Text.Printf
import System.FilePath ((</>))
import Data.List.Split (splitOn)
import OrthoLang.Util (absolutize, globFiles)
import Data.List (sort)
import System.Directory (doesPathExist)

dayToDir :: Day -> FilePath
dayToDir date = sYear </> sMonth </> sDay
  where
    (year, month, day) = toGregorian date
    sYear  = printf "%04d" year
    sMonth = printf "%02d" month
    sDay   = printf "%02d" day

dirToDay :: FilePath -> Day
dirToDay dir = fromGregorian nYear nMonth nDay
  where
    (day:month:year:_) = reverse $ splitOn "/" dir
    nYear  = read year  :: Integer
    nMonth = read month :: Int
    nDay   = read day   :: Int

getToday :: IO Day
getToday = do
  (UTCTime today _) <- getCurrentTime
  return today

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- | Returns an existing cache matching a specific path + date, or Nothing
existingCacheDated :: FilePath -> FilePath -> Day -> IO (Maybe FilePath)
existingCacheDated cacheDir cachePath day = do
  cacheDir' <- absolutize cacheDir
  let dated = cacheDir' </> dayToDir day </> cachePath
  exists <- doesPathExist dated
  return $ if exists
    then Just dated
    else Nothing

-- | Returns the latest existing cache matching a specific path, or Nothing
--   Warning: assumes all files in the cache dir are yyyy/mm/dd formatted
existingCacheLatest :: FilePath -> FilePath -> IO (Maybe FilePath)
existingCacheLatest cacheDir cachePath = do
  matches <- globFiles $ cacheDir </> "*" </> "*" </> "*" </> cachePath
  if null matches then return Nothing
  else return $ Just $ head $ sort matches

-- | Returns today's cache for a specific file, whether or not it exists yet
cacheToday :: FilePath -> FilePath -> IO FilePath
cacheToday cacheDir cachePath = do
  today <- getToday
  cacheDir' <- absolutize cacheDir
  let dated = cacheDir' </> dayToDir today </> cachePath
  return dated

-- | Entry point for finding a cached file from a user-specified date.
--   It silently defaults to the cache path for today's date unless it can find
--   a valid one matching the user input.
cacheUser :: FilePath -> FilePath -> String -> IO FilePath
cacheUser cacheDir cachePath userInput = do
  let userDay = parseDate userInput
  userCache <- case userDay of
    Nothing -> return Nothing
    Just d -> existingCacheDated cacheDir cachePath d
  latest  <- existingCacheLatest cacheDir cachePath
  today   <- cacheToday          cacheDir cachePath
  if userInput == "latest" then case latest of
    Nothing -> return today
    Just c  -> return c
  else case userCache of
    Nothing -> return today
    Just c  -> return c

main :: IO ()
main = undefined
