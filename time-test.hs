module Main where

-- TODO looking good! next step is to add parsing
-- minimum parsing that would work:
--   always require an exact YYYY-MM-DD, but ignore seconds
--   if it's in the future/past/not available/invalid, ask to switch to today and re-run
--   conflicting files: overwrite the same day, assuming it's fixing an error, and re-run everything

import Data.Time
import Text.Printf
import System.FilePath ((</>))
import Data.List.Split (splitOn)
import OrthoLang.Util (absolutize, globFiles)
import Data.List (sort)

nowDatedDir :: IO FilePath
nowDatedDir = do
  (UTCTime date _) <- getCurrentTime
  return $ dayToDir date

dayToDir :: Day -> FilePath
dayToDir date = sYear </> sMonth </> sDay
  where
    (year, month, day) = toGregorian date
    sYear  = printf "%04d" year
    sMonth = printf "%02d" month
    sDay   = printf "%02d" day

-- TODO return a Maybe?
dirToDay :: FilePath -> Day
dirToDay dir = fromGregorian nYear nMonth nDay
  where
    (day:month:year:_) = reverse $ splitOn "/" dir
    nYear  = read year  :: Integer
    nMonth = read month :: Int
    nDay   = read day   :: Int

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

parseDateToday :: String -> IO Day
parseDateToday input = do
  today <- getCurrentDay
  if input == "today" then return today
  else case parseDate input of
    Just day -> return day
    Nothing -> do
      putStrLn $ "Invalid date '" ++ input ++ "'. Defaulting to the current UTC date."
      return today

getCurrentDay :: IO Day
getCurrentDay = do
  (UTCTime today _) <- getCurrentTime
  return today

cacheDirDated :: FilePath -> String -> IO FilePath
cacheDirDated cacheDir date = do
  day <- parseDateToday date
  cacheDir' <- absolutize cacheDir
  return $ cacheDir' </> dayToDir day

-- warning: assumes all files in the cache dir are yyyy/mm/dd formatted
cacheDirLatest :: FilePath -> IO (Maybe FilePath)
cacheDirLatest cacheDir = do
  matches <- globFiles $ cacheDir </> "*" </> "*" </> "*" 
  if null matches then return Nothing
  else return $ Just $ head $ sort matches

-- | Entry point for finding a cache dir from a user-specified date
cacheDirUser :: FilePath -> String -> IO FilePath
cacheDirUser cacheDir input = do
  latest <- cacheDirLatest cacheDir
  case input of
    "latest" -> case latest of
      Nothing -> do
        putStrLn $ "No existing cache in '" ++ cacheDir ++ "'. Defaulting to the current UTC date."
        cacheDirDated cacheDir "today"
      Just d  -> return d
    _ -> cacheDirDated cacheDir input

main :: IO ()
main = do
  cDir <- nowDatedDir
  let dlDir = "~/.ortholang/cache/download/" ++ cDir
  putStrLn $ "download dir based on current time is " ++ dlDir
  let cDay = dirToDay dlDir
  putStrLn $ "converting it back to a Day, we get " ++ show cDay
