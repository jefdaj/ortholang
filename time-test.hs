module Main where

-- TODO looking good! next step is to add parsing
-- minimum parsing that would work:
--   always require an exact YYYY-MM-DD, but ignore seconds
--   if it's in the future/past/not available/invalid, ask to switch to today and re-run
--   conflicting files: overwrite the same day, assuming it's fixing an error, and re-run everything

-- TODO cases:
--        if re-download config, default to "today"
--        if "latest", pick the latest cache or default to "today"
--        if exact date, use it if its valid + exists of default to "today"
--        if invalid date, default to "today"
--        if "today", re-download unless today's cache exists already
--        so default to "today" if: :config redownload, invalid date, "latest" but no cache, exact date but no cache
--        only times *not* to use today: "latest" and cache exists, exact date and cache exists

import Data.Time
import Text.Printf
import System.FilePath ((</>))
import Data.List.Split (splitOn)
import OrthoLang.Util (absolutize, globFiles)
import Data.List (sort)
import System.Directory (doesPathExist)

-- nowDatedDir :: IO FilePath
-- nowDatedDir = do
  -- (UTCTime date _) <- getCurrentTime
  -- return $ dayToDir date

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

getToday :: IO Day
getToday = do
  (UTCTime today _) <- getCurrentTime
  return today

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- parseDateToday :: String -> IO Day
-- parseDateToday input = do
--   today <- getToday
--   if input == "today" then return today
--   else case parseDate input of
--     Just day -> return day
--     Nothing -> do
--       putStrLn $ "Invalid date '" ++ input ++ "'. Defaulting to the current UTC date."
--       return today

existingCacheDated :: FilePath -> FilePath -> Day -> IO (Maybe FilePath)
existingCacheDated cacheDir cachePath day = do
  cacheDir' <- absolutize cacheDir
  let dated = cacheDir' </> dayToDir day </> cachePath
  exists <- doesPathExist dated
  return $ if exists
    then Just dated
    else Nothing

  -- exists <- doesDirectoryExist dated
  -- if exists || day == today
    -- then return dated
    -- else do
      -- putStrLn $ "No existing cache in '" ++ cacheDir ++
                 -- "' from '" ++ date ++ "'. Defaulting to the current UTC date."
      -- existingCacheDated cacheDir "today"

-- warning: assumes all files in the cache dir are yyyy/mm/dd formatted
existingCacheLatest :: FilePath -> FilePath -> IO (Maybe FilePath)
existingCacheLatest cacheDir cachePath = do
  matches <- globFiles $ cacheDir </> "*" </> "*" </> "*" </> cachePath
  if null matches then return Nothing
  else return $ Just $ head $ sort matches

-- note: this is the only one that returns a result even if the path doesn't exist yet
cacheToday :: FilePath -> IO FilePath
cacheToday cacheDir = do
  today <- getToday
  cacheDir' <- absolutize cacheDir
  let dated = cacheDir' </> dayToDir today
  return dated

-- | Entry point for finding a cached file from a user-specified date
-- TODO need to take the relpath inside the cache dir into account too
-- cacheDirUser :: FilePath -> String -> IO FilePath
-- cacheDirUser cacheDir input = do
--   latest <- existingCacheLatest cacheDir
--   case input of
--     "latest" -> case latest of
--       Nothing -> do
--         putStrLn $ "No existing cache in '" ++ cacheDir ++ "'. Defaulting to the current UTC date."
--         existingCacheDated cacheDir "today"
--       Just d -> do
--         putStrLn $ "Using the latest cached version of '" ++ cacheDir ++ "', which is from '" ++ d ++ "'."
--         return d
--     _ -> existingCacheDated cacheDir input

main :: IO ()
main = undefined
--   cDir <- nowDatedDir
--   let dlDir = "~/.ortholang/cache/download/" ++ cDir
--   putStrLn $ "download dir based on current time is " ++ dlDir
--   let cDay = dirToDay dlDir
--   putStrLn $ "converting it back to a Day, we get " ++ show cDay
