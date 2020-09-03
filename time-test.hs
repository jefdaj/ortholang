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

nowDatedDir :: IO FilePath
nowDatedDir = do
  (UTCTime date _) <- getCurrentTime
  let (year, month, day) = toGregorian date
      sYear  = printf "%04d" year
      sMonth = printf "%02d" month
      sDay   = printf "%02d" day
      -- sSecs  = show (round secs :: Integer)
      dir    = sYear </> sMonth </> sDay -- </> sSecs
  return dir

-- TODO return a Maybe?
dirToDay :: FilePath -> Day
dirToDay dir = fromGregorian nYear nMonth nDay
  where
    (day:month:year:_) = reverse $ splitOn "/" dir
    nYear  = read year  :: Integer
    nMonth = read month :: Int
    nDay   = read day   :: Int

-- isCachedDataValid :: Day -> IO Bool
-- isCachedDataValid dayFetched = do
--   (UTCTime currentDay _) <- getCurrentTime
--   let lastDayValid = addDays 90 dayFetched -- TODO configure the validity time?
--       daysLeft = diffDays lastDayValid currentDay
--   return $ daysLeft > 0

main :: IO ()
main = do
  cDir <- nowDatedDir
  let dlDir = "~/.ortholang/cache/download/" ++ cDir
  putStrLn $ "download dir based on current time is " ++ dlDir
  let cDay = dirToDay dlDir
  putStrLn $ "converting it back to a Day, we get " ++ show cDay
