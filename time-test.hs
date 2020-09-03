module Main where

import Data.Time
import Text.Printf
import System.FilePath ((</>))
import Data.List.Split (splitOn)

nowDatedDir :: IO FilePath
nowDatedDir = do
  (UTCTime date secs) <- getCurrentTime
  let (year, month, day) = toGregorian date
      sYear  = printf "%04d" year
      sMonth = printf "%02d" month
      sDay   = printf "%02d" day
      sSecs  = show (round secs :: Integer)
      dir    = sYear </> sMonth </> sDay </> sSecs
  return dir

-- TODO return a Maybe?
dirToDay :: FilePath -> Day
dirToDay dir = fromGregorian nYear nMonth nDay
  where
    (_:day:month:year:_) = reverse $ splitOn "/" dir
    nYear  = read year  :: Integer
    nMonth = read month :: Int
    nDay   = read day   :: Int

isCachedDataValid :: Day -> IO Bool
isCachedDataValid dayFetched = do
  (UTCTime currentDay _) <- getCurrentTime
  let lastDayValid = addDays 90 dayFetched -- TODO configure the validity time?
      daysLeft = diffDays lastDayValid currentDay
  return $ daysLeft > 0

main :: IO ()
main = do
  cDir <- nowDatedDir
  let dlDir = "~/.ortholang/cache/download/" ++ cDir
  putStrLn $ "download dir based on current time is " ++ dlDir
  let cDay = dirToDay dlDir
  putStrLn $ "converting it back to a Day, we get " ++ show cDay
  let plus = addDays 90 cDay
      minus = addDays (-90) cDay
  putStrLn $ "so current downloads are valid until " ++ show plus
  putStrLn $ "and today, downloads after " ++ show minus ++ " are valid"
  let old = fromGregorian 2019 01 01
      new = fromGregorian 2020 09 01
      future = fromGregorian 2025 01 01
  oldV <- isCachedDataValid old
  putStrLn $ "is data from " ++ show old ++ " valid? " ++ show oldV
  newV <- isCachedDataValid new
  putStrLn $ "is data from " ++ show new ++ " valid? " ++ show newV
  futureV <- isCachedDataValid future
  putStrLn $ "is data from " ++ show future ++ " valid? " ++ show futureV

  -- TODO looking good! next step is rewrite blastdblist to use it?
  -- TODO and separate filter into "find" or something
