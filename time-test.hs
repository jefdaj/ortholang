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

main :: IO ()
main = do
  cDir <- nowDatedDir
  let dlDir = "~/.ortholang/cache/download/" ++ cDir
  putStrLn $ "download dir based on current time is " ++ dlDir
  let cDay = dirToDay dlDir
  putStrLn $ "converting it back to a Day, we get " ++ show cDay
