module Main where

-- TODO have cacheUser check the cfg and default to today if we're re-downloading everything
-- TODO add a warning message when we're about to re-download

-- TODO is "latest" the wrong word? maybe "cached" makes more sense or "latest-cached" or "previous" or "most-recent-cached" or "last"
--        doesn't matter for now, because the implementation is the same

-- TODO modules to integrate: Curl, BlastDB, BioMartR, Busco, *not* Load (maybe use curl for that)

-- TODO Curl integration: curl "date" -> curl_path "dst path"
--        could have an ortholang curl fn : str str -> untyped, and allow either untyped or str in load fns
--        first str would be the date, second the url
--        might be good to implement this first because blastdb + biomartr could be made to use it? check on that

-- TODO BlastDB integration: blastdblist "date" -> rules for today's dblist.txt, load_list <dblist.txt path>?
--        separate this into its own module? it's different from and built on top of blastdb
--        the blastdblist and blastdbget fns should both have date strs because you might want to list newer than get

-- TODO BioMartR integration: biomartr_cache "date" -> biomartr_path "dst path", biomartr_search "date"?
--        seems like you have to download stuff for each search but there's also an overall list of txt files? see if really both

-- TODO Busco integration: busco_list_lineages "date" -> busco_list_lineages_path "cache path"
--        actually you could hold off on this one for now as they seem to be static

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
--   If this returns Nothing, it means we'll need to download the file.
--
--   TODO confirmation dialog before downloading, or just assume?
cacheUser :: FilePath -> FilePath -> String -> IO (Maybe FilePath)
cacheUser cacheDir cachePath userInput = do
  let userDay = parseDate userInput
  userCache <- case userDay of
    Nothing -> return Nothing
    Just d -> existingCacheDated cacheDir cachePath d
  latest  <- existingCacheLatest cacheDir cachePath
  today   <- cacheToday cacheDir cachePath
  return $ if      userInput == "latest" then latest
           else if userInput == "today"  then Just today
           else    userCache

main :: IO ()
main = undefined
