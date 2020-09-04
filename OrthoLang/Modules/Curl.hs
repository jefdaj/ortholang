module OrthoLang.Modules.Curl
  (

  -- * Function for use in Haskell code in other modules
    curl

  -- * OrthoLang module with function for end users
  , olModule

  )
  where

-- TODO include in the load function too if given a url?
-- TODO add a "loadable" typegroup in Load.hs that works with str or untyped (using the raw path)

import OrthoLang.Types
import OrthoLang.Interpreter
import Development.Shake

import Control.Monad.IO.Class     (liftIO)
import Data.Maybe                 (fromJust)
import Development.Shake          (Action, getShakeExtra)
import Development.Shake.FilePath ((</>))
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))

import Data.Time
import Text.Printf
-- import System.FilePath ((</>))
import Data.List.Split (splitOn)
import OrthoLang.Util (absolutize, globFiles)
import Data.List (sort)
import System.Directory (doesPathExist)


----------------------
-- haskell function --
----------------------

-- TODO make it a NewAction1? 2?
curl :: Path -> Action Path
curl url = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.curl.curl"
      url'    = fromPath loc cfg url
      cDir    = fromPath loc cfg $ cacheDir cfg "curl"
      outPath = cDir </> digest loc url
  liftIO $ createDirectoryIfMissing True cDir
  runCmd $ CmdDesc
    { cmdBinary = "curl.sh"
    , cmdArguments = [outPath, url']
    , cmdFixEmpties = False
    , cmdParallel = False
    , cmdInPatterns = []
    , cmdNoNeedDirs = []
    , cmdOutPath = outPath
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath]
    }
  return $ toPath loc cfg outPath

----------------------
-- ortholang module --
----------------------

olModule :: Module
olModule = Module
  { mName = "Curl"
  , mDesc = "Download generic (untyped) files for use in the load_ functions"
  , mTypes = []
  , mGroups = []
  , mEncodings = []
  , mFunctions = [curlDate]
  }

-----------------------------------
-- future core library functions --
-----------------------------------

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
  cached  <- existingCacheLatest cacheDir cachePath
  today   <- cacheToday cacheDir cachePath
  return $ if      userInput == "cached" then cached
           else if userInput == "today"  then Just today
           else    userCache

---------------
-- curl_date --
---------------

curlDate :: Function
curlDate = newFnA2
  "curl_date"
  (Exactly str, Exactly str)
  (Exactly Untyped)
  aCurlDate
  [ReadsURL]

aCurlDate :: NewAction2
aCurlDate (ExprPath outPath') userCacheDescPath' urlPath' = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "ortholang.modules.curl.aCurlDate"
      urlPath = toPath loc cfg urlPath'
      outPath = toPath loc cfg outPath'

  -- decide on the dated cache path
  userCacheDesc <- readLit loc userCacheDescPath'
  let cacheDir' = fromPath loc cfg $ cacheDir cfg "curl"
      cachePath = digest loc urlPath' </> "result"

  liftIO $ putStrLn $ "userCacheDesc: " ++ userCacheDesc
  liftIO $ putStrLn $ "cacheDir': " ++ cacheDir'
  liftIO $ putStrLn $ "cachePath: " ++ cachePath

  -- if the cache path resolution works, this is Just something
  -- TODO if not, just return cacheToday here? or should the distinction be used for a warning?
  mUserCachePath <- liftIO $ cacheUser cacheDir' cachePath userCacheDesc
  cachePath' <- case mUserCachePath of
    Nothing -> liftIO $ cacheToday cacheDir' cachePath
    Just p -> return p
  let cachePath = toPath loc cfg cachePath'

  -- download the file if needed
  -- TODO this has to be a separate rule though, right?
  -- TODO merge it into the curl function itself by adding an outpath?
  -- TODO are both symlinks here really necessary?
  tmpPath <- curl urlPath
  symlink cachePath tmpPath
  symlink outPath cachePath
