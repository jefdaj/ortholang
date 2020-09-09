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
import OrthoLang.Interpreter.Paths (listDigestsInPath, pathDigest)
import Development.Shake

import Control.Monad.IO.Class     (liftIO)
import Data.Maybe                 (fromJust)
import Development.Shake          (Action, getShakeExtra)
import Development.Shake.FilePath ((</>))
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))

-- import Data.Time
-- import Text.Printf
-- import System.FilePath ((</>))
-- import Data.List.Split (splitOn)
-- import OrthoLang.Util (absolutize, globFiles)
-- import Data.List (sort)
-- import System.Directory (doesPathExist)
import System.FilePath (takeDirectory)


----------------------
-- haskell function --
----------------------

-- TODO make it a NewAction1? 2?
curl :: Path -> Path -> Action ()
curl outPath urlPath = do
  liftIO $ putStrLn $ "running curl '" ++ show urlPath ++ "'"
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.curl.curl"
      urlPath' = fromPath loc cfg urlPath
      outPath' = fromPath loc cfg outPath
      cDir     = fromPath loc cfg $ cacheDir cfg "curl"
      -- outPath  = cDir </> digest loc url
  liftIO $ createDirectoryIfMissing True cDir
  runCmd $ CmdDesc
    { cmdBinary = "curl.sh"
    , cmdArguments = [outPath', urlPath']
    , cmdFixEmpties = False
    , cmdParallel = False
    , cmdInPatterns = []
    , cmdNoNeedDirs = []
    , cmdOutPath = outPath'
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath']
    }
  -- return $ toPath loc cfg outPath

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
  , mRules = []
  , mFunctions = [curlImplicit, curlDate]
  }

-----------------------------------
-- future core library functions --
-----------------------------------

-- dayToDir :: Day -> FilePath
-- dayToDir date = sYear </> sMonth </> sDay
--   where
--     (year, month, day) = toGregorian date
--     sYear  = printf "%04d" year
--     sMonth = printf "%02d" month
--     sDay   = printf "%02d" day
-- 
-- dirToDay :: FilePath -> Day
-- dirToDay dir = fromGregorian nYear nMonth nDay
--   where
--     (day:month:year:_) = reverse $ splitOn "/" dir
--     nYear  = read year  :: Integer
--     nMonth = read month :: Int
--     nDay   = read day   :: Int
-- 
-- getToday :: IO Day
-- getToday = do
--   (UTCTime today _) <- getCurrentTime
--   return today
-- 
-- parseDate :: String -> Maybe Day
-- parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"
-- 
-- -- | Returns an existing cache matching a specific path + date, or Nothing
-- existingCacheDated :: FilePath -> FilePath -> Day -> IO (Maybe FilePath)
-- existingCacheDated cacheDir cachePath day = do
--   cacheDir' <- absolutize cacheDir
--   let dated = cacheDir' </> dayToDir day </> cachePath
--   exists <- doesPathExist dated
--   return $ if exists
--     then Just dated
--     else Nothing
-- 
-- -- | Returns the latest existing cache matching a specific path, or Nothing
-- --   Warning: assumes all files in the cache dir are yyyy/mm/dd formatted
-- existingCacheLatest :: FilePath -> FilePath -> IO (Maybe FilePath)
-- existingCacheLatest cacheDir cachePath = do
--   matches <- globFiles $ cacheDir </> "*" </> "*" </> "*" </> cachePath
--   if null matches then return Nothing
--   else return $ Just $ head $ sort matches
-- 
-- -- | Returns today's cache for a specific file, whether or not it exists yet
-- cacheToday :: FilePath -> FilePath -> IO FilePath
-- cacheToday cacheDir cachePath = do
--   today <- getToday
--   cacheDir' <- absolutize cacheDir
--   let dated = cacheDir' </> dayToDir today </> cachePath
--   return dated
-- 
-- -- | Entry point for finding a cached file from a user-specified date.
-- --   If this returns Nothing, it means we'll need to download the file.
-- --
-- --   TODO confirmation dialog before downloading, or just assume?
-- cacheUser :: FilePath -> FilePath -> String -> IO (Maybe FilePath)
-- cacheUser cacheDir cachePath userInput = do
--   let userDay = parseDate userInput
--   userCache <- case userDay of
--     Nothing -> return Nothing
--     Just d -> existingCacheDated cacheDir cachePath d
--   cached  <- existingCacheLatest cacheDir cachePath
--   today   <- cacheToday cacheDir cachePath
--   return $ if      userInput == "cached" then cached
--            else if userInput == "today"  then Just today
--            else    userCache

-----------
-- curl* --
-----------

-- | Version that auto-finds the proper date based on user input
curlImplicit :: Function
curlImplicit = newFnA2
  "curl"
  (Exactly str, Exactly str)
  (Exactly Untyped)
  (newDate1of2 "curl")
  [ReadsURL, ReadsFile]

-- | Version that assumes the date is properly formatted
curlDate :: Function
curlDate = newFnA2
  "curl_date"
  (Exactly str, Exactly str)
  (Exactly Untyped)
  aCurlDate
  [ReadsURL, Hidden]

{- Explain that the valid date strs are:
 -
 - * a specific date in "yyyy-mm-dd" format, which requires that day's cache to exist
 -     (in case of conflicts, the latest-timestamped version with the same date wins)
 - * "today", which acts like today's date and can download + cache new files
 - * "cached", which picks the most recent existing cache or acts like "today" if there is none
 -
 - Date ranges or other expressions might be added in the future.
 -}

-- TODO should this be the regular "curl" as a macro expansion?
-- TODO basically, we want the final date decided on to be one of the args right?
--      path could be like: exprs/curl_date/2020-09-04/<the other hash>/result
--      except we don't want to mess up the expr path mapping stuff! that's fiddly
--      so do the date-overwriting thing during macro expansion, then pass as a regular str to the rest
--      but inside the Action you *can* access the date and use it to control the cache dir
--      so expand the macro to get a cache date, then read + use that in the action

-- | `cacheDir` and `cachePath` are properties of the `Action` to be performed,
--   while `dateDescPath` is a path to a user-supplied date description. It can
--   be "today", "cached", or a specific date in "yyyy-mm-dd" format. This
--   function returns a valid path to the cached file. The file must either exist
--   already or be described by a separate `Rules` entry, or Shake will fail to
--   build it. The path after the dated part must also be unique.
-- withDatedCache :: FilePath               -- ^ path before the dated part
--                -> FilePath               -- ^ path after  the dated part
--                -> FilePath               -- ^ path to user-supplied description of the date
--                -> (FilePath -> Action a) -- ^ fn to pass the cached downloaded file to
--                -> Action a
-- withDatedCache cacheDir cachePath dateDescPath actFn = do
--   let resolvedCachePath = undefined dateDescPath
--   actFn resolvedCachePath

aCurlDate :: NewAction2
aCurlDate (ExprPath outPath') datePath' urlPath' = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "ortholang.modules.curl.aCurlDate"
      urlPath = toPath loc cfg urlPath'
      outPath = toPath loc cfg outPath'

  liftIO $ putStrLn $ "datePath': " ++ datePath'
  liftIO $ putStrLn $ "urlPath': " ++ urlPath'

  -- decide on the dated cache path
  date <- readLit loc datePath'
  let cacheDir' = fromPath loc cfg $ cacheDir cfg "curl"
      -- cachePath = digest loc urlPath' </> "result" -- TODO actually we want the digest of the url right?
      -- urlDigest = takeBaseName $ takeDirectory urlPath' -- this is the cachePath within the dated dir
      (PathDigest urlDigest) = last $ listDigestsInPath cfg urlPath'
      -- cachePath = urlDigest </> "result"

  liftIO $ putStrLn $ "date: " ++ date
  liftIO $ putStrLn $ "cacheDir': " ++ cacheDir'
  liftIO $ putStrLn $ "urlDigest: " ++ urlDigest

  -- if the cache path resolution works, this is Just something
  -- TODO if not, just return cacheToday here? or should the distinction be used for a warning?
  -- mUserCachePath <- liftIO $ cacheUser cacheDir' urlDigest userCacheDesc
  -- cachePath' <- case mUserCachePath of
    -- Nothing -> liftIO $ cacheToday cacheDir' urlDigest
    -- Just p -> return p

  -- TODO the final output path should depend on this rather than the initial userCacheDescPath', right?
  let cachePath' = cacheDir' </> date </> urlDigest -- TODO make this a dir and add /result?
      cachePath  = toPath loc cfg cachePath'
  liftIO $ createDirectoryIfMissing True $ takeDirectory cachePath'

  -- download the file if needed
  -- TODO this has to be a separate rule though, right?
  -- TODO merge it into the curl function itself by adding an outpath?
  -- TODO are both symlinks here really necessary?
  curl cachePath urlPath -- TODO move this to a Rules pattern
  liftIO $ putStrLn $ "aCurlDate cachePath, which should be handled by a separate Rule: " ++ render (pPrint cachePath)
  liftIO $ putStrLn $ "aCurlDate symlink: " ++ show outPath ++ " -> " ++ show cachePath
  symlink outPath cachePath
