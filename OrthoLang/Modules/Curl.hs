{-# LANGUAGE ScopedTypeVariables #-}

module OrthoLang.Modules.Curl
  (

  -- * Function for use in Haskell code in other modules
    -- curl

  -- * OrthoLang module with function for end users
    olModule

  )
  where

-- TODO include in the load function too if given a url?
-- TODO add a "loadable" typegroup in Load.hs that works with str or untyped (using the raw path)

import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Interpreter.Paths (listDigestsInPath)
import Development.Shake

import Control.Monad.IO.Class     (liftIO)
import Data.Maybe                 (fromJust)
import Development.Shake.FilePath ((</>))
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))

import Data.Time
import System.FilePath (takeDirectory, takeBaseName)

import OrthoLang.Interpreter.Actions (debugA)

---------------
-- debugging --
---------------

debugA' :: String -> String -> Action ()
debugA' loc = debugA ("ortholang.modules.curl." ++ loc)

---------------
-- curl rule --
---------------

-- TODO think it works? move to a utility module?
-- lookupPath :: PathDigest -> Action (Maybe Path)
-- lookupPath d = do
--   (dRef :: DigestsRef) <- fmap (justOrDie "lookupPath") getShakeExtra
--   dm <- liftIO $ readIORef dRef
--   return $ fmap snd $ M.lookup d dm

rCurl :: Rules ()
rCurl = do
  cfg <- fmap fromJust getShakeExtraRules
  (day :: Day) <- fmap fromJust getShakeExtraRules
  let cDir = tmpdir cfg </> "cache" </> "curl" </> show day
      ptn = cDir </> "*"
  ptn %> aCurl

aCurl :: FilePath -> Action ()
aCurl outPath' = do
  let loc = "modules.curl.aCurl"
  cfg <- fmap fromJust getShakeExtra
  debugA' "aCurl" $ "outPath': " ++ outPath'
  let d = takeBaseName outPath'
  debugA' "aCurl" $ "d: " ++ show d
  let urlPath' = tmpdir cfg </> "exprs" </> "str" </> d </> "result"
      urlPath = toPath loc cfg urlPath'
  debugA' "aCurl" $ "urlPath: " ++ render (pPrint urlPath)
  let cDir = fromPath loc cfg $ cacheDir cfg "curl"
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
  , mRules = [rCurl]
  , mFunctions = [curl, curlDate]
  }

-------------------------
-- ortholang functions --
-------------------------

-- | Version that auto-finds the proper date based on user input
curl :: Function
curl = newFnA2
  "curl"
  (Exactly str, Exactly str)
  (Exactly Untyped)
  (newDate1of2 "curl") -- TODO main remaining bug is that this doesn't add its outpath digest?
  [ReadsURL, ReadsFile]

-- | Hidden version that assumes the date is properly formatted
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

aCurlDate :: NewAction2
aCurlDate (ExprPath outPath') datePath' urlPath' = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "ortholang.modules.curl.aCurlDate"
      outPath = toPath loc cfg outPath'
  debugA' "aCurlDate" $ "datePath': " ++ datePath'
  debugA' "aCurlDate" $ "urlPath': " ++ urlPath'
  date <- readLit loc datePath'
  let cacheDir' = fromPath loc cfg $ cacheDir cfg "curl"
      (PathDigest urlDigest) = last $ listDigestsInPath cfg urlPath'
  debugA' "aCurlDate" $ "date: " ++ date
  debugA' "aCurlDate" $ "cacheDir': " ++ cacheDir'
  debugA' "aCurlDate" $ "urlDigest: " ++ urlDigest
  -- TODO the final output path should depend on this rather than the initial userCacheDescPath', right?
  let cachePath' = cacheDir' </> date </> urlDigest -- TODO make this a dir and add /result?
      cachePath  = toPath loc cfg cachePath'
  liftIO $ createDirectoryIfMissing True $ takeDirectory cachePath'
  need' loc [cachePath']
  debugA' "aCurlDate" $ "symlink: " ++ show outPath ++ " -> " ++ show cachePath
  symlink outPath cachePath
