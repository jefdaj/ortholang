module OrthoLang.Modules.Curl
  (

  -- * Function for use in other modules
    curl
  , isURL

  )
  where

import OrthoLang.Core

import Control.Monad.IO.Class     (liftIO)
import Data.List                  (isInfixOf)
import Data.Maybe                 (fromJust)
import Development.Shake          (Action, getShakeExtra)
import Development.Shake.FilePath ((</>))
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))

-- This is hacky, but should work with multiple protocols like http(s):// and ftp://
isURL :: String -> Bool
isURL s = "://" `isInfixOf` take 10 s

curl :: String -> Action Path
curl url = do
  cfg <- fmap fromJust getShakeExtra
  let loc = "modules.curl.curl"
      cDir    = fromPath loc cfg $ cacheDir cfg "curl"
      outPath = cDir </> digest url
  liftIO $ createDirectoryIfMissing True cDir
  runCmd $ CmdDesc
    { cmdBinary = "curl.sh"
    , cmdArguments = [outPath, url]
    , cmdFixEmpties = False
    , cmdParallel = False
    , cmdInPatterns = []
    , cmdOutPath = outPath
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath]
    }
  return $ toPath loc cfg outPath
