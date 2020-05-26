module OrthoLang.Modules.Curl
  (

  -- * Function for use in other modules
    curl

  )
  where

import OrthoLang.Types
import OrthoLang.Interpreter

import Control.Monad.IO.Class     (liftIO)
-- import Data.List                  (isInfixOf)
import Data.Maybe                 (fromJust)
import Development.Shake          (Action, getShakeExtra)
import Development.Shake.FilePath ((</>))
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))

-- TODO should a URL also be a valid Path?
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
    , cmdOutPath = outPath
    , cmdExtraOutPaths = []
    , cmdSanitizePaths = []
    , cmdOptions = []
    , cmdExitCode = ExitSuccess
    , cmdRmPatterns = [outPath]
    }
  return $ toPath loc cfg outPath
