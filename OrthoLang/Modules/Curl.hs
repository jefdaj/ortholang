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

import Control.Monad.IO.Class     (liftIO)
import Data.Maybe                 (fromJust)
import Development.Shake          (Action, getShakeExtra)
import Development.Shake.FilePath ((</>))
import System.Directory           (createDirectoryIfMissing)
import System.Exit                (ExitCode(..))

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

---------------
-- curl_date --
---------------

curlDate :: Function
curlDate = newExprExpansion
  "curl_date"
  [Exactly str, Exactly str]
  (Exactly Untyped)
  mCurlDate
  [ReadsURL]

mCurlDate :: ExprExpansion
mCurlDate _ _ (Fun r ms ds n [dateStr, urlStr]) = undefined
  where
    datePath = undefined
mCurlDate _ _ e = error $ "bad argument to mCurlDate: " ++ show e

-- mOrthologInStr :: String -> ExprExpansion
-- mOrthologInStr name _ _ (Fun rType seed deps _  [groups , faas]) =
--   Fun rType seed deps (name ++ "_str")   [groups', faas']
--   where
--     groups' = Fun sll seed (depsOf groups) "orthogroups"      [groups]
--     faas'   = Fun sll seed (depsOf faas  ) "extract_ids_each" [faas]
-- mOrthologInStr _ _ _ _ = error "bad arguments to mOrthologInStr"
