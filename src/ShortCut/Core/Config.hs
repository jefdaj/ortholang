module ShortCut.Core.Config where

import Prelude             hiding (lookup)
import Data.Configurator          (load, lookup)
import Data.Configurator.Types    (Config, Worth(..))
import Data.Maybe                 (fromJust)
import Data.Text                  (pack)
import Development.Shake          (newResourceIO)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core              (CutConfig(..), CutModule(..), ClusterConfig(..))
import ShortCut.Core.Util         (expandTildes)
import System.Console.Docopt      (Docopt, Arguments, getArg, isPresent, longOption)
import System.Console.Docopt.NoTH (parseUsageOrExit)

loadField :: Arguments -> Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = lookup cfg $ pack key

-- TODO how to enforce via docopt that if limit is given, script is too?
-- Note that cfg here is an intermediate docopt thing, not the same cfg that
-- gets passed around everywhere else in ShortCut.
loadClusterConfig :: Arguments -> Config -> IO (Maybe ClusterConfig)
loadClusterConfig args cfg = do
  cscr <- loadField args cfg "cluster-script"
  case cscr of
    Nothing -> return Nothing
    Just cscr' -> do
      clim <- loadField args cfg "cluster-limit"
      clim' <- case clim of
        Nothing -> return Nothing
        Just n  -> do
          r <- newResourceIO "cluster-limit" $ read n
          return $ Just r
      return $ Just ClusterConfig
        { clusterScript = cscr'
        , clusterLimit  = clim'
        }

loadConfig :: [CutModule] -> Arguments -> IO CutConfig
loadConfig mods args = do
  let path = fromJust $ getArg args $ longOption "config"
  cfg <- load [Optional path]
  csc <- loadField args cfg "script"
  ctd <- loadField args cfg "tmpdir"
  -- TODO if cluster-scripts given, activate clusterCmd calls
  -- TODO if cluster-limit given, define clusterCmd to use a resource
  cls <- loadClusterConfig args cfg
  return CutConfig
    { cfgScript  = csc -- cut script, not cluster script
    , cfgTmpDir  = fromJust ctd
    , cfgDebug   = isPresent args $ longOption "debug"
    , cfgModules = mods
    , cfgCluster = cls
    }

getUsage :: IO Docopt
getUsage = do
  path <- getDataFileName "usage.txt"
  txt  <- expandTildes =<< readFile path
  parseUsageOrExit txt

hasArg :: Arguments -> String -> Bool
hasArg as a = isPresent as $ longOption a
