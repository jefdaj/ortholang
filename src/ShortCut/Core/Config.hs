module ShortCut.Core.Config where

import Prelude             hiding (lookup)
import Data.Configurator          (load, lookup)
import Data.Configurator.Types    (Config, Worth(..))
import Data.Maybe                 (fromJust)
import Data.Text                  (pack)
import Development.Shake          (newResourceIO, command_, withResource, Action, CmdOption(..))
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Types        (CutConfig(..), CutModule(..), WrapperConfig(..))
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
loadWrapperConfig :: Arguments -> Config -> IO (Maybe WrapperConfig)
loadWrapperConfig args cfg = do
  wscr <- loadField args cfg "wrapperscript"
  case wscr of
    Nothing -> return Nothing
    Just wscr' -> do
      wlim <- loadField args cfg "wrapperlimit"
      wlim' <- case wlim of
        Nothing -> return Nothing
        Just n  -> case read n of
          0  -> return Nothing
          n' -> do
            r <- newResourceIO "wrapperlimit" n'
            return $ Just r
      return $ Just WrapperConfig
        { wrapperScript = wscr'
        , wrapperLimit  = wlim'
        }

loadConfig :: [CutModule] -> Arguments -> IO CutConfig
loadConfig mods args = do
  let path = fromJust $ getArg args $ longOption "config"
  cfg <- load [Optional path]
  csc <- loadField args cfg "script"
  ctd <- loadField args cfg "tmpdir"
  rep <- loadField args cfg "report"
  -- TODO if cluster-scripts given, activate wrappedCmd calls
  -- TODO if cluster-limit given, define wrappedCmd to use a resource
  cls <- loadWrapperConfig args cfg
  return CutConfig
    { cfgScript  = csc -- cut script, not cluster script
    , cfgTmpDir  = fromJust ctd
    , cfgDebug   = isPresent args $ longOption "debug"
    , cfgModules = mods
    , cfgWrapper = cls
    , cfgReport  = rep
    }

getUsage :: IO Docopt
getUsage = do
  path <- getDataFileName "usage.txt"
  txt  <- expandTildes =<< readFile path
  parseUsageOrExit txt

hasArg :: Arguments -> String -> Bool
hasArg as a = isPresent as $ longOption a

-- Shake's command_ adapted to work with wrapperScript and wrapperLimit if used
-- TODO gather shake stuff into a Shake.hs module?
--      could have config, debug, wrappedCmd, eval...
wrappedCmd :: CutConfig -> [CmdOption] -> FilePath -> [String] -> Action ()
wrappedCmd cfg opts bin args = case cfgWrapper cfg of
  Nothing -> command_ opts bin args
  Just wc -> case wrapperLimit wc of
    Nothing -> wCmd
    Just l  -> withResource l 1 wCmd
    where
      wCmd = command_ opts (wrapperScript wc) (bin:args)
