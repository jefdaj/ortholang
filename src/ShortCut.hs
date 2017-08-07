module Main where

import ShortCut.Core.Debug        (debug)
import Control.Monad              (when)
import Data.Configurator          (load, lookup)
import Data.Configurator.Types    (Config, Worth(..))
import Data.Maybe                 (fromJust)
import Data.Text                  (pack)
import Data.Version               (showVersion)
import Paths_ShortCut             (version, getDataFileName)
import Prelude             hiding (lookup)
import ShortCut.Core              (runRepl, CutConfig(..), CutModule(..), evalFile)
import ShortCut.Core.Util         (expandTildes)
import ShortCut.Modules           (modules)
import ShortCut.Test              (runTests)
import System.Console.Docopt      (Docopt, Arguments, exitWithUsage,
                                   getArg, isPresent, longOption, parseArgsOrExit)
import System.Console.Docopt.NoTH (parseUsageOrExit)
import System.Environment         (getArgs, withArgs)
import System.Exit                (exitSuccess)
import Control.Monad.IO.Class   (liftIO)
import System.IO

loadField :: Arguments -> Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = lookup cfg $ pack key

loadConfig :: [CutModule] -> Arguments -> IO CutConfig
loadConfig mods args = do
  let path = fromJust $ getArg args $ longOption "config"
  cfg <- load [Optional path]
  csc <- loadField args cfg "script"
  ctd <- loadField args cfg "tmpdir"
  return CutConfig
    { cfgScript  = csc
    , cfgTmpDir  = fromJust ctd
    , cfgDebug   = isPresent args $ longOption "debug"
    , cfgModules = mods
    }

getUsage :: IO Docopt
getUsage = do
  path <- getDataFileName "usage.txt"
  txt  <- expandTildes =<< readFile path
  parseUsageOrExit txt

hasArg :: Arguments -> String -> Bool
hasArg as a = isPresent as $ longOption a

main:: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  usage <- getUsage
  args <- parseArgsOrExit usage =<< getArgs
  when (hasArg args "help")
    (exitWithUsage usage)
  when (hasArg args "version")
    (putStrLn ("ShortCut " ++ showVersion version) >> exitSuccess)
  when (hasArg args "test")
    -- TODO allow passing args to tasty here if not too hard
    (withArgs [] $ runTests modules >> exitSuccess)
  cfg <- loadConfig modules args
  let cfg' = debug cfg ("config: " ++ show cfg) cfg
  if (hasArg args "script" && (not $ hasArg args "interactive"))
    then evalFile (liftIO . putStrLn) cfg'
    else runRepl  cfg'
