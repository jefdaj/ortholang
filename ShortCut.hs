{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad              (when)
import Data.Configurator          (load, lookup)
import Data.Configurator.Types    (Config, Worth(..))
import Data.Maybe                 (fromJust, fromMaybe)
import Data.Text                  (pack)
import Data.Version               (showVersion)
import Paths_ShortCut             (version, getDataFileName)
import Prelude             hiding (lookup)
import ShortCut.Core              (runRepl, CutConfig(..), evalFile)
import ShortCut.Core.Util         (expandTildes)
import ShortCut.Test              (runTests)
import System.Console.Docopt      (Docopt, Arguments, exitWithUsage,
                                   getArg, isPresent, longOption, parseArgsOrExit)
import System.Console.Docopt.NoTH (parseUsageOrExit)
import System.Environment         (getArgs, withArgs)
import System.Exit                (exitSuccess)

-- TODO separate Config.hs, but only if it can actually be separated

loadField :: Arguments -> Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = lookup cfg $ pack key

loadConfig :: Arguments -> IO CutConfig
loadConfig args = do
  let path = fromJust $ getArg args $ longOption "config"
  -- putStrLn $ show args
  cfg <- load [Optional path]
  csc <- loadField args cfg "script"
  ctd <- loadField args cfg "tmpdir"
  cvb <- loadField args cfg "verbose"
  return CutConfig
    { cfgScript  = csc
    , cfgTmpDir  = fromJust ctd
    , cfgVerbose = read $ fromMaybe "False" cvb -- TODO why is this needed?
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
  usage <- getUsage
  args <- parseArgsOrExit usage =<< getArgs
  when (hasArg args "help")
    (exitWithUsage usage)
  when (hasArg args "version")
    (putStrLn ("ShortCut " ++ showVersion version) >> exitSuccess)
  when (hasArg args "test")
    -- TODO allow passing args to tasty here if not too hard
    (withArgs [] $ runTests >> exitSuccess)
  cfg <- loadConfig args
  if (hasArg args "script" && (not $ hasArg args "interactive"))
    then evalFile cfg
    else runRepl cfg
