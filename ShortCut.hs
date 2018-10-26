module Main where

import Control.Monad         (when)
import Data.Version          (showVersion)
import Paths_ShortCut        (version)
import ShortCut.Core         (runRepl, evalFile)
import ShortCut.Core.Config  (getUsage, loadConfig, hasArg)
-- import ShortCut.Core.Debug   (debug)
import ShortCut.Core.Types   (CutConfig(..))
import ShortCut.Core.Locks   (initLocks)
import ShortCut.Modules      (modules)
import ShortCut.Test         (runTests)
import System.Console.Docopt (exitWithUsage, parseArgsOrExit)
import System.Environment    (getArgs, withArgs)
import System.Exit           (exitSuccess)
import System.IO             (stdout)
import System.Directory      (setCurrentDirectory)

main:: IO ()
main = do
  usage <- getUsage
  args  <- parseArgsOrExit usage =<< getArgs
  when (hasArg args "help")
    (exitWithUsage usage)
  when (hasArg args "version")
    (putStrLn ("ShortCut " ++ showVersion version) >> exitSuccess)
  cfg <- loadConfig modules args
  ref <- initLocks
  when (cfgDebug cfg) $ putStrLn $ "config: " ++ show cfg
  setCurrentDirectory $ cfgWorkDir cfg
  when (hasArg args "test")
    (withArgs [] $ runTests (cfg {cfgWidth = Just 100}) ref)
  if (hasArg args "script" && (not $ hasArg args "interactive"))
    then evalFile stdout cfg ref
    else runRepl  cfg ref