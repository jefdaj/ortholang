module Main where

import qualified Data.Map as M

import Control.Monad         (when)
import Data.Version          (showVersion)
import Paths_Detourrr        (version)
import Detourrr.Core         (runRepl, evalFile)
import Detourrr.Core.Config  (getUsage, loadConfig, hasArg)
-- import Detourrr.Core.Debug   (debug)
import Detourrr.Core.Types   (CutConfig(..))
import Detourrr.Core.Locks   (initLocks)
import Detourrr.Modules      (modules)
import Detourrr.Test         (runTests)
import Detourrr.Reference    (writeReference)
import System.Console.Docopt (exitWithUsage, parseArgsOrExit)
import System.Environment    (getArgs, withArgs)
import System.Exit           (exitSuccess)
import System.IO             (stdout)
import System.Directory      (setCurrentDirectory)
import Data.IORef            (newIORef)

main:: IO ()
main = do
  usage <- getUsage
  args  <- parseArgsOrExit usage =<< getArgs
  when (hasArg args "help")
    (exitWithUsage usage)
  when (hasArg args "version")
    (putStrLn ("Detourrr " ++ showVersion version) >> exitSuccess)
  when (hasArg args "reference")
    (writeReference >> exitSuccess)
  cfg <- loadConfig modules args
  ref <- initLocks
  when (cfgDebug cfg) $ putStrLn $ "config: " ++ show cfg
  setCurrentDirectory $ cfgWorkDir cfg
  ids <- newIORef M.empty
  when (hasArg args "test")
    (withArgs [] $ runTests (cfg {cfgWidth = Just 100}) ref ids)
  if (hasArg args "script" && (not $ hasArg args "interactive"))
    then evalFile stdout cfg ref ids
    else runRepl  cfg ref ids
