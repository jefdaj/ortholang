module Main where

import System.Locale.SetLocale
import qualified Data.Map as M

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
import ShortCut.Reference    (writeReference)
import System.Console.Docopt (exitWithUsage, parseArgsOrExit)
import System.Environment    (getArgs, withArgs)
import System.Exit           (exitSuccess)
import System.IO             (hSetBuffering, BufferMode(..), stdin, stdout)
import System.Directory      (setCurrentDirectory)
import Data.IORef            (newIORef)
import System.Environment    (setEnv)

main:: IO ()
main = do
  -- TODO does this work on all systems with the Nix package?
  _ <- setLocale LC_ALL $ Just "C"
  setEnv "LANG" "C"
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  -- parse config
  usage <- getUsage
  args  <- parseArgsOrExit usage =<< getArgs
  when (hasArg args "help")
    (exitWithUsage usage)
  when (hasArg args "version")
    (putStrLn ("ShortCut " ++ showVersion version) >> exitSuccess)
  when (hasArg args "reference")
    (writeReference >> exitSuccess)
  cfg <- loadConfig modules args
  setEnv "TMPDIR" $ cfgTmpDir cfg -- for subprocesses like R

  ref <- initLocks
  when (cfgDebug cfg) $ putStrLn $ "config: " ++ show cfg
  setCurrentDirectory $ cfgWorkDir cfg
  ids <- newIORef M.empty
  when (hasArg args "test")
    (withArgs [] $ runTests (cfg {cfgWidth = Just 100}) ref ids)
  if (hasArg args "script" && (not $ hasArg args "interactive"))
    then evalFile stdout cfg ref ids
    else runRepl  cfg ref ids
