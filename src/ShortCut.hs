module Main where

import Control.Monad         (when)
import Data.Version          (showVersion)
import Paths_ShortCut        (version)
import ShortCut.Core         (runRepl, evalFile)
import ShortCut.Core.Config  (getUsage, loadConfig, hasArg)
import ShortCut.Core.Debug   (debug)
import ShortCut.Modules      (modules)
import ShortCut.Test         (runTests)
import System.Console.Docopt (exitWithUsage, parseArgsOrExit)
import System.Environment    (getArgs, withArgs)
import System.Exit           (exitSuccess)
import System.IO             (stdin, stdout, hSetBuffering, BufferMode(..))

-- TODO --interactive and --script should work at the same time

main:: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  usage <- getUsage
  args  <- parseArgsOrExit usage =<< getArgs
  when (hasArg args "help")
    (exitWithUsage usage)
  when (hasArg args "version")
    (putStrLn ("ShortCut " ++ showVersion version) >> exitSuccess)
  when (hasArg args "test")
    -- TODO allow passing args to tasty here if not too hard
    -- TODO still run other stuff after tests?
    (withArgs [] $ runTests modules >> exitSuccess)
  cfg <- loadConfig modules args
  let cfg' = debug cfg ("config: " ++ show cfg) cfg
  if (hasArg args "script" && (not $ hasArg args "interactive"))
    then evalFile stdout cfg'
    else runRepl  cfg'