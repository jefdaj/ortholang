{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Logging

import Data.IORef            (newIORef)
import Data.Version          (showVersion)
import OrthoLang.Interpreter        (runRepl, evalFile)
import OrthoLang.Interpreter.Config (getUsage, loadConfig, dispatch)
import OrthoLang.Locks  (initLocks)
import OrthoLang.Types  (Config(..), emptyDigests, emptyScript, emptyIDs)
import OrthoLang.Modules     (modules)
import OrthoLang.Test        (runTests)
import Paths_OrthoLang       (version)
import System.Console.Docopt (exitWithUsage, parseArgsOrExit)
import System.Directory      (setCurrentDirectory)
import System.Environment    (getArgs)
import System.Environment    (setEnv)
import System.Exit           (exitSuccess)
import System.IO             (stdout)

-- debug :: Text -> IO ()
-- debug = debugS "ortholang.main"

-- TODO does this work everywhere?
-- forceUtf8 = do
--   _ <- setLocale LC_ALL $ Just "en_US.UTF-8"
--   setEnv "LANG" "en_US.UTF-8" -- TODO and is this part superfluous now?
--   hSetBuffering stdin  LineBuffering
--   hSetBuffering stdout LineBuffering

-- TODO any good way to configure logging from the beginning, before docopt?
-- TODO switch to stderr? withStderrLogging dumps to console,
--      but lets you run main from ghci more than once
main:: IO ()
main = withFileLogging "ortholang.log" $ do
  setLogTimeFormat "[%Y-%m-%d %H:%M:%S %q]"
  debugS' "ortholang.main" "starting main"

  usage <- getUsage
  args  <- parseArgsOrExit usage =<< getArgs

  dispatch args "help" $ exitWithUsage usage

  dispatch args "version" $ do
    putStrLn $ "OrthoLang " ++ showVersion version
    exitSuccess

  cfg <- loadConfig modules args
  setEnv "TMPDIR" $ cfgTmpDir cfg -- for subprocesses like R
  ref <- initLocks
  setCurrentDirectory $ cfgWorkDir cfg
  ids <- newIORef emptyIDs
  dRef <- newIORef emptyDigests

  dispatch args "test" $ do
    -- args is used here to set up the Tasty environment vars
    runTests args (cfg {cfgWidth = Just 100}) ref ids dRef
    exitSuccess

  -- TODO typecheck only option here
  let initialState = (emptyScript, cfg, ref, ids, dRef)
  if (cfgInteractive cfg)
    then runRepl  initialState
    else evalFile initialState stdout

  -- TODO is it a problem that --test never reaches this?
  debug "finished main"
