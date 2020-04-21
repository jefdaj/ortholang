{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Logging

import Data.IORef            (newIORef)
import Data.Version          (showVersion)
import OrthoLang.Interpreter        (runRepl, evalFile)
import OrthoLang.Config (getUsage, loadConfig, dispatch)
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

-- TODO does this work everywhere?
-- forceUtf8 = do
--   _ <- setLocale LC_ALL $ Just "en_US.UTF-8"
--   setEnv "LANG" "en_US.UTF-8" -- TODO and is this part superfluous now?
--   hSetBuffering stdin  LineBuffering
--   hSetBuffering stdout LineBuffering

main:: IO ()
main = do

  -- warning: logging to file doesn't work in this section
  (args, cfg) <- withStderrLogging $ do
    setLogTimeFormat "[%Y-%m-%d %H:%M:%S %q]"
    usage <- getUsage
    args  <- parseArgsOrExit usage =<< getArgs
    dispatch args "help" $ exitWithUsage usage
    dispatch args "version" $ do
      putStrLn $ "OrthoLang " ++ showVersion version
      exitSuccess
    cfg <- loadConfig args
    return (args, cfg)

  -- from here on, logging to file is ok
  withFileLogging (logfile cfg) $ do
    setLogTimeFormat "[%Y-%m-%d %H:%M:%S %q]"
    debugS' "ortholang.main" "started logging"

    setEnv "TMPDIR" $ tmpdir cfg -- for subprocesses like R
    ref <- initLocks
    setCurrentDirectory $ workdir cfg
    ids <- newIORef emptyIDs
    dRef <- newIORef emptyDigests

    dispatch args "test" $ do
      -- args is used here to set up the Tasty environment vars
      runTests args (cfg {termcolumns = Just 100}) ref ids dRef
      exitSuccess

    -- TODO typecheck only option here
    let initialState = (emptyScript, cfg, ref, ids, dRef)
    if (interactive cfg)
      then runRepl  modules initialState
      else evalFile modules initialState stdout

    -- TODO is it a problem that --test never reaches this?
    debug "finished main"
