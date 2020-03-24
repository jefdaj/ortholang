{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Locale.SetLocale
import qualified Data.Map.Strict as M

import Data.Version          (showVersion)
import Paths_OrthoLang        (version)
import OrthoLang.Core         (runRepl, evalFile)
import OrthoLang.Core.Config  (getUsage, loadConfig, dispatch)
-- import OrthoLang.Core.Debug   (debug)
import OrthoLang.Core.Types   (OrthoLangConfig(..), HashedIDs(..))
import OrthoLang.Core.Locks   (initLocks)
import OrthoLang.Modules      (modules)
import OrthoLang.Test         (runTests)
import System.Console.Docopt (exitWithUsage, parseArgsOrExit)
import System.Environment    (getArgs)
import System.Exit           (exitSuccess)
-- import System.IO             (hSetBuffering, BufferMode(..), stdin, stdout)
import System.IO             (stdout)
import System.Directory      (setCurrentDirectory)
import Data.IORef            (newIORef)
import System.Environment    (setEnv)

-- import Data.Text (pack)
-- import Data.Text.Internal (Text)
import Control.Logging -- (withFileLogging)

-- debug :: Text -> IO ()
-- debug = debugS "ortholang.main"

-- TODO any good way to configure logging from the beginning, before docopt?
main:: IO ()
main = withFileLogging "ortholang.log" $ do
  setLogTimeFormat "[%Y-%m-%d %H:%M:%S %q]"
  debugS' "ortholang.main" "starting"

  -- TODO does this work everywhere?
  -- _ <- setLocale LC_ALL $ Just "en_US.UTF-8"
  -- setEnv "LANG" "en_US.UTF-8" -- TODO and is this part superfluous now?
  -- hSetBuffering stdin  LineBuffering
  -- hSetBuffering stdout LineBuffering

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
  ids <- newIORef $ HashedIDs M.empty M.empty M.empty

  -- TODO should these be mutually exclusive?

  -- TODO hide this from users?
  -- when (hasArg args "reference") $ do
    -- debug "handling --reference"
  -- dispatch args "reference" $ do
  --   writeReference
  --   writeDocPlaceholders (cfgModules cfg)
  --   exitSuccess -- TODO combine into one docs dir

  dispatch args "test" $ do
    -- args is used here to set up the Tasty environment vars
    runTests args (cfg {cfgWidth = Just 100}) ref ids
    exitSuccess

  -- TODO typecheck only option here
  if (cfgInteractive cfg)
    then runRepl cfg ref ids -- TODO take an entire state here too?
    else evalFile ([], cfg, ref, ids) stdout

  -- TODO is it a problem that --test never reaches this?
  -- debug "finished main"
