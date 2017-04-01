{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad           (when)
import Data.Configurator       (load, lookup)
import Data.Configurator.Types (Config, Worth(..))
import Data.Maybe              (fromJust, fromMaybe)
import Data.Text               (pack)
import Data.Version            (showVersion)
import Paths_ShortCut          (version)
import Prelude          hiding (lookup)
import ShortCut.Core           (repl, CutConfig(..))
import ShortCut.Tests          (spec)
import System.Console.Docopt   (Docopt, docoptFile, Arguments, exitWithUsage,
                                getArg, isPresent, longOption, parseArgsOrExit)
import System.Environment      (getArgs, withArgs)
import System.Exit             (exitSuccess)
import Test.Hspec              (hspec)

-- TODO separate Config.hs, but only if it can actually be separated

loadField :: Arguments -> Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = lookup cfg $ pack key

loadConfig :: Arguments -> IO CutConfig
loadConfig args = do
  let path = fromJust $ getArg args $ longOption "config"
  putStrLn $ show args
  cfg <- load [Optional path]
  csc <- loadField args cfg "script"
  cwd <- loadField args cfg "workdir"
  ctd <- loadField args cfg "tmpdir"
  cvb <- loadField args cfg "verbose"
  return CutConfig
    { cfgScript  = csc
    , cfgWorkDir = fromJust cwd
    , cfgTmpDir  = fromJust ctd
    , cfgVerbose = read $ fromMaybe "False" cvb -- TODO why is this needed?
    }

runScript :: CutConfig -> IO ()
runScript _ = undefined -- TODO write this
-- TODO codify/explain the "result" file a little more

usage :: Docopt
usage = [docoptFile|usage.txt|]

hasArg :: Arguments -> String -> Bool
hasArg as a = isPresent as $ longOption a

main:: IO ()
main = do
  args <- parseArgsOrExit usage =<< getArgs
  when (hasArg args "help")
    (exitWithUsage usage)
  when (hasArg args "version")
    (putStrLn ("ShortCut " ++ showVersion version) >> exitSuccess)
  when (hasArg args "test")
    -- TODO allow passing args to hspec here if not too hard
    (withArgs [] $ hspec spec >> exitSuccess)
  cfg <- loadConfig args
  if (hasArg args "script" && (not $ hasArg args "interactive"))
    then (runScript cfg)
    else (repl cfg)
