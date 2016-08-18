module Main where

import System.Console.Docopt.NoTH

import Control.Monad      (when)
import Data.Map           (toList)
import ShortCut.Repl      (repl)
import ShortCutSpec       (spec)
import System.Environment (getArgs)
import System.Exit        (exitSuccess)
import Test.Hspec         (hspec)

gotLong :: Arguments -> String -> Bool
gotLong args arg = isPresent args $ longOption arg

gotEither :: Arguments -> Char -> String -> Bool
gotEither args short long
  = isPresent args (shortOption short)
 || gotLong args long 

-- TODO if given a file, run iFile + eval on it
-- TODO if no commands given, enter the REPL

printVersion :: IO ()
printVersion = putStrLn "ShortCut 0.7 \"De Pijp\""

printUsage :: Docopt -> IO ()
printUsage = putStrLn . usage

runTests :: IO ()
runTests = hspec spec

main:: IO ()
main = do
  -- TODO would the quasiquoted version be easier to deploy?
  help <- parseUsageOrExit =<< readFile "usage.txt"
  args <- parseArgsOrExit help =<< getArgs
  let gotL = gotLong   args
      gotE = gotEither args

  -- TODO parse SCRIPT, WDIR, CDIR, verbosity into a config

  when (gotE 'h' "help"       ) (printUsage help >> exitSuccess)
  when (gotL     "version"    ) (printVersion    >> exitSuccess)
  when (gotE 't' "test"       ) runTests
  when (gotE 'i' "interactive") repl

  -- TODO remove once CLI works
  putStrLn $ "parsed these arguments:"
  mapM_ (putStrLn . ("  " ++) .show) $ toList args
  putStrLn ""


  -- when (args `isPresent` (shortOption 't')) $ hspec spec
  -- TODO how to get docopt to abort with help on --help?
  -- ifArg args "h" $ putStrLn $ usage help
  -- when (args `isPresent` (command "test")) $ hspec spec
  -- when (args `isPresent` (command "i"   )) $ repl
