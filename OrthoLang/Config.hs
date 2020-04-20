{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module OrthoLang.Config where

-- TODO absolutize in the setters too? or unify them with initial loaders?

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import OrthoLang.Debug (debug)
import OrthoLang.Types (Config(..))
import OrthoLang.Util  (absolutize, justOrDie)

import Control.Logging            (LogLevel(..), setLogLevel, setDebugSourceRegex)
import Control.Monad              (when)
import Data.List                  (isPrefixOf, isInfixOf)
import Data.Maybe                 (isNothing, catMaybes)
import Data.Text                  (pack)
import Paths_OrthoLang            (getDataFileName)
import System.Console.Docopt      (Docopt, Arguments, getArg, isPresent, longOption, getAllArgs)
import System.Console.Docopt.NoTH (parseUsageOrExit)
import System.Directory           (doesFileExist)
import System.FilePath            ((</>), (<.>))
import Text.Read.HT               (maybeRead)

{- The logging module keeps its own state in an IORef, so no need to include
 - this in the main OrthoLang config below.
 - TODO still put all the config stuff in Config though, and make it changable in the repl!
 -}
dispatch :: Arguments -> String -> IO () -> IO ()
dispatch args arg act = when (isPresent args $ longOption arg) $ do
  debug' $ "handling --" ++ arg
  act

{- The base debugging function used in other modules too. This is admittedly a
 - weird place to put it, but makes everything much easier as far as avoiding
 - import cycles.
 -
 - TODO remove this and rewrite with logging module
 -}
debug' :: String -> IO ()
debug' = debug "config"

loadMaybe :: Arguments -> C.Config -> String -> IO (Maybe String)
loadMaybe args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = C.lookup cfg $ pack key

loadMaybeAbs :: Arguments -> C.Config -> String -> IO (Maybe FilePath)
loadMaybeAbs args cfg key = do
  mPath <- loadMaybe args cfg key
  case mPath of
    Nothing -> return Nothing
    Just p -> absolutize p >>= return . Just

loadAbs :: Arguments -> C.Config -> String -> IO FilePath
loadAbs args cfg key = fmap (justOrDie msg) $ loadMaybeAbs args cfg key
  where
    msg = "failed to parse --" ++ key

-- TODO deduplicate with the one in Paths.hs
isURL :: String -> Bool
isURL s = "://" `isInfixOf` take 10 s

loadConfig :: Arguments -> IO Config
loadConfig args = do
  debug' $ "docopt arguments: " ++ show args
  defaultCfg <- getDataFileName "default.cfg"
  userCfg    <- absolutize "~/.ortholang/ortholang.cfg"
  userCfgExists <- doesFileExist userCfg
  let extraCfgs = catMaybes
        [ if userCfgExists then Just userCfg else Nothing
        , getArg args $ longOption "config"
        ]
  cfg <- C.load $ C.Required defaultCfg : fmap C.Optional extraCfgs -- TODO reverse list?
  debugregex <- loadMaybe args cfg "debugregex"
  tmpdir  <- loadAbs args cfg "tmpdir"
  workdir <- loadAbs args cfg "workdir"
  script  <- loadMaybeAbs args cfg "script"
  report  <- loadMaybeAbs args cfg "report"
  wrapper <- loadMaybeAbs args cfg "wrapper"
  history <- loadMaybeAbs args cfg "history"
  outfile <- loadMaybeAbs args cfg "outfile"
  shared  <- loadMaybe args cfg "shared"
               >>= mapM (\p -> if isURL p then return p else absolutize p)
  let testpattern = getAllArgs args (longOption "test")
  let interactive = isNothing script || (isPresent args $ longOption "interactive")
  let termcolumns = Nothing -- not used except in testing
  let shellaccess = isPresent args $ longOption "shellaccess" -- TODO clean up
  let progressbar = isPresent args $ longOption "progressbar" -- TODO clean up
  let showhidden  = isPresent args $ longOption "showhidden" -- TODO clean up
  let res = Config
              { script
              , interactive
              , debugregex
              , wrapper
              , history
              , report
              , testpattern
              , termcolumns
              , shellaccess
              , progressbar
              , outfile
              , shared
              , tmpdir
              , workdir
              , showhidden
              }
  debug' $ show res
  updateDebug debugregex
  return res

-- TODO any way to recover if missing? probably not
-- TODO use a safe read function with locks here?
getDoc :: [FilePath] -> IO String -- TODO IO (Maybe String)?
getDoc docPaths = do
  paths' <- mapM (\p -> getDataFileName ("docs" </> p <.> "txt") >>= absolutize) $ docPaths
  tests <- mapM doesFileExist paths'
  -- let path' = listToMaybe [p | (p, t) <- zip paths' tests, t] -- TODO remove head?
  let path' = head [p | (p, t) <- zip paths' tests, t] -- TODO remove head?
  -- putStrLn $ "path':" ++ path'
  -- this should only happen during development:
  -- written <- doesFileExist path'
  -- when (not written) $ writeFile path' $ "write " ++ docPath ++ " doc here"
  -- mapM readFile path'
  doc <- readFile path'
  return doc

getUsage :: IO Docopt
-- getUsage = getDoc ["usage"] >>= parseUsageOrExit . fromJust
getUsage = getDoc ["usage"] >>= parseUsageOrExit

-- hasArg :: Arguments -> String -> Bool
-- hasArg as a = isPresent as $ longOption a

-------------------------
-- getters and setters --
-------------------------

{- These are done the simple, repetitive way for now to avoid lenses.  That
 - might change in the future though, because turns out getters and setters are
 - horrible!
 -
 - Note that secure is purposely not avialable here.
 -}

-- This is mainly for use in the REPL so no need to return usable data
-- TODO just show the whole config; no need for fields
-- TODO oh and there's a display function for that!
-- showConfigField :: Config -> String -> String
-- showConfigField cfg key = case lookup key fields of
--   Nothing -> "no such config setting: " ++ key
--   Just (getter, _) -> getter cfg

setConfigField :: Config -> String -> String -> Either String (IO Config)
setConfigField cfg key val = case lookup key fields of
  Nothing -> Left $ "no such config setting: " ++ key
  Just setter -> setter cfg val

-- TODO add modules? maybe not much need
-- TODO add interactive?
-- TODO these show* functions could be Pretty instances, or just directly showable
-- TODO remove anything that can't be shown
-- TODO remove show functions and show directly (possibly using Configurator.display)
fields :: [(String, (Config -> String -> Either String (IO Config)))]
fields =
  [ ("script" , setScript )
  , ("tmpdir" , setTmpdir )
  , ("workdir", setWorkdir)
  , ("debug"  , setDebug  )
  , ("wrapper", setWrapper)
  , ("report" , setReport )
  , ("termcolumns", setWidth  )
  , ("outfile" , setOutFile)
  -- TODO add share?
  ]

-- showConfig :: Config -> String
-- showConfig cfg = unlines $ map showField fields
--   where
--     showField (name, (getter, _)) = name ++ " = " ++ getter cfg

setDebug :: Config -> String -> Either String (IO Config)
setDebug cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ do
    updateDebug $ Just v
    return $ cfg { debugregex = Just v }

-- this is run during setDebug, and once in loadConfig
updateDebug :: Maybe String -> IO ()
updateDebug regex = case regex of
  Nothing -> do
    debug' "turning off debugging"
    setLogLevel LevelWarn
  Just r -> do
    setLogLevel LevelDebug
    setDebugSourceRegex r
    debug' $ "set debug regex to " ++ show regex

-- TODO this seems ok, it just needs to be refactored a bit to have one parser per type:
--      string, boolean, int
--      then for strings have it strip off optional quotes

-- standard string
setScript :: Config -> String -> Either String (IO Config)
setScript cfg "Nothing" = Right $ return $ cfg { script = Nothing }
setScript cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { script = Just v }

-- standard string
setTmpdir :: Config -> String -> Either String (IO Config)
setTmpdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { tmpdir = v }

-- standard string
setWorkdir :: Config -> String -> Either String (IO Config)
setWorkdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { workdir = v }

-- standard string
setWrapper :: Config -> String -> Either String (IO Config)
setWrapper cfg "Nothing" = Right $ return $ cfg { wrapper = Nothing }
setWrapper cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { wrapper = Just v }

-- standard string
setReport :: Config -> String -> Either String (IO Config)
setReport cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  v       -> Right $ return $ cfg { report = v }

-- TODO remove?
-- setThreads :: Config -> String -> Either String (IO Config)
-- setThreads cfg val = case maybeRead val of
  -- Nothing -> Left  $ "invalid: " ++ val
  -- Just v  -> Right $ return $ cfg { cfgThreads = v }

-- standard int
setWidth :: Config -> String -> Either String (IO Config)
setWidth cfg "Nothing" = Right $ return $ cfg { termcolumns = Nothing }
setWidth cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just n  -> Right $ return $ cfg { termcolumns = Just n }

-- standard string
setOutFile :: Config -> String -> Either String (IO Config)
setOutFile cfg "Nothing" = Right $ return $ cfg { outfile = Nothing }
setOutFile cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { outfile = Just v }

-- TODO add: progressbar, hiddenfns, etc (Bools)
-- TODO add logfile and have it update that (move from main)
