{-# LANGUAGE OverloadedStrings #-}

-- TODO show sharedir

module OrthoLang.Config where

-- TODO absolutize in the setters too? or unify them with initial loaders?

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import OrthoLang.Debug (debug)
import OrthoLang.Types (Config(..), Module(..))
import OrthoLang.Util  (absolutize, justOrDie)

import Control.Logging            (LogLevel(..), setLogLevel, setDebugSourceRegex)
import Control.Monad              (when)
import Data.List                  (isPrefixOf)
import Data.Maybe                 (isNothing)
import Data.Text                  (pack)
import Development.Shake          (newResourceIO)
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

loadField :: Arguments -> C.Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = C.lookup cfg $ pack key

defaultConfig :: FilePath -> FilePath -> IO Config
defaultConfig td wd = do
  -- cp <- getNumProcessors
  return Config
    { script      = Nothing
    , interactive = False
    , tmpdir      = td
    , workdir     = wd
    , debugregex       = Nothing
    , cfgModules     = [] -- TODO fix this
    , wrapper     = Nothing
    , outfile     = Nothing
    , sharedir       = Nothing
    , report      = Nothing
    , testpattern     = [] -- [] means run all tests
    , termcolumns       = Nothing
    , secure      = False
    , progressbar      = True
    -- , cfgParLock     = par
    -- , cfgThreads     = cp
    , showhidden     = False
    }

loadConfig :: [Module] -> Arguments -> IO Config
loadConfig mods args = do
  debug' $ "docopt arguments: " ++ show args
  let path = justOrDie "parse --config arg failed!" $ getArg args $ longOption "config"
  cfg <- C.load [C.Optional path]
  csc <- loadField args cfg "script"
  csc' <- case csc of
            Nothing -> return Nothing
            Just s  -> absolutize s >>= return . Just
  dbg <- loadField args cfg "debug"
  ctd <- mapM absolutize =<< loadField args cfg "tmpdir"
  cwd <- mapM absolutize =<< loadField args cfg "workdir"
  let ctd' = justOrDie "parse --tmpdir arg failed!" ctd
      cwd' = justOrDie "parse --workdir arg failed!" cwd
  def <- defaultConfig ctd' cwd'
  rep <- mapM absolutize =<< loadField args cfg "report"
  cls <- mapM absolutize =<< loadField args cfg "wrapper"
  out <- mapM absolutize =<< loadField args cfg "output"
  shr <- mapM (\p -> if "http" `isPrefixOf` p then return p else absolutize p) =<< loadField args cfg "shared"
  let ctp = getAllArgs args (longOption "test")
  let int = isNothing csc' || (isPresent args $ longOption "interactive")
  let res = def
              { script  = csc'
              , interactive = int
              , debugregex   = dbg
              , cfgModules = mods
              , wrapper = cls
              , report  = rep
              , testpattern = ctp
              , termcolumns   = Nothing -- not used except in testing
              , secure  = isPresent args $ longOption "secure"
              , progressbar  = isPresent args $ longOption "noprogress"
              , outfile = out
              , sharedir   = shr
              }
  debug' $ show res
  updateDebug dbg
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
  , ("width"  , setWidth  )
  , ("output" , setOutFile)
  -- , ("threads", setThreads)
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
