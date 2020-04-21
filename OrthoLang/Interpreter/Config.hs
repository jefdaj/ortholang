{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OrthoLang.Interpreter.Config where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import OrthoLang.Debug (debug)
import OrthoLang.Types (Config(..))
import OrthoLang.Util  (absolutize, headOrDie, justOrDie, retryIncSuffix)

import Control.Logging            (LogLevel(..), setLogLevel, setDebugSourceRegex, setLogFile)
import Control.Monad              (when)
import Data.Char                  (toUpper, toLower)
import Data.List                  (isInfixOf)
import Data.Maybe                 (isNothing, catMaybes, fromJust)
import Data.Text                  (pack)
import Paths_OrthoLang            (getDataFileName)
import System.Console.Docopt      (Docopt, Arguments, getArg, isPresent, longOption)
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
  -- debug' $ "handling --" ++ arg
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

loadMaybeInt :: Arguments -> C.Config -> String -> IO (Maybe Int)
loadMaybeInt args cfg key = do
  mInt <- loadMaybe args cfg key
  case mInt of
    Nothing -> return Nothing
    Just s  -> return $ maybeRead s

loadAbs :: Arguments -> C.Config -> String -> IO FilePath
loadAbs args cfg key = fmap (justOrDie msg) $ loadMaybeAbs args cfg key
  where
    msg = "failed to parse --" ++ key

-- TODO deduplicate with the one in Paths.hs
isURL :: String -> Bool
isURL s = "://" `isInfixOf` take 10 s

loadConfig :: Arguments -> IO Config
loadConfig args = do
  -- debug' $ "docopt arguments: " ++ show args
  defaultCfg <- getDataFileName "default.cfg"
  userCfg    <- absolutize "~/.ortholang/ortholang.cfg"
  userCfgExists <- doesFileExist userCfg
  let extraCfgs = catMaybes
        [ if userCfgExists then Just userCfg else Nothing
        , getArg args $ longOption "config"
        ]
  cfg <- C.load $ C.Required defaultCfg : fmap C.Optional extraCfgs -- TODO reverse list?
  debugregex  <- loadMaybe    args cfg "debugregex"
  tmpdir      <- loadAbs      args cfg "tmpdir"
  workdir     <- loadAbs      args cfg "workdir"
  logfile     <- loadAbs      args cfg "logfile"
  script      <- loadMaybeAbs args cfg "script"
  report      <- loadMaybeAbs args cfg "report"
  wrapper     <- loadMaybeAbs args cfg "wrapper"
  history     <- loadMaybeAbs args cfg "history"
  outfile     <- loadMaybeAbs args cfg "outfile"
  shared      <- loadMaybe    args cfg "shared" >>= mapM absPathOrUrl
  termcolumns <- loadMaybeInt args cfg "termcolumns"
  let interactive = isNothing script || (isPresent args $ longOption "interactive")
      shellaccess = isPresent args $ longOption "shellaccess"
      progressbar = isPresent args $ longOption "progressbar"
      showhidden  = isPresent args $ longOption "showhidden"
  let res = Config { .. }
  -- debug' $ show res
  setLogLevel LevelDebug
  mapM_ setDebugSourceRegex debugregex
  return res

getDoc :: String -> IO (Maybe String)
getDoc name = do
  path <- getDataFileName $ "docs" </> name <.> "txt"
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else readFile path >>= return . Just

getUsage :: IO Docopt
getUsage = getDoc "usage" >>= parseUsageOrExit . fromJust

{- These are done the simple, repetitive way for now to avoid lenses.  That
 - might change in the future though, because turns out getters and setters are
 - horrible!
 -
 - Note that shellacces is purposely not available to change here.
 -}

-- TODO move to Pretty?
showConfig :: Config -> String
showConfig cfg = init $ unlines $ map (showConfigField cfg . fst) configFields

showConfigField :: Config -> String -> String
showConfigField cfg fieldName = case lookup fieldName configFields of
  Nothing -> "no such config setting: " ++ fieldName
  Just (shower, _) -> fieldName ++ " = " ++ shower cfg

setConfigField :: Config -> String -> String -> Either String (IO Config)
setConfigField cfg key val = case lookup key configFields of
  Nothing -> Left $ "no such config setting: " ++ key
  Just (_, setter) -> case setter cfg val of
      Left err -> Left err
      Right io -> Right (io >>= \cfg' -> do
        putStrLn $ showConfigField cfg' key
        return cfg')

-- | Takes the current config and a new value. Does some IO, then returns the new Config or an error.
type ConfigSetter = Config -> String -> Either String (IO Config)

-- | Association list of field name -> (getter, setter)
configFields :: [(String, (Config -> String, ConfigSetter))]
configFields =
  [ ("tmpdir"     , (show . tmpdir     , mkSet parseString      (\c  p ->       absolutize  p  >>=  \a -> return $ c {tmpdir  =  a})))
  , ("workdir"    , (show . workdir    , mkSet parseString      (\c  p ->       absolutize  p  >>=  \a -> return $ c {workdir =  a})))
  , ("logfile"    , (show . logfile    , mkSet parseString      (\c  p ->       absolutize  p  >>=  \a -> updateLog a >>= \a' -> return (c {logfile =  a'}))))
  , ("script"     , (show . script     , mkSet parseMaybeString (\c mp -> (mapM absolutize mp) >>= \ma -> return $ c {script  = ma})))
  , ("wrapper"    , (show . wrapper    , mkSet parseMaybeString (\c mp -> (mapM absolutize mp) >>= \ma -> return $ c {wrapper  = ma})))
  , ("report"     , (show . report     , mkSet parseMaybeString (\c mp -> (mapM absolutize mp) >>= \ma -> return $ c {report  = ma})))
  , ("outfile"    , (show . outfile    , mkSet parseMaybeString (\c mp -> (mapM absolutize mp) >>= \ma -> return $ c {outfile = ma})))
  , ("shared"     , (show . shared     , mkSet parseMaybeString (\c ms -> (mapM absPathOrUrl ms) >>= \ma -> return $ c {shared  = ma})))
  , ("termcolumns", (show . termcolumns, mkSet parseMaybeInt (\c mi -> return $ c {termcolumns = mi})))
  , ("debugregex" , (show . debugregex , mkSet parseMaybeString (\c mr -> updateDebug mr >> return (c {debugregex = mr}))))
  , ("showhidden" , (show . showhidden , mkSet parseBool (\c b -> return (c {showhidden = b}))))
  , ("shellaccess" , (show . shellaccess , mkSet parseBool (\c _ -> putStrLn securityMessage >> return c)))
  , ("progressbar" , (show . progressbar , mkSet parseBool (\c b -> return (c {progressbar = b}))))
  ]

securityMessage :: String
securityMessage = "For security reasons, you can't change this from inside the REPL."

absPathOrUrl :: String -> IO String
absPathOrUrl p = if isURL p then return p else absolutize p

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

{-|
This will throw "resource busy (file is locked)" if you re-open a previous log file,
so we try that and add a numeric suffix if needed to ensure we can keep logging.
Returns the first path that worked.
-}
updateLog :: FilePath -> IO FilePath
updateLog path = retryIncSuffix path 1 $ \p -> do
  debug' $ "changing logfile to '" ++ p ++ "'"
  setLogFile p
  debug' $ "changed logfile to '" ++ p ++ "'"
  return p

mkSet :: (String -> Either String a) -- ^ parser function
      -> (Config -> a -> IO Config)  -- ^ setter function
      -> Config                      -- ^ current config
      -> String                      -- ^ new field value
      -> Either String (IO Config)
mkSet parser setter cfg value =
  case parser value of
    Left  err -> Left err
    Right val -> Right (setter cfg val)

-- from https://stackoverflow.com/a/3743602
-- TODO error if quotes are mismatched? like "this' or 'this"
sq :: String -> String
sq s@[_]                     = s
sq ('"':s)  | last s == '"'  = init s
            | otherwise      = s
sq ('\'':s) | last s == '\'' = init s
            | otherwise      = s
sq s                         = s

parseString :: String -> Either String FilePath
parseString input = case maybeRead ("\"" ++ sq input ++ "\"") of
                    Nothing -> Left $ "invalid: '" ++ input ++ "'"
                    Just "" -> Left $ "invalid: \"\""
                    Just p  -> Right p

-- note that if you write a quoted empty string it gets parsed as Nothing here,
-- but the actual empty string (no path given) will be interpreted as reading
-- the config value rather than changing it
parseMaybeString :: String -> Either String (Maybe String)
parseMaybeString input
  | map toLower input `elem` ["\"\"", "''", "nothing"] = Right Nothing
  | otherwise = fmap Just $ parseString input

parseInt :: String -> Either String Int
parseInt input = case maybeRead input of
                   Nothing -> Left $ "invalid: '" ++ input ++ "'"
                   Just n  -> Right n

parseMaybeInt :: String -> Either String (Maybe Int)
parseMaybeInt input
  | map toLower input `elem` ["\"\"", "''", "nothing"] = Right Nothing
  | otherwise = fmap Just $ parseInt input

parseBool :: String -> Either String Bool
parseBool []     = Left $ "invalid: ''"
parseBool "''"   = Right False
parseBool "\"\"" = Right False
parseBool input = let noquotes' = sq input
                      capitalized = toUpper (head noquotes'):map toLower (tail noquotes')
                  in case maybeRead capitalized of
                       Nothing -> Left $ "invalid: '" ++ input ++ "'"
                       Just b  -> Right b
