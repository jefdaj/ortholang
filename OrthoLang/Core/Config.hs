{-# LANGUAGE OverloadedStrings #-}

-- TODO show cfgShare

module OrthoLang.Core.Config where

-- TODO absolutize in the setters too? or unify them with initial loaders?

import qualified Data.Configurator as C

import Data.Configurator.Types    (Config, Worth(..))
import Data.Maybe                 (isNothing)
import Data.Text                  (pack)
import Development.Shake           (newResourceIO)
-- import Development.Shake          (command, Action, CmdOption(..), Exit(..),
                                   -- removeFiles, liftIO)
import Paths_OrthoLang             (getDataFileName)
import OrthoLang.Core.Types        (OrthoLangConfig(..), OrthoLangModule(..))
import OrthoLang.Core.Util         (absolutize, justOrDie, debug)
import System.Console.Docopt      (Docopt, Arguments, getArg, isPresent,
                                   longOption, getAllArgs)
import System.Console.Docopt.NoTH (parseUsageOrExit)
import Text.Read.HT               (maybeRead)
import System.FilePath            ((</>), (<.>))
import System.Directory           (doesFileExist)
import System.Info                (os)

import Control.Logging (LogLevel(..), setLogLevel, setDebugSourceRegex)
import Control.Monad         (when)
import Data.List (isPrefixOf)
import GHC.Conc (getNumProcessors)

{- The logging module keeps its own state in an IORef, so no need to include
 - this in the main OrthoLang config below.
 - TODO still put all the config stuff in OrthoLangConfig though, and make it changable in the repl!
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
-- debug cfg msg rtn = if cfgDebug cfg then trace msg rtn else rtn
-- debug :: OrthoLangConfig -> String -> a -> a
-- debug _ msg rtn = traceSL ... (pack msg) rtn

debug' :: String -> IO ()
debug' = debug "config"

loadField :: Arguments -> Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = C.lookup cfg $ pack key

loadConfig :: [OrthoLangModule] -> Arguments -> IO OrthoLangConfig
loadConfig mods args = do
  debug' $ "docopt arguments: " ++ show args
  let path = justOrDie "parse --config arg failed!" $ getArg args $ longOption "config"
  cfg <- C.load [Optional path]
  csc <- loadField args cfg "script"
  csc' <- case csc of
            Nothing -> return Nothing
            Just s  -> absolutize s >>= return . Just
  dbg <- loadField args cfg "debug"
  ctd <- mapM absolutize =<< loadField args cfg "tmpdir"
  cwd <- mapM absolutize =<< loadField args cfg "workdir"
  rep <- mapM absolutize =<< loadField args cfg "report"
  cls <- mapM absolutize =<< loadField args cfg "wrapper"
  out <- mapM absolutize =<< loadField args cfg "output"
  shr <- mapM (\p -> if "http" `isPrefixOf` p then return p else absolutize p) =<< loadField args cfg "shared"
  let ctp = getAllArgs args (longOption "test")
  par <- newResourceIO "parallel" 1 -- TODO set to number of nodes
  let int = isNothing csc' || (isPresent args $ longOption "interactive")
  os' <- getOS
  cp <- getNumProcessors
  let res = OrthoLangConfig
              { cfgScript  = csc'
              , cfgInteractive = int
              , cfgTmpDir  = justOrDie "parse --tmpdir arg failed!" ctd
              , cfgWorkDir = justOrDie "parse --workdir arg failed!" cwd
              , cfgDebug   = dbg
              , cfgModules = mods
              , cfgWrapper = cls
              , cfgReport  = rep
              , cfgTestPtn = ctp
              , cfgWidth   = Nothing -- not used except in testing
              , cfgSecure  = isPresent args $ longOption "secure"
              , cfgNoProg  = isPresent args $ longOption "noprogress"
              , cfgParLock = par
              , cfgOutFile = out
              , cfgShare   = shr
              , cfgOS      = os'
              , cfgThreads = cp
              }
  debug' $ show res
  updateDebug dbg
  return res

getOS :: IO String
getOS = return $ if os == "darwin" then "mac" else os

-- TODO any way to recover if missing? probably not
-- TODO use a safe read function with locks here?
getDoc :: [FilePath] -> IO String
getDoc docPaths = do
  paths' <- mapM (\p -> getDataFileName ("docs" </> p <.> "txt") >>= absolutize) docPaths
  tests <- mapM doesFileExist paths'
  let path' = head [p | (p, t) <- zip paths' tests, t]
  -- putStrLn $ "path':" ++ path'
  -- this should only happen during development:
  -- written <- doesFileExist path'
  -- when (not written) $ writeFile path' $ "write " ++ docPath ++ " doc here"
  doc <- readFile path'
  return doc

getUsage :: IO Docopt
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
 - Note that cfgSecure is purposely not avialable here.
 -}

-- This is mainly for use in the REPL so no need to return usable data
showConfigField :: OrthoLangConfig -> String -> String
showConfigField cfg key = case lookup key fields of
  Nothing -> "no such config setting: " ++ key
  Just (getter, _) -> getter cfg

setConfigField :: OrthoLangConfig -> String -> String -> Either String (IO OrthoLangConfig)
setConfigField cfg key val = case lookup key fields of
  Nothing -> Left $ "no such config setting: " ++ key
  Just (_, setter) -> setter cfg val

-- TODO add modules? maybe not much need
-- TODO add interactive?
fields :: [(String, (OrthoLangConfig -> String,
                     OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)))]
fields =
  [ ("script" , (show . cfgScript , setScript ))
  , ("tmpdir" , (show . cfgTmpDir , setTmpdir ))
  , ("workdir", (show . cfgWorkDir, setWorkdir))
  , ("debug"  , (show . cfgDebug  , setDebug  ))
  , ("wrapper", (show . cfgWrapper, setWrapper))
  , ("report" , (show . cfgReport , setReport ))
  , ("width"  , (show . cfgWidth  , setWidth  ))
  , ("output" , (show . cfgOutFile, setOutFile))
  , ("threads", (show . cfgThreads, setThreads))
  -- TODO add share?
  ]

showConfig :: OrthoLangConfig -> String
showConfig cfg = unlines $ map showField fields
  where
    showField (name, (getter, _)) = name ++ " = " ++ getter cfg

setDebug :: OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)
setDebug cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ do
    updateDebug $ Just v
    return $ cfg { cfgDebug = Just v }

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

setScript :: OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)
setScript cfg "Nothing" = Right $ return $ cfg { cfgScript = Nothing }
setScript cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgScript = Just v }

setTmpdir :: OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)
setTmpdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgTmpDir = v }

setWorkdir :: OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)
setWorkdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgWorkDir = v }

setWrapper :: OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)
setWrapper cfg "Nothing" = Right $ return $ cfg { cfgWrapper = Nothing }
setWrapper cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgWrapper = Just v }

setReport :: OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)
setReport cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  v       -> Right $ return $ cfg { cfgReport = v }

setThreads :: OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)
setThreads cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgThreads = v }

setWidth :: OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)
setWidth cfg "Nothing" = Right $ return $ cfg { cfgWidth = Nothing }
setWidth cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just n  -> Right $ return $ cfg { cfgWidth = Just n }

setOutFile :: OrthoLangConfig -> String -> Either String (IO OrthoLangConfig)
setOutFile cfg "Nothing" = Right $ return $ cfg { cfgOutFile = Nothing }
setOutFile cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgOutFile = Just v }
