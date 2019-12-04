{-# LANGUAGE OverloadedStrings #-}

module ShortCut.Core.Config where

-- TODO absolutize in the setters too? or unify them with initial loaders?

import qualified Data.Configurator as C

import Data.Configurator.Types    (Config, Worth(..))
import Data.Maybe                 (isNothing)
import Data.Text                  (pack)
import Development.Shake           (newResourceIO)
-- import Development.Shake          (command, Action, CmdOption(..), Exit(..),
                                   -- removeFiles, liftIO)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Types        (CutConfig(..), CutModule(..))
import ShortCut.Core.Util         (absolutize, justOrDie, debug)
import System.Console.Docopt      (Docopt, Arguments, getArg, isPresent,
                                   longOption, getAllArgs)
import System.Console.Docopt.NoTH (parseUsageOrExit)
import Text.Read.HT               (maybeRead)
import System.FilePath            ((</>), (<.>))
import System.Info                (os)

import Control.Logging (LogLevel(..), setLogLevel, setDebugSourceRegex)
import Control.Monad         (when)

{- The logging module keeps its own state in an IORef, so no need to include
 - this in the main ShortCut config below.
 - TODO still put all the config stuff in CutConfig though, and make it changable in the repl!
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
-- debug :: CutConfig -> String -> a -> a
-- debug _ msg rtn = traceSL ... (pack msg) rtn

debug' :: String -> IO ()
debug' = debug "config"

loadField :: Arguments -> Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = C.lookup cfg $ pack key

loadConfig :: [CutModule] -> Arguments -> IO CutConfig
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
  shr <- mapM absolutize =<< loadField args cfg "sharedir"
  let ctp = getAllArgs args (longOption "test")
  par <- newResourceIO "parallel" 1 -- TODO set to number of nodes
  let int = isNothing csc' || (isPresent args $ longOption "interactive")
  os' <- getOS
  let res = CutConfig
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
              }
  debug' $ show res
  updateDebug dbg
  return res

getOS :: IO String
getOS = return $ if os == "darwin" then "mac" else os

-- TODO any way to recover if missing? probably not
-- TODO use a safe read function with locks here?
getDoc :: FilePath -> IO String
getDoc docPath = do
  path' <- getDataFileName $ "docs" </> docPath <.> "txt"
  -- putStrLn $ "path':" ++ path'
  -- this should only happen during development:
  -- written <- doesFileExist path'
  -- when (not written) $ writeFile path' $ "write " ++ docPath ++ " doc here"
  doc <- absolutize path' >>= readFile
  return doc

getUsage :: IO Docopt
getUsage = getDoc "usage" >>= parseUsageOrExit

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
showConfigField :: CutConfig -> String -> String
showConfigField cfg key = case lookup key fields of
  Nothing -> "no such config setting: " ++ key
  Just (getter, _) -> getter cfg

setConfigField :: CutConfig -> String -> String -> Either String (IO CutConfig)
setConfigField cfg key val = case lookup key fields of
  Nothing -> Left $ "no such config setting: " ++ key
  Just (_, setter) -> setter cfg val

-- TODO add modules? maybe not much need
-- TODO add interactive?
fields :: [(String, (CutConfig -> String,
                     CutConfig -> String -> Either String (IO CutConfig)))]
fields =
  [ ("script" , (show . cfgScript , setScript ))
  , ("tmpdir" , (show . cfgTmpDir , setTmpdir ))
  , ("workdir", (show . cfgWorkDir, setWorkdir))
  , ("debug"  , (show . cfgDebug  , setDebug  ))
  , ("wrapper", (show . cfgWrapper, setWrapper))
  , ("report" , (show . cfgReport , setReport ))
  , ("width"  , (show . cfgWidth  , setWidth  ))
  , ("output" , (show . cfgOutFile, setOutFile))
  -- TODO add share?
  ]

showConfig :: CutConfig -> String
showConfig cfg = unlines $ map showField fields
  where
    showField (name, (getter, _)) = name ++ " = " ++ getter cfg

setDebug :: CutConfig -> String -> Either String (IO CutConfig)
setDebug cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ do
    updateDebug $ Just v
    return $ cfg { cfgDebug = Just v }

-- this is run during setDebug, and once in loadConfig
updateDebug :: Maybe String -> IO ()
updateDebug regex = case regex of
  Nothing -> do
    debug' "turning of debugging"
    setLogLevel LevelWarn
  Just r -> do
    setLogLevel LevelDebug
    setDebugSourceRegex r
    debug' $ "set debug regex to " ++ show regex

setScript :: CutConfig -> String -> Either String (IO CutConfig)
setScript cfg "Nothing" = Right $ return $ cfg { cfgScript = Nothing }
setScript cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgScript = Just v }

setTmpdir :: CutConfig -> String -> Either String (IO CutConfig)
setTmpdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgTmpDir = v }

setWorkdir :: CutConfig -> String -> Either String (IO CutConfig)
setWorkdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgWorkDir = v }

setWrapper :: CutConfig -> String -> Either String (IO CutConfig)
setWrapper cfg "Nothing" = Right $ return $ cfg { cfgWrapper = Nothing }
setWrapper cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgWrapper = Just v }

setReport :: CutConfig -> String -> Either String (IO CutConfig)
setReport cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  v       -> Right $ return $ cfg { cfgReport = v }

setWidth :: CutConfig -> String -> Either String (IO CutConfig)
setWidth cfg "Nothing" = Right $ return $ cfg { cfgWidth = Nothing }
setWidth cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just n  -> Right $ return $ cfg { cfgWidth = Just n }

setOutFile :: CutConfig -> String -> Either String (IO CutConfig)
setOutFile cfg "Nothing" = Right $ return $ cfg { cfgOutFile = Nothing }
setOutFile cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ return $ cfg { cfgOutFile = Just v }
