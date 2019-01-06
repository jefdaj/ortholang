module Detourrr.Core.Config where

-- TODO absolutize in the setters too? or unify them with initial loaders?

import qualified Data.Configurator as C

import Data.Configurator.Types    (Config, Worth(..))
import Data.Maybe                 (fromJust)
import Data.Text                  (pack)
import Development.Shake           (newResourceIO)
-- import Development.Shake          (command, Action, CmdOption(..), Exit(..),
                                   -- removeFiles, liftIO)
import Paths_Detourrr             (getDataFileName)
import Detourrr.Core.Types        (RrrConfig(..), RrrModule(..))
import Detourrr.Core.Util         (absolutize)
import System.Console.Docopt      (Docopt, Arguments, getArg, isPresent,
                                   longOption)
import System.Console.Docopt.NoTH (parseUsageOrExit)
import Text.Read.HT               (maybeRead)
import Debug.Trace       (trace)

{- The base debugging function used in other modules too. This is admittedly a
 - weird place to put it, but makes everything much easier as far as avoiding
 - import cycles.
 -}
debug :: RrrConfig -> String -> a -> a
debug cfg msg rtn = if cfgDebug cfg then trace msg rtn else rtn

loadField :: Arguments -> Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = C.lookup cfg $ pack key

loadConfig :: [RrrModule] -> Arguments -> IO RrrConfig
loadConfig mods args = do
  let path = fromJust $ getArg args $ longOption "config"
  cfg <- C.load [Optional path]
  csc <- loadField args cfg "script"
  csc' <- case csc of
            Nothing -> return Nothing
            Just s  -> absolutize s >>= return . Just
  ctd <- mapM absolutize =<< loadField args cfg "tmpdir"
  cwd <- mapM absolutize =<< loadField args cfg "workdir"
  rep <- mapM absolutize =<< loadField args cfg "report"
  cls <- mapM absolutize =<< loadField args cfg "wrapper"
  ctp <- loadField args cfg "pattern"
  par <- newResourceIO "parallel" 1
  return RrrConfig
    { cfgScript  = csc'
    , cfgTmpDir  = fromJust ctd
    , cfgWorkDir = fromJust cwd
    , cfgDebug   = isPresent args $ longOption "debug"
    , cfgModules = mods
    , cfgWrapper = cls
    , cfgReport  = rep
    , cfgTestPtn = ctp
    , cfgWidth   = Nothing -- not used except in testing
    , cfgSecure  = isPresent args $ longOption "secure"
    , cfgParLock = par
    }

getUsage :: IO Docopt
getUsage = do
  path <- getDataFileName "usage.txt"
  -- TODO use a safe read function with locks here?
  txt  <- absolutize =<< readFile path
  parseUsageOrExit txt

hasArg :: Arguments -> String -> Bool
hasArg as a = isPresent as $ longOption a

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
showConfigField :: RrrConfig -> String -> String
showConfigField cfg key = case lookup key fields of
  Nothing -> "no such config setting: " ++ key
  Just (getter, _) -> getter cfg

setConfigField :: RrrConfig -> String -> String -> Either String RrrConfig
setConfigField cfg key val = case lookup key fields of
  Nothing -> Left $ "no such config setting: " ++ key
  Just (_, setter) -> setter cfg val

-- TODO add modules? maybe not much need
fields :: [(String, (RrrConfig -> String,
                     RrrConfig -> String -> Either String RrrConfig))]
fields =
  [ ("script" , (show . cfgScript , setScript ))
  , ("tmpdir" , (show . cfgTmpDir , setTmpdir ))
  , ("workdir", (show . cfgWorkDir, setWorkdir))
  , ("debug"  , (show . cfgDebug  , setDebug  ))
  , ("wrapper", (show . cfgWrapper, setWrapper))
  , ("report" , (show . cfgReport , setReport ))
  , ("width"  , (show . cfgWidth  , setWidth  ))
  ]

showConfig :: RrrConfig -> String
showConfig cfg = unlines $ map showField fields
  where
    showField (name, (getter, _)) = name ++ " = " ++ getter cfg

setDebug :: RrrConfig -> String -> Either String RrrConfig
setDebug cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgDebug = v }

setScript :: RrrConfig -> String -> Either String RrrConfig
setScript cfg "Nothing" = Right $ cfg { cfgScript = Nothing }
setScript cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgScript = Just v }

setTmpdir :: RrrConfig -> String -> Either String RrrConfig
setTmpdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgTmpDir = v }

setWorkdir :: RrrConfig -> String -> Either String RrrConfig
setWorkdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgWorkDir = v }

setWrapper :: RrrConfig -> String -> Either String RrrConfig
setWrapper cfg "Nothing" = Right $ cfg { cfgWrapper = Nothing }
setWrapper cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgWrapper = Just v }

setReport :: RrrConfig -> String -> Either String RrrConfig
setReport cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  v       -> Right $ cfg { cfgReport = v }

setWidth :: RrrConfig -> String -> Either String RrrConfig
setWidth cfg "Nothing" = Right $ cfg { cfgWidth = Nothing }
setWidth cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just n  -> Right $ cfg { cfgWidth = Just n }
