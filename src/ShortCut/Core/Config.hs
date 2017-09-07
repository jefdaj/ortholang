module ShortCut.Core.Config where

-- TODO absolutize in the setters too? or unify them with initial loaders?

import qualified Data.Configurator as C

import Data.Configurator.Types    (Config, Worth(..))
import Data.Maybe                 (fromJust)
import Data.Text                  (pack)
import Development.Shake          (command, Action, CmdOption(..), CmdResult, Exit(..),
                                   removeFiles, liftIO)
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Types        (CutConfig(..), CutModule(..))
import ShortCut.Core.Util         (absolutize)
import System.Console.Docopt      (Docopt, Arguments, getArg, isPresent,
                                   longOption)
import System.Console.Docopt.NoTH (parseUsageOrExit)
import Text.Read.HT               (maybeRead)
import System.Exit                (ExitCode(..))
import System.FilePath            ((</>), takeDirectory, takeFileName)

loadField :: Arguments -> Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = C.lookup cfg $ pack key

loadConfig :: [CutModule] -> Arguments -> IO CutConfig
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
  return CutConfig
    { cfgScript  = csc'
    , cfgTmpDir  = fromJust ctd
    , cfgWorkDir = fromJust cwd
    , cfgDebug   = isPresent args $ longOption "debug"
    , cfgModules = mods
    , cfgWrapper = cls
    , cfgReport  = rep
    }

getUsage :: IO Docopt
getUsage = do
  path <- getDataFileName "usage.txt"
  txt  <- absolutize =<< readFile path
  parseUsageOrExit txt

hasArg :: Arguments -> String -> Bool
hasArg as a = isPresent as $ longOption a

----------------------
-- wrapped commands --
----------------------

-- TODO separate module for this?

failGracefully :: String -> Int -> [String] -> Action ()
failGracefully bin n ptns = do
  -- toDel <- globs dir ptns -- TODO any better dir? absolute?
  -- liftIO $ removeFiles dir ptns
  liftIO $ mapM_ (\p -> removeFiles (takeDirectory p) [takeFileName p]) ptns
  error $ unlines $
    [ "Oh no! " ++ bin ++ " failed with error code " ++ show n ++ "."
    , "The files it was working on have been deleted:"
    ] ++ ptns

-- TODO call this when exiting nonzero and/or exception thrown
-- TODO take a list of globs and resolve them to files
-- TODO delete the files, telling shake if possible
-- TODO print a message for the user
-- TODO raise/re-raise an exception

-- Shake's command_ adapted to work with wrapperScript and wrapperLimit if used
-- TODO gather shake stuff into a Shake.hs module?
--      could have config, debug, wrappedCmd, eval...
-- ptns is a list of patterns for files to delete in case the cmd fails
wrappedCmd :: CutConfig -> [String]
           -> [CmdOption] -> FilePath -> [String] -> Action ()
wrappedCmd cfg ptns opts bin args = do
  let cmd = case cfgWrapper cfg of
              Nothing -> command opts bin args
              Just w  -> command opts w (bin:args)
  Exit code <- cmd
  case code of
    ExitFailure n -> failGracefully bin n ptns
    ExitSuccess   -> return ()

-------------------------
-- getters and setters --
-------------------------

{- These are done the simple, repetitive way for now to avoid lenses.  That
 - might change in the future though, because turns out getters and setters are
 - horrible!
 -}

-- This is mainly for use in the REPL so no need to return usable data
showConfigField :: CutConfig -> String -> String
showConfigField cfg key = case lookup key fields of
  Nothing -> "no such config setting: " ++ key
  Just (getter, _) -> getter cfg

setConfigField :: CutConfig -> String -> String -> Either String CutConfig
setConfigField cfg key val = case lookup key fields of
  Nothing -> Left $ "no such config setting: " ++ key
  Just (_, setter) -> setter cfg val

-- TODO add modules? maybe not much need
fields :: [(String, (CutConfig -> String,
                     CutConfig -> String -> Either String CutConfig))]
fields =
  [ ("script" , (show . cfgScript , setScript ))
  , ("tmpdir" , (show . cfgTmpDir , setTmpdir ))
  , ("workdir", (show . cfgWorkDir, setWorkdir))
  , ("debug"  , (show . cfgDebug  , setDebug  ))
  , ("wrapper", (show . cfgWrapper, setWrapper))
  , ("report" , (show . cfgReport , setReport ))
  ]

showConfig :: CutConfig -> String
showConfig cfg = unlines $ map showField fields
  where
    showField (name, (getter, _)) = name ++ " = " ++ getter cfg

setDebug :: CutConfig -> String -> Either String CutConfig
setDebug cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgDebug = v }

setScript :: CutConfig -> String -> Either String CutConfig
setScript cfg "Nothing" = Right $ cfg { cfgScript = Nothing }
setScript cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgScript = Just v }

setTmpdir :: CutConfig -> String -> Either String CutConfig
setTmpdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgTmpDir = v }

setWorkdir :: CutConfig -> String -> Either String CutConfig
setWorkdir cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgWorkDir = v }

setWrapper :: CutConfig -> String -> Either String CutConfig
setWrapper cfg "Nothing" = Right $ cfg { cfgWrapper = Nothing }
setWrapper cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgWrapper = Just v }

setReport :: CutConfig -> String -> Either String CutConfig
setReport cfg val = case maybeRead ("\"" ++ val ++ "\"") of
  Nothing -> Left  $ "invalid: " ++ val
  v       -> Right $ cfg { cfgReport = v }
