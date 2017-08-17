module ShortCut.Core.Config where

import qualified Data.Configurator as C

import Data.Configurator.Types    (Config, Worth(..))
import Data.Maybe                 (fromJust)
import Data.Text                  (pack)
import Development.Shake          (newResourceIO, command_, withResource, Action, CmdOption(..))
import Paths_ShortCut             (getDataFileName)
import ShortCut.Core.Types        (CutConfig(..), CutModule(..), WrapperConfig(..))
import ShortCut.Core.Util         (expandTildes)
import System.Console.Docopt      (Docopt, Arguments, getArg, isPresent, longOption)
import System.Console.Docopt.NoTH (parseUsageOrExit)
import System.Directory           (makeAbsolute)
import Development.Shake          (Resource)
import Text.Read.HT               (maybeRead)

loadField :: Arguments -> Config -> String -> IO (Maybe String)
loadField args cfg key
  | isPresent args (longOption key) = return $ getArg args $ longOption key
  | otherwise = C.lookup cfg $ pack key

-- TODO how to enforce via docopt that if limit is given, script is too?
-- Note that cfg here is an intermediate docopt thing, not the same cfg that
-- gets passed around everywhere else in ShortCut.
loadWrapperConfig :: Arguments -> Config -> IO (Maybe WrapperConfig)
loadWrapperConfig args cfg = do
  wscr <- loadField args cfg "wrapper"
  case wscr of
    Nothing -> return Nothing
    Just wscr' -> do
      wscr'' <- makeAbsolute wscr'
      wlim   <- loadField args cfg "limit"
      wlim'  <- case wlim of
        Nothing -> return Nothing
        Just n  -> case read n of
          0  -> return Nothing
          n' -> do
            r <- newResourceIO "limit" n'
            return $ Just r
      return $ Just WrapperConfig
        { wrapperScript = wscr''
        , wrapperLimit  = wlim'
        }

loadConfig :: [CutModule] -> Arguments -> IO CutConfig
loadConfig mods args = do
  let path = fromJust $ getArg args $ longOption "config"
  cfg <- C.load [Optional path]
  csc <- loadField args cfg "script"
  csc' <- case csc of
            Nothing -> return Nothing
            Just s  -> makeAbsolute s >>= return . Just
  ctd <- loadField args cfg "tmpdir"
  cwd <- loadField args cfg "workdir"
  rep <- loadField args cfg "report"
  -- TODO if cluster-scripts given, activate wrappedCmd calls
  -- TODO if cluster-limit given, define wrappedCmd to use a resource
  cls <- loadWrapperConfig args cfg
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
  txt  <- expandTildes =<< readFile path
  parseUsageOrExit txt

hasArg :: Arguments -> String -> Bool
hasArg as a = isPresent as $ longOption a

-- Shake's command_ adapted to work with wrapperScript and wrapperLimit if used
-- TODO gather shake stuff into a Shake.hs module?
--      could have config, debug, wrappedCmd, eval...
wrappedCmd :: CutConfig -> [CmdOption] -> FilePath -> [String] -> Action ()
wrappedCmd cfg opts bin args = case cfgWrapper cfg of
  Nothing -> command_ opts bin args
  Just wc -> case wrapperLimit wc of
    Nothing -> wCmd
    Just l  -> withResource l 1 wCmd
    where
      wCmd = command_ opts (wrapperScript wc) (bin:args)

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

fields :: [(String, (CutConfig -> String,
                     CutConfig -> String -> Either String CutConfig))]
fields =
  [ ("script" , (show . cfgScript , setScript ))
  , ("tmpdir" , (show . cfgTmpDir , setTmpdir ))
  , ("workdir", (show . cfgWorkDir, setWorkdir))
  , ("debug"  , (show . cfgDebug  , setDebug  ))
  , ("wrapper", (show . getWrapper, setWrapper))
  -- , ("limit"  , (show . getLimit  , setLimit  ))
  , ("report" , (show . cfgReport , setReport ))
  ]

getWrapper :: CutConfig -> Maybe FilePath
getWrapper = fmap wrapperScript . cfgWrapper

getLimit :: CutConfig -> Maybe Resource
getLimit cfg = cfgWrapper cfg >>= wrapperLimit

setDebug :: CutConfig -> String -> Either String CutConfig
setDebug cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgDebug = v }

setScript :: CutConfig -> String -> Either String CutConfig
setScript cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgScript = Just v }

setTmpdir :: CutConfig -> String -> Either String CutConfig
setTmpdir cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgTmpDir = v }

setWorkdir :: CutConfig -> String -> Either String CutConfig
setWorkdir cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> Right $ cfg { cfgWorkDir = v }

setWrapper :: CutConfig -> String -> Either String CutConfig
setWrapper cfg val = case maybeRead val of
  Nothing -> Left  $ "invalid: " ++ val
  Just v  -> let wrp' = case cfgWrapper cfg of
                   Just w  -> w { wrapperScript = v }
                   Nothing -> WrapperConfig
                                { wrapperScript = v
                                , wrapperLimit = Nothing
                                }
             in Right $ cfg { cfgWrapper = Just wrp' }

setReport :: CutConfig -> String -> Either String CutConfig
setReport cfg val = case read val of
  Nothing -> Left  $ "invalid: " ++ val
  v       -> Right $ cfg { cfgReport = v }

-- TODO fix this one when there's time or a reason
-- setLimit cfg val = case maybeRead val of
--   Nothing -> Left $ "invalid: " ++ val
--   just v  -> case cfgWrapper cfg of
--                Nothing -> Left $ "can't set wrapper limit without a wrapper script"
--                Just w -> Right $ w { wrapperLimit = v }
