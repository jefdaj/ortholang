{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Configurator as C
import qualified Data.Text.Lazy    as T

import Text.Pretty.Simple (pShowNoColor)

-- this will stay in Types.hs; the rest goes in Config.hs except anything reallly needed in Main
data Config = Config
  { interactive    :: Bool
  , debugregex     :: Maybe String -- TODO not needed during repl right?
  , testpattern    :: Maybe String
  , termwidth      :: Int  -- TODO does this ever need editing live? couldn't hurt
  , shellaccess    :: Bool -- TODO make this the only one you can't edit live?
  , progressbar    :: Bool
  , showhiddenfns  :: Bool
  , tmpdir         :: FilePath
  , workdir        :: FilePath
  , sharedir       :: Maybe FilePath
  , script         :: Maybe FilePath
  , wrapper        :: Maybe FilePath
  , output         :: Maybe FilePath
  , report         :: Maybe FilePath
  , logfile        :: FilePath
  }
  deriving (Eq, Show)

-- TODO try adding a docopt stage on top of this, since cli options should override the config file
-- TODO try adding a main and sub-configs
-- TODO use the current setup for live parsing. it's actually pretty close to ideal

readConfigFile :: FilePath -> IO Config
readConfigFile cfgFile = do
  cfg <- C.load [C.Required cfgFile]

  -- TODO true only if --script not passed
  interactive    <- C.lookupDefault False cfg "interactive"

  shellaccess    <- C.lookupDefault True  cfg "shellaccess"
  progressbar    <- C.lookupDefault True  cfg "progressbar"
  showhiddenfns  <- C.lookupDefault False cfg "showhiddenfns"

  -- required fields with default values
  tmpdir    <- C.lookupDefault "$(HOME)/.ortholang"         cfg "tmpdir"
  logfile   <- C.lookupDefault "$(HOME)/.ortholang/log.txt" cfg "logfile"
  workdir   <- C.lookupDefault "." cfg "workdir" -- TODO default to actual working dir
  termwidth <- C.lookupDefault 80 cfg "termwidth"

  -- optional fields
  debugregex     <- C.lookup cfg "debugregex"
  testpattern    <- C.lookup cfg "testpattern"
  sharedir       <- C.lookup cfg "sharedir"
  script         <- C.lookup cfg "script"
  wrapper        <- C.lookup cfg "wrapper"
  output         <- C.lookup cfg "output"
  report         <- C.lookup cfg "report"

  return $ Config
    { interactive
    , debugregex
    , testpattern
    , termwidth
    , shellaccess
    , progressbar
    , showhiddenfns
    , tmpdir
    , workdir
    , sharedir
    , script
    , wrapper
    , output
    , report
    , logfile
    }

main :: IO ()
main = do
  cfg <- readConfigFile "examples/ortholang.cfg"
  putStrLn $ T.unpack $ pShowNoColor cfg
