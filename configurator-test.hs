{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Configurator as C
import qualified Data.Text.Lazy    as T

import Text.Pretty.Simple (pShowNoColor)

data Config = Config
  { interactive    :: Bool
  , debugregex     :: Maybe String
  , testpattern    :: Maybe String
  , termwidth      :: Int
  , shellaccess    :: Bool
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
  cfg <- readConfigFile "configurator-test.cfg"
  putStrLn $ T.unpack $ pShowNoColor cfg
