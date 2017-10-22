module ShortCut.Core.Cmd
  ( wrappedCmd
  , wrappedCmdExit
  , wrappedCmdOut
  , wrappedCmdError -- for calling when a cmd is found to have failed
  )
  where

import Development.Shake
import ShortCut.Core.Types

import System.Exit     (ExitCode(..))
import System.FilePath (takeDirectory, takeFileName)

wrappedCmdError :: String -> Int -> [String] -> Action a
wrappedCmdError bin n ptns = do
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
-- TODO any way to propogate Shake cmd's cool stdout, stderr, exit feature?
wrappedCmd' :: CutConfig
            -> [CmdOption] -> FilePath -> [String]
            -> Action (String, ExitCode)
wrappedCmd' cfg opts bin args = do
  let fn = case cfgWrapper cfg of
             Nothing -> command opts bin args
             Just w  -> command opts w (bin:args)
  (Stdouterr out, Exit code) <- fn
  return (out, code)

wrappedCmd :: CutConfig -> [String]
           -> [CmdOption] -> FilePath -> [String]
           -> Action ()
wrappedCmd c ps os b as = do
  (_, code) <- wrappedCmd' c os b as
  case code of
    ExitFailure n -> wrappedCmdError b n ps
    ExitSuccess   -> return ()

wrappedCmdOut :: CutConfig -> [String]
              -> [CmdOption] -> FilePath -> [String]
              -> Action String
wrappedCmdOut c ps os b as = do
  (out, code) <- wrappedCmd' c os b as
  case code of
    ExitFailure n -> wrappedCmdError b n ps
    ExitSuccess   -> return out

-- Note that this one desn't have wrappedCmdError,
-- because it's used for when you expect a nonzero exit code.
-- TODO write some other error checking to go along with it!
wrappedCmdExit :: CutConfig
               -> [CmdOption] -> FilePath -> [String]
               -> Action ExitCode
wrappedCmdExit c os b as = wrappedCmd' c os b as >>= return . snd
