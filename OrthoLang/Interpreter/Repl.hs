{-# LANGUAGE ScopedTypeVariables #-}

-- TODO no welcome if going to load a file + clear the screen anyway
-- TODO could simplify to the same code everywhere except you pass the handle (file vs stdout)?

-- Based on:
-- http://dev.stephendiehl.com/hask/ (the Haskeline section)
-- https://github.com/goldfirere/glambda

-- TODO prompt to remove any bindings dependent on one the user is changing
-- TODO you should be able to write comments in the REPL

module OrthoLang.Interpreter.Repl
  (

  -- * Used in main
    runRepl

  -- * Used in tests
  , mkRepl
  , help
  , helpTopics
  , ReplM
  , promptArrow

  -- * Implementation details
  , cmds
  , cmdBang
  , cmdConfig
  , cmdQuit
  , runCmd
  , loop
  , replSettings
  , step

  )
  where

import Prelude hiding (print)

import OrthoLang.Types
import OrthoLang.Debug (debug)
import OrthoLang.Interpreter.Repl.Help
import OrthoLang.Interpreter.Repl.Edit
import OrthoLang.Interpreter.Repl.Info

import OrthoLang.Interpreter.Eval          (evalScript)
import OrthoLang.Interpreter.Parse         (isExpr, parseStatement)
import OrthoLang.Interpreter.Config    (showConfig, showConfigField, setConfigField)
import OrthoLang.Interpreter.Repl.Help (help)
import OrthoLang.Util           (stripWhiteSpace, headOrDie)

import Control.Exception.Safe     (Exception, throw)
import Control.Monad              (when)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.State.Strict (lift, get, put)
import Data.Char                  (isSpace)
import Data.List                  (isPrefixOf, filter)
import System.Console.Haskeline   (Settings(..), InputT, getInputLine)
import System.IO                  (Handle, hPutStrLn, stdout)
import System.Process             (runCommand, waitForProcess)
import Control.Exception.Safe     (catch)

-- | Main entry point for running the REPL
runRepl :: [Module] -> GlobalEnv -> IO ()
runRepl mods = mkRepl mods (repeat getInputLine) stdout

-- Like runRepl, but allows overriding the prompt function for golden testing.
-- Used by mockRepl in OrthoLang/Core/Repl/Tests.hs
-- TODO separate script from rest of GlobalEnv
mkRepl :: [Module] -> [String -> InputT ReplM (Maybe String)] -> Handle -> GlobalEnv -> IO ()
mkRepl mods promptFns hdl (_, cfg, ref, ids, dRef) = do
  clear
  let st = (emptyScript, cfg, ref, ids, dRef)
  st' <- case script cfg of
          Nothing -> welcome hdl >> return st
          Just path -> cmdLoad mods st hdl path -- >> cmdShow st hdl []
  catch
    (runReplM (replSettings mods cfg) (loop mods promptFns hdl) st')
    (\(_ :: QuitRepl) -> return ())

-- There are four types of input we might get, in the order checked for:
-- TODO update this to reflect 3/4 merged
--   1. a blank line, in which case we just loop again
--   2. a REPL command, which starts with `:`
--   3. an assignment statement (even an invalid one)
--   4. a one-off expression to be evaluated
--      (this includes if it's the name of an existing var)
--
-- TODO if you type an existing variable name, should it evaluate the script
--      *only up to the point of that variable*? or will that not be needed
--      in practice once the kinks are worked out?
--
-- TODO improve error messages by only parsing up until the varname asked for!
-- TODO should the new statement go where the old one was, or at the end??
--
-- The weird list of prompt functions allows mocking stdin for golded testing.
-- (No need to mock print because stdout can be captured directly) --
-- The `(Maybe String)` is for signaling the end of user input. `Nothing` comes
-- through if the user actually types :quit during their REPL session, or when
-- the last input promptFn from a Golden test has been used.
--
-- TODO replace list of prompts with pipe-style read/write from here?
--      http://stackoverflow.com/a/14027387
loop :: [Module] -> [String -> InputT ReplM (Maybe String)] -> Handle -> InputT ReplM (Maybe String)
loop _ [] _ = return Nothing
loop mods (promptFn:promptFns) hdl = do
  st@(_, cfg, _, _, _)  <- lift $ get
  mLine <- promptFn $ shortPrompt cfg
  st' <- liftIO $ step mods st hdl mLine
  lift $ put st'
  loop mods promptFns hdl

-- Attempts to process a line of input, but prints an error and falls back to
-- the current state if anything goes wrong. This should eventually be the only
-- place exceptions are caught.
step :: [Module] -> GlobalEnv -> Handle -> Maybe String -> IO GlobalEnv
step mods st hdl mLine = case mLine of
  Nothing -> return st
  Just line -> case stripWhiteSpace line of
    ""        -> return st
    ('#':_  ) -> return st
    (':':cmd) -> runCmd mods st hdl cmd
    statement -> withPostEditHook
                   runStatement mods st hdl statement

runStatement :: ReplEdit
runStatement mods st@(scr, cfg, ref, ids, dRef) hdl line = case parseStatement mods cfg scr line of
  Left  e -> hPutStrLn hdl (stripWhiteSpace e) >> return st
  Right r -> do
    let st' = (updateVars scr r, cfg, ref, ids, dRef)
    when (isExpr mods cfg scr line) (evalScript mods hdl st')
    return st'

-- TODO can this use tab completion?
runCmd :: [Module] -> GlobalEnv -> Handle -> String -> IO GlobalEnv
runCmd mods st@(_, cfg, _, _, _) hdl line = case matches of
  [(_, fn)] -> fn mods st hdl $ stripWhiteSpace args
  []        -> hPutStrLn hdl ("unknown command: "   ++ cmd) >> return st
  _         -> hPutStrLn hdl ("ambiguous command: " ++ cmd) >> return st
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) (cmds cfg)

cmds :: Config -> [(String, ReplEdit)]
cmds cfg = secureCmds ++ replCmds ++ editCmds ++ infoCmds
  where
    secureCmds = if shellaccess cfg then [] else [("!", cmdBang)] -- TODO :shell instead?
    replCmds =
      [ ("quit"  , cmdQuit  )
      , ("config", cmdConfig)
      ]
    editCmds =
      -- post-edit hook is skipped for writes to prevent double-writing
      -- TODO are there other hooks that do still need to be run?
      -- TODO do more people expect 'save' or 'write'?
      [ ("write", cmdWrite) ]
      ++
      map (\(n, c) -> (n, withPostEditHook c))
        [ ("load"  , cmdLoad  )
        , ("drop"  , cmdDrop  ) -- TODO autosave here?
        , ("reload", cmdReload)
        ]
    infoCmds :: [(String, ReplEdit)]
    infoCmds = map (\(n,c) -> (n, withSameState c))
      [ ("help"     , cmdHelp     )
      , ("type"     , cmdType     )
      , ("show"     , cmdShow     )
      , ("neededfor", cmdNeededFor)
      , ("neededby" , cmdNeededBy )
      ]

-- | Run a 'ReplEdit' command with post-edit hooks
withPostEditHook :: ReplEdit -> ReplEdit
withPostEditHook edit mods env@(scr, _, _, _, _) hdl s = do
  env'@(scr', cfg', _, _, _) <- edit mods env hdl s
  when (scr /= scr') $ autosaveScript cfg' scr'
  return env'

autosaveScript :: Config -> Script -> IO ()
autosaveScript cfg scr = case script cfg of
  Nothing   -> return ()
  Just path -> when (autosave cfg) $ do
    debug "interpreter.repl.autosaveScript" $ "autosaving '" ++ path ++ "'..."
    saveScript cfg scr path

-- | Run a 'ReplInfo' command and return the initial state
withSameState :: ReplInfo -> ReplEdit
withSameState info mods env hdl s = do
  info mods env hdl s
  hPutStrLn hdl "" -- extra newline before next prompt makes it look nicer
  return env

-- TODO does this one need to be a special case now?
cmdQuit :: ReplEdit
cmdQuit _ _ hdl _ = do
  hPutStrLn hdl "Bye for now!"
  throw QuitRepl

-- TODO move to Types.hs
-- TODO use this pattern for other errors? or remove?

data QuitRepl = QuitRepl
  deriving Show -- TODO was the Typeable instance important too?

instance Exception QuitRepl

-- instance Show QuitRepl where
  -- show QuitRepl = "Bye for now!" -- TODO get this to print

cmdBang :: ReplEdit
cmdBang _ st _ cmd = (runCommand cmd >>= waitForProcess) >> return st

-- TODO move most of this to Config?
-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: ReplEdit
cmdConfig _ st@(scr, cfg, ref, ids, dRef) hdl s = do
  let ws = words s -- TODO only split on the first + second space, passing the rest as one string (for patterns)
  if (length ws == 0)
    then hPutStrLn hdl (showConfig cfg ++ "\n") >> return st -- TODO Pretty instance?
    else if (length ws  > 2)
      then hPutStrLn hdl "too many variables\n" >> return st
      -- TODO any better way to handle this?
      -- TODO make repl tests for these
      else if (length ws == 1)
        -- TODO use pretty print or display here instead
        -- TODO remove modules, since they're never used and can't show/read
        then hPutStrLn hdl (showConfigField cfg $ head ws) >> return st -- TODO just the one field
        else case setConfigField cfg (headOrDie "cmdConfig failed" ws) (last ws) of
               Left err -> hPutStrLn hdl (err ++ "\n") >> return st
               Right iocfg' -> do
                 cfg' <- iocfg'
                 return (scr, cfg', ref, ids, dRef)

-- TODO move to Config? Types?
replSettings :: [Module] -> Config -> Settings ReplM
replSettings mods cfg =
  let cmdNames = map fst $ cmds cfg
  in Settings
       { complete       = myComplete mods cmdNames
       , historyFile    = history cfg
       , autoAddHistory = True
       }
