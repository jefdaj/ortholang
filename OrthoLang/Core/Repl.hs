-- TODO no welcome if going to load a file + clear the screen anyway
-- TODO could simplify to the same code everywhere except you pass the handle (file vs stdout)?

-- Based on:
-- http://dev.stephendiehl.com/hask/ (the Haskeline section)
-- https://github.com/goldfirere/glambda

-- TODO prompt to remove any bindings dependent on one the user is changing
-- TODO you should be able to write comments in the REPL

module OrthoLang.Core.Repl
  (

  -- * Used in main
    runRepl

  -- * Used in tests
  , mkRepl
  , ReplM
  , promptArrow

  -- * Implementation details
  , help
  , clear
  , cmdBang
  , cmdConfig
  , cmdDrop
  , cmdHelp
  , cmdLoad
  , cmdNeededBy
  , cmdNeeds
  , cmdQuit
  , cmdReload
  , cmdShow
  , cmdType
  , cmdWrite
  , cmds
  , depsOnly
  , dereference
  , loop
  , myComplete
  , nakedCompletions
  , prompt
  , quotedCompletions
  , removeSelfReferences
  , replSettings2
  , replaceVar
  , runCmd
  , runReplM
  , runStatement
  , saveScript
  , shortPrompt
  , showAssignType
  , showExprType
  , step
  , updateVars

  )
  where

import Prelude                  hiding (print)

import qualified Data.Map.Strict as M

import OrthoLang.Core.Types
import OrthoLang.Core.Config (showConfigField, setConfigField)
import OrthoLang.Core.Eval   (evalScript)
import OrthoLang.Core.Repl.Help   (help, renderTypeSig)
import OrthoLang.Core.Pretty (pPrintHdl)
import OrthoLang.Util        (stripWhiteSpace, headOrDie)

import OrthoLang.Core.Repl.Actions
import OrthoLang.Core.Repl.Messages

import Control.Exception.Safe     (Exception, Typeable, throw)
import Control.Monad.IO.Class     (liftIO)
import Data.Char                  (isSpace)
import Data.List                  (isPrefixOf, filter)
import System.FilePath.Posix      ((</>))
import System.IO                  (Handle, hPutStrLn, stdout)
import System.Process             (runCommand, waitForProcess)
import System.Console.Haskeline hiding (catch)
import Control.Monad.State.Strict (lift, get, put)

prompt :: String -> InputT ReplM (Maybe String)
prompt = getInputLine

-------------------
-- main interface --
--------------------

runRepl :: GlobalEnv -> IO ()
runRepl = mkRepl (repeat prompt) stdout

-- Like runRepl, but allows overriding the prompt function for golden testing.
-- Used by mockRepl in OrthoLang/Core/Repl/Tests.hs
-- TODO separate script from rest of GlobalEnv
mkRepl :: [String -> InputT ReplM (Maybe String)] -> Handle -> GlobalEnv -> IO ()
mkRepl promptFns hdl (_, cfg, ref, ids, dRef) = do
  let st = (emptyScript, cfg, ref, ids, dRef)
  st' <- case cfgScript cfg of
          Nothing -> welcome hdl >> return st
          Just path -> cmdLoad st hdl path >> cmdShow st hdl []
  runReplM (replSettings2 cfg) (loop promptFns hdl) st'

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
-- (No need to mock print because stdout can be captured directly)
--
-- TODO replace list of prompts with pipe-style read/write from here?
--      http://stackoverflow.com/a/14027387
loop :: [String -> InputT ReplM (Maybe String)] -> Handle -> InputT ReplM (Maybe String)
loop [] _ = return Nothing
loop (promptFn:promptFns) hdl = do
  st@(_, cfg, _, _, _)  <- lift $ get
  mLine <- promptFn $ shortPrompt cfg
  st' <- liftIO $ step st hdl mLine -- TODO what kind of lift is appropriate here?
  lift $ put st'
  loop promptFns hdl

-- TODO move to Types.hs
-- TODO use this pattern for other errors? or remove?

data QuitRepl = QuitRepl
  deriving Typeable

instance Exception QuitRepl

instance Show QuitRepl where
  show QuitRepl = "Bye for now!"

-- Attempts to process a line of input, but prints an error and falls back to
-- the current state if anything goes wrong. This should eventually be the only
-- place exceptions are caught.
step :: GlobalEnv -> Handle -> Maybe String -> IO GlobalEnv
step st hdl mLine = case mLine of
  Nothing -> return st
  Just line -> case stripWhiteSpace line of
    ""        -> return st
    ('#':_  ) -> return st
    (':':cmd) -> runCmd st hdl cmd
    statement -> runStatement st hdl statement

--------------------------
-- dispatch to commands --
--------------------------

-- TODO can this use tab completion?
runCmd :: GlobalEnv -> Handle -> String -> IO GlobalEnv
runCmd st@(_, cfg, _, _, _) hdl line = case matches of
  [(_, fn)] -> fn st hdl $ stripWhiteSpace args
  []        -> hPutStrLn hdl ("unknown command: "   ++ cmd) >> return st
  _         -> hPutStrLn hdl ("ambiguous command: " ++ cmd) >> return st
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) (cmds cfg)

cmds :: Config -> [(String, ReplCmd)]
cmds cfg =
  [ ("help"     , cmdHelp    )
  , ("load"     , cmdLoad    )
  , ("write"    , cmdWrite   ) -- TODO do more people expect 'save' or 'write'?
  , ("needs"    , cmdNeeds   )
  , ("neededfor", cmdNeededBy)
  , ("drop"     , cmdDrop    )
  , ("type"     , cmdType    )
  , ("show"     , cmdShow    )
  , ("reload"   , cmdReload  )
  , ("quit"     , cmdQuit    )
  , ("config"   , cmdConfig  )
  ]
  ++ if cfgSecure cfg then [] else [("!", cmdBang)]

-- TODO does this one need to be a special case now?
cmdQuit :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdQuit _ _ _ = throw QuitRepl
-- cmdQuit _ _ _ = ioError $ userError "Bye for now!"

cmdBang :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdBang st _ cmd = (runCommand cmd >>= waitForProcess) >> return st

-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdConfig st@(scr, cfg, ref, ids, dRef) hdl s = do
  let ws = words s
  if (length ws == 0)
    then pPrintHdl cfg hdl cfg >> return st -- TODO Pretty instance
    else if (length ws  > 2)
      then hPutStrLn hdl "too many variables" >> return st
      else if (length ws == 1)
        then hPutStrLn hdl (showConfigField cfg $ headOrDie "cmdConfig failed" ws) >> return st
        else case setConfigField cfg (headOrDie "cmdConfig failed" ws) (last ws) of
               Left err -> hPutStrLn hdl err >> return st
               Right iocfg' -> do
                 cfg' <- iocfg'
                 return (scr, cfg', ref, ids, dRef)

replSettings2 :: Config -> Settings ReplM
replSettings2 cfg = Settings
  { complete       = myComplete $ cmds cfg
  , historyFile    = Just $ cfgTmpDir cfg </> "history.txt"
  , autoAddHistory = True
  }
