{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Based on:
-- http://dev.stephendiehl.com/hask/ (the Haskeline section)
-- https://github.com/goldfirere/glambda

-- TODO prompt to remove any bindings dependent on one the user is changing
--      hey! just store a list of vars referenced as you go too. much easier!
--      will still have to do that recursively.. don't try until after lab meeting

-- TODO you should be able to write comments in the REPL
-- TODO why doesn't prettyShow work anymore? what changed??
-- TODO should be able to :reload the current script, if any

module ShortCut.Core.Repl
  ( mkRepl
  , runRepl
  )
  where

import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Identity   (mzero)
import Control.Monad.State.Lazy (get, put)
import Data.Char                (isSpace)
import Data.List                (isPrefixOf)
import Data.List.Utils          (delFromAL)
import Data.Maybe               (fromJust, fromMaybe)
import Debug.Trace
import Prelude           hiding (print)
import ShortCut.Core.Compile    (compileScript)
import ShortCut.Core.Eval       (evalScript)
import ShortCut.Core.Parse      (isExpr, parseExpr, parseStatement, parseFile)
import ShortCut.Core.Types
import ShortCut.Core.Util       (absolutize, stripWhiteSpace)
import System.Command           (runCommand, waitForProcess)
import System.IO.Silently       (capture_)

--------------------
-- main interface --
--------------------

-- TODO load script from cfg if one was given on the command line
runRepl :: CutConfig -> IO ()
runRepl = mkRepl $ repeat prompt
-- repl = mockRepl ["1 + 1", ":q"]

-- Like runRepl, but allows overriding the prompt function for golden testing.
-- Used by mockRepl in ShortCut/Core/Repl/Tests.hs
-- TODO pass modules list on here
mkRepl :: [(String -> ReplM (Maybe String))] -> CutConfig -> IO ()
mkRepl promptFns cfg = welcome >> runReplM (loop promptFns) ([], cfg) >> goodbye

welcome :: IO ()
welcome = putStrLn
  "Welcome to the ShortCut interpreter!\n\
  \Type :help for a list of the available commands."

goodbye :: IO ()
goodbye = putStrLn "Bye for now!"

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
loop :: [(String -> ReplM (Maybe String))] -> ReplM ()
loop [] = runCmd "quit"
loop (promptFn:promptFns) = do
  mline <- promptFn "shortcut >> "
  case stripWhiteSpace (fromJust mline) of -- can this ever be Nothing??
    ""        -> return ()
    (':':cmd) -> runCmd cmd
    line      -> do
      st@(scr, cfg) <- get
      case parseStatement st line of
        Left e -> print $ show e
        Right r@(v, e) -> do
          let scr' = delFromAL scr v ++ [r]
          put (scr', cfg) -- v is always "result"
          -- even though result gets added to the script either way,
          -- still have to check whether to print it
          -- TODO should be able to factor this out and put in Eval.hs
          when (isExpr st line) (liftIO $ evalScript cfg scr')
  loop promptFns

--------------------------
-- dispatch to commands --
--------------------------

runCmd :: String -> ReplM ()
runCmd line = case matches of
  [(_, fn)] -> fn $ stripWhiteSpace args
  []        -> print $ "unknown command: "   ++ cmd
  _         -> print $ "ambiguous command: " ++ cmd
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) cmds

cmds :: [(String, String -> ReplM ())]
cmds =
  [ ("help" , cmdHelp)
  , ("load" , cmdLoad)
  , ("write", cmdSave)
  , ("drop" , cmdDrop)
  , ("type" , cmdType)
  , ("show" , cmdShow)
  , ("set"  , cmdSet)
  , ("quit" , cmdQuit)
  , ("!"    , cmdBang)
  , ("config", cmdConfig)
  ]

---------------------------
-- run specific commands --
---------------------------

cmdHelp :: String -> ReplM ()
cmdHelp _ = print
  "You can type or paste ShortCut code here to run it, same as in a script.\n\
  \There are also some extra commands:\n\n\
  \:help  to print this help text\n\
  \:load  to load a script (same as typing the file contents)\n\
  \:write to write the current script to a file\n\
  \:drop  to discard the current script and start fresh\n\
  \:quit  to discard the current script and exit the interpreter\n\
  \:type  to print the type of an expression\n\
  \:show  to print an expression along with its type\n\
  \:!     to run the rest of the line as a shell command"

-- TODO this is totally duplicating code from putAssign; factor out
-- TODO this shouldn't crash if a file referenced from the script doesn't exist!
cmdLoad :: String -> ReplM ()
cmdLoad path = do
  (_, cfg)  <- get
  new <- liftIO $ parseFile cfg path
  case new of
    Left  e -> print $ show e
    Right s -> put (s, cfg)

-- TODO this needs to read a second arg for the var to be main?
--      or just tell people to define main themselves?
-- TODO replace showHack with something nicer
cmdSave :: String -> ReplM ()
cmdSave path = do
  path' <- liftIO $ absolutize path
  get >>= \s -> liftIO $ writeFile path' $ showHack $ fst s
  where
    showHack = unlines . map prettyShow

cmdDrop :: String -> ReplM ()
cmdDrop [] = get >>= \(_, cfg) -> put ([], cfg)
cmdDrop var = do
  (scr, cfg) <- get
  let v = CutVar var
  case lookup v scr of
    Nothing -> print $ "Var '" ++ var ++ "' not found"
    Just _  -> put (delFromAL scr v, cfg)

-- TODO show the type description here too once that's ready
--      (add to the pretty instance?)
cmdType :: String -> ReplM ()
cmdType s = do
  state <- get
  print $ case parseExpr state s of
    Right expr -> show $ typeOf expr
    Left  err  -> show err

cmdShow :: String -> ReplM ()
cmdShow [] = get >>= \(s, _) -> liftIO $ mapM_ (putStrLn . prettyShow) s
cmdShow var = do
  (scr, _) <- get
  print $ case lookup (CutVar var) scr of
    Nothing -> "Var '" ++ var ++ "' not found"
    Just e  -> prettyShow e

cmdQuit :: String -> ReplM ()
cmdQuit _ = mzero

cmdBang :: String -> ReplM ()
cmdBang cmd = liftIO (runCommand cmd >>= waitForProcess) >> return ()

-- TODO split string into first word and the rest
-- TODO case statement for first word: verbose, workdir, tmpdir, script?
-- TODO script sets the default for cmdSave?
-- TODO don't bother with script yet; start with the obvious ones
cmdSet :: String -> ReplM ()
cmdSet = undefined

-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: String -> ReplM ()
cmdConfig s = do
  (_, cfg) <- get
  let ws = words s
  if (length ws == 0)
    then (print (prettyShow cfg) >> return ()) -- TODO Pretty instance
    else if (length ws  > 2)
      then (print "too many variables" >> return ())
      -- TODO separate into get/set cases:
      else if (length ws == 1)
        then (cmdConfigShow (head ws))
        else (cmdConfigSet  (head ws) (last ws))

cmdConfigShow :: String -> ReplM ()
cmdConfigShow key = get >>= \(_, cfg) -> print $ fn cfg
  where
    fn = case key of
          "script"  -> (\c -> fromMaybe "none" $ cfgScript c)
          "verbose" -> (\c -> show $ cfgVerbose c)
          "tmpdir"  -> cfgTmpDir
          _ -> \_ -> "no such config entry"

cmdConfigSet :: String -> String -> ReplM ()
cmdConfigSet key val = do
  (scr, cfg) <- get
  case key of
    "script"  -> put (scr, cfg { cfgScript  = Just val })
    "verbose" -> put (scr, cfg { cfgVerbose = read val })
    "tmpdir"  -> put (scr, cfg { cfgTmpDir  = val })
    _ -> fail $ "no such variable '" ++ key ++ "'"
