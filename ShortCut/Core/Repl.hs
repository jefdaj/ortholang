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

module ShortCut.Core.Repl where

import ShortCut.Core.Types
import ShortCut.Core.Interpret

import Control.Monad.IO.Class   (liftIO)
import Control.Monad.Identity   (mzero)
import Control.Monad.State.Lazy (get, put)
import Data.Char                (isSpace)
import Data.List                (isPrefixOf)
import Data.List.Utils          (delFromAL)
import Data.Maybe               (fromJust, fromMaybe)
import Prelude           hiding (print)
import ShortCut.Core.Util            (absolutize, stripWhiteSpace)
import System.Command           (runCommand, waitForProcess)

--------------------
-- main interface --
--------------------

repl :: CutConfig -> IO ()
repl cfg = welcome >> runRepl loop ([], cfg) >> goodbye

welcome :: IO ()
welcome = putStrLn
  "Welcome to the ShortCut interpreter!\n\
  \Type :help for a list of the available commands."

goodbye :: IO ()
goodbye = putStrLn "Bye for now!"

-- There are four types of input we might get, in the order checked for:
--   1. a blank line, in which case we just loop again
--   2. a REPL command, which starts with `:`
--   3. an assignment statement (even an invalid one)
--   4. a one-off expression to be evaluated
--      (this includes if it's the name of an existing var)
--
-- TODO if you type an existing variable name, should it evaluate the script
--      *only up to the point of that variable*? or will that not be needed
--      in practice once the kinks are worked out?
-- TODO improve error messages by only parsing up until the varname asked for!
-- TODO should the new statement go where the old one was, or at the end??
loop :: Repl ()
loop = do
  mline <- prompt "shortcut >> "
  case stripWhiteSpace (fromJust mline) of -- can this ever be Nothing??
    "" -> return ()
    (':':cmd) -> runCmd cmd
    line -> do
      (scr, cfg) <- get
      if isAssignment scr line
        then do
          case iAssign scr line of
            Left  e -> print $ show e
            Right a@(v, _) -> do
              let scr' = delFromAL scr v
              put (scr' ++ [a], cfg)
        else do
          -- TODO how to handle if the var isn't in the script??
          -- TODO hook the logs + configs together?
          -- TODO only evaluate up to the point where the expression they want?
          case iExpr scr line of
            Left  err  -> fail $ "oh no! " ++ show err
            Right expr -> do
              let res  = CutVar "result"
                  scr' = delFromAL scr res ++ [(res,expr)]
              liftIO $ eval $ cScript res scr'
  loop

--------------------------
-- dispatch to commands --
--------------------------

runCmd :: String -> Repl ()
runCmd line = case matches of
  [(_, fn)] -> fn $ stripWhiteSpace args
  []        -> print $ "unknown command: "   ++ cmd
  _         -> print $ "ambiguous command: " ++ cmd
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) cmds

cmds :: [(String, String -> Repl ())]
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

cmdHelp :: String -> Repl ()
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
cmdLoad :: String -> Repl ()
cmdLoad path = do
  new <- liftIO $ iFile path
  case new of
    Left  e -> print $ show e
    Right s -> get >>= \(_, c) -> put (s, c)

-- TODO this needs to read a second arg for the var to be main?
--      or just tell people to define main themselves?
-- TODO replace showHack with something nicer
cmdSave :: String -> Repl ()
cmdSave path = do
  path' <- liftIO $ absolutize path
  get >>= \s -> liftIO $ writeFile path' $ showHack $ fst s
  where
    showHack = unlines . map prettyShow

cmdDrop :: String -> Repl ()
cmdDrop [] = get >>= \s -> put ([], snd s)
cmdDrop var = do
  (scr, cfg) <- get
  let v = CutVar var
  case lookup v scr of
    Nothing -> print $ "Var '" ++ var ++ "' not found"
    Just _  -> put (delFromAL scr v, cfg)

-- TODO show the type description here too once that's ready
--      (add to the pretty instance?)
cmdType :: String -> Repl ()
cmdType s = do
  (scr, _) <- get
  print $ case iExpr scr s of
    Right expr -> show $ typeOf expr
    Left  err  -> show err

cmdShow :: String -> Repl ()
cmdShow [] = get >>= \(s, _) -> liftIO $ mapM_ (putStrLn . show) s
cmdShow var = do
  (scr, _) <- get
  print $ case lookup (CutVar var) scr of
    Nothing -> "Var '" ++ var ++ "' not found"
    Just e  -> show e

cmdQuit :: String -> Repl ()
cmdQuit _ = mzero

cmdBang :: String -> Repl ()
cmdBang cmd = liftIO (runCommand cmd >>= waitForProcess) >> return ()

-- TODO split string into first word and the rest
-- TODO case statement for first word: verbose, workdir, tmpdir, script?
-- TODO script sets the default for cmdSave?
-- TODO don't bother with script yet; start with the obvious ones
cmdSet :: String -> Repl ()
cmdSet = undefined

-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: String -> Repl ()
cmdConfig s = do
  (_, cfg) <- get
  let ws = words s
  if (length ws == 0)
    then (print (show cfg) >> return ()) -- TODO Pretty instance
    else if (length ws  > 2)
      then (print "too many variables" >> return ())
      -- TODO separate into get/set cases:
      else if (length ws == 1)
        then (cmdConfigShow (head ws))
        else (cmdConfigSet  (head ws) (last ws))

cmdConfigShow :: String -> Repl ()
cmdConfigShow key = get >>= \(_, cfg) -> print $ fn cfg
  where
    fn = case key of
          "script"  -> (\c -> fromMaybe "none" $ cfgScript c)
          "verbose" -> (\c -> show $ cfgVerbose c)
          "workdir" -> cfgWorkDir
          "tmpdir"  -> cfgTmpDir
          _ -> \_ -> "no such config entry"

cmdConfigSet :: String -> String -> Repl ()
cmdConfigSet key val = do
  (scr, cfg) <- get
  case key of
    "script"  -> put (scr, cfg { cfgScript  = Just val })
    "verbose" -> put (scr, cfg { cfgVerbose = read val })
    "workdir" -> put (scr, cfg { cfgWorkDir = val })
    "tmpdir"  -> put (scr, cfg { cfgTmpDir  = val })
    _ -> fail $ "no such variable '" ++ key ++ "'"
