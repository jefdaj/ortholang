-- Based on:
-- http://dev.stephendiehl.com/hask/ (the Haskeline section)
-- https://github.com/goldfirere/glambda

-- TODO prompt to remove any bindings dependent on one the user is changing
--      hey! just store a list of vars referenced as you go too. much easier!
--      will still have to do that recursively.. don't try until after lab meeting

module ShortCut.Repl where

import ShortCut.Types
import ShortCut.Interpret

import Control.Monad.IO.Class         (liftIO)
import Data.Char                      (isSpace)
import Data.List                      (dropWhileEnd)
import Data.List                      (isPrefixOf)
import Data.List.Utils                (delFromAL)
import Data.Maybe                     (fromJust)
import System.Command                 (runCommand, waitForProcess)
import Text.PrettyPrint.HughesPJClass (prettyShow)

---------------
-- utilities --
---------------

stripWhiteSpace :: String -> String
stripWhiteSpace = dropWhile isSpace . dropWhileEnd isSpace

--------------------
-- main interface --
--------------------

repl :: IO ()
repl = welcome >> runReplM loop [] [] >> goodbye

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
loop :: ReplM ()
loop = do
  mline <- prompt "shortcut >> "
  case stripWhiteSpace (fromJust mline) of -- can this ever be Nothing??
    "" -> return ()
    (':':cmd) -> runCmd cmd
    line -> if isAssignment line then do
              scr <- getScript
              case iAssign scr line of
                Left  e -> message $ show e
                Right a -> putAssign a
            else eExpr line
  loop

--------------------------
-- dispatch to commands --
--------------------------

runCmd :: String -> ReplM ()
runCmd line = case matches of
  [(_, fn)] -> fn $ stripWhiteSpace args
  []        -> message $ "unknown command: "   ++ cmd
  _         -> message $ "ambiguous command: " ++ cmd
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) cmds

cmds :: [(String, String -> ReplM ())]
cmds =
  [ ("help" , cmdHelp)
  , ("load" , cmdLoad)
  , ("write", cmdSave) -- TODO rename to write to avoid conflict with show
  , ("drop" , cmdDrop)
  , ("type" , cmdType)
  , ("show" , cmdShow)
  , ("quit" , cmdQuit)
  , ("!"    , cmdBang)
  ]

---------------------------
-- run specific commands --
---------------------------

cmdHelp :: String -> ReplM ()
cmdHelp _ = message
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
cmdLoad :: String -> ReplM ()
cmdLoad path = do
  ec <- liftIO $ iFile path 
  case ec of
    Left e  -> message $ show e
    Right c -> putScript c

-- TODO this needs to read a second arg for the var to be main?
--      or just tell people to define main themselves?
-- TODO replace showHack with something nicer
cmdSave :: String -> ReplM ()
cmdSave path = getScript >>= \s -> liftIO $ writeFile path $ showHack s
  where
    showHack = unlines . map show

cmdDrop :: String -> ReplM ()
cmdDrop [] = putScript []
cmdDrop var = do
  expr <- getExpr (TypedVar var)
  case expr of
    Nothing -> message $ "VarName '" ++ var ++ "' not found"
    Just _  -> getScript >>= \s -> putScript $ delFromAL s (TypedVar var)

cmdType :: String -> ReplM ()
cmdType s = do
  script <- getScript
  message $ case iExpr script s of
    Right expr -> prettyShow expr
    Left  err  -> show err

cmdShow :: String -> ReplM ()
cmdShow [] = getScript >>= liftIO . mapM_ (putStrLn . prettyShow)
cmdShow var = do
  expr <- getExpr (TypedVar var)
  message $ case expr of
    Nothing -> "VarName '" ++ var ++ "' not found"
    Just e  -> prettyShow e

cmdQuit :: String -> ReplM ()
cmdQuit _ = quit

cmdBang :: String -> ReplM ()
cmdBang cmd = liftIO (runCommand cmd >>= waitForProcess) >> return ()
