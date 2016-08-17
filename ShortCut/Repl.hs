{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Based on:
-- http://dev.stephendiehl.com/hask/ (the Haskeline section)
-- https://github.com/goldfirere/glambda

-- TODO prompt to remove any bindings dependent on one the user is changing
--      hey! just store a list of vars referenced as you go too. much easier!
--      will still have to do that recursively.. don't try until after lab meeting

module ShortCut.Repl where

import ShortCut.Types
import ShortCut.Interpret
import Control.Monad.Except           (throwError, MonadError, ExceptT
                                      ,runExceptT)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.Identity         (mzero)
import Control.Monad.RWS.Lazy         (RWST, runRWS, runRWST, get, put, ask)
import Control.Monad.Reader           (MonadReader)
import Control.Monad.State            (MonadState)
import Control.Monad.Trans            (MonadTrans, lift)
import Control.Monad.Trans.Maybe      (MaybeT(..), runMaybeT)
import Control.Monad.Writer           (MonadWriter)
import Data.Char                      (isSpace)
import Data.List                      (dropWhileEnd, isPrefixOf)
import Data.List.Utils                (delFromAL)
import Data.Maybe                     (fromJust)
import Prelude                 hiding (print)
import System.Command                 (runCommand, waitForProcess)
import System.Console.Haskeline       (InputT, runInputT, defaultSettings
                                      ,getInputLine)
import Text.PrettyPrint.HughesPJClass (prettyShow)

----------------
-- Repl monad --
----------------

{- Normally I put all the types in Types.hs, but this seems so repl-specific
 - that I moved it here for modularity.
 -}

newtype ReplM a = ReplM
  { unReplM :: MaybeT (CutT (InputT IO)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader CutConfig
    , MonadWriter CutLog
    , MonadState  CutState
    , MonadError  CutError
    )

runReplM :: ReplM a -> CutConfig -> CutState
         -> IO (Either CutError (Maybe a), CutState, CutLog)
runReplM r c s = runInputT defaultSettings $ runCutT (runMaybeT $ unReplM r) c s

prompt :: String -> ReplM (Maybe String)
prompt = ReplM . lift . lift . getInputLine

print :: String -> ReplM ()
print str = liftIO $ putStr str

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
    line -> do
      scr <- get
      if isAssignment line
        then do
          case iAssign scr line of
            Left  e -> print $ show e
            Right a -> putAssign a
        else do
          -- TODO how to handle if the var isn't in the script??
          -- TODO hook the logs + configs together?
          -- TODO only evaluate up to the point where the expression they want?
          case iExpr scr line of
            Left  err -> throwError err
            Right expr -> do
              let res  = VarName "result"
                  scr' = delFromAL scr res ++ [(res,expr)]
              liftIO $ eval $ cScript res scr'
  loop

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
  ec <- liftIO $ iFile path 
  case ec of
    Left e  -> print $ show e
    Right c -> put c

-- TODO this needs to read a second arg for the var to be main?
--      or just tell people to define main themselves?
-- TODO replace showHack with something nicer
cmdSave :: String -> ReplM ()
cmdSave path = get >>= \s -> liftIO $ writeFile path $ showHack s
  where
    showHack = unlines . map show

cmdDrop :: String -> ReplM ()
cmdDrop [] = put []
cmdDrop var = do
  s <- get
  case lookup (VarName var) s of
    Nothing -> print $ "VarName '" ++ var ++ "' not found"
    Just _  -> get >>= \s -> put $ delFromAL s (VarName var)

cmdType :: String -> ReplM ()
cmdType s = do
  script <- get
  print $ case iExpr script s of
    Right expr -> prettyShow $ getExt expr
    Left  err  -> show err

cmdShow :: String -> ReplM ()
cmdShow [] = get >>= liftIO . mapM_ (putStrLn . prettyShow)
cmdShow var = do
  s <- get
  print $ case lookup (VarName var) s of
    Nothing -> "VarName '" ++ var ++ "' not found"
    Just e  -> prettyShow e

cmdQuit :: String -> ReplM ()
cmdQuit _ = ReplM mzero

cmdBang :: String -> ReplM ()
cmdBang cmd = liftIO (runCommand cmd >>= waitForProcess) >> return ()
