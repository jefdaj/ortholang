{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Based on:
-- http://dev.stephendiehl.com/hask/ (the Haskeline section)
-- https://github.com/goldfirere/glambda

-- TODO prompt to remove any bindings dependent on one the user is changing
--      hey! just store a list of vars referenced as you go too. much easier!
--      will still have to do that recursively.. don't try until after lab meeting

-- TODO you should be able to write comments in the REPL

module ShortCut.Repl where

import ShortCut.Types
import ShortCut.Interpret
import ShortCut.Utils                 (absolutize)
import Control.Monad.Except           (throwError, MonadError)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.Identity         (mzero)
-- import Control.Monad.RWS.Lazy         (get, put, ask)
-- import Control.Monad.Reader           (MonadReader)
import Control.Monad.State            (MonadState, get, put)
import Control.Monad.Trans            (lift)
import Control.Monad.Trans.Maybe      (MaybeT(..), runMaybeT)
-- import Control.Monad.Writer           (MonadWriter)
import Data.Char                      (isSpace)
import Data.List                      (dropWhileEnd, isPrefixOf)
import Data.List.Utils                (delFromAL)
import Data.Maybe                     (fromJust)
import Prelude                 hiding (print)
import System.Command                 (runCommand, waitForProcess)
import System.Console.Haskeline       (InputT, runInputT, defaultSettings
                                      ,getInputLine)
import Text.PrettyPrint.HughesPJClass (prettyShow)
-- import Control.Monad (when)

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
    -- , MonadReader CutConfig
    -- , MonadWriter CutLog
    , MonadState  CutState
    , MonadError  CutError
    )

runReplM :: ReplM a -> CutState -> IO (Either CutError (Maybe a), CutState)
runReplM r s = runInputT defaultSettings $ runCutT (runMaybeT $ unReplM r) s

prompt :: String -> ReplM (Maybe String)
prompt = ReplM . lift . lift . getInputLine

print :: String -> ReplM ()
print str' = liftIO $ putStrLn str'

---------------
-- utilities --
---------------

stripWhiteSpace :: String -> String
stripWhiteSpace = dropWhile isSpace . dropWhileEnd isSpace

--------------------
-- main interface --
--------------------

repl :: CutConfig -> IO ()
repl cfg = welcome >> runReplM loop ([], cfg) >> goodbye

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
      (scr, cfg) <- get
      if isAssignment line
        then do
          case iAssign cfg scr line of
            Left  e -> print $ show e
            Right a -> putAssign a
        else do
          -- TODO how to handle if the var isn't in the script??
          -- TODO hook the logs + configs together?
          -- TODO only evaluate up to the point where the expression they want?
          case iExpr cfg scr line of
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
  (_, cfg) <- get
  new <- liftIO $ iFile cfg path 
  case new of
    Left  e -> print $ show e
    Right n -> put (n, cfg)

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
  (script, cfg) <- get
  case lookup (VarName var) script of
    Nothing -> print $ "VarName '" ++ var ++ "' not found"
    Just _  -> put (delFromAL script (VarName var), cfg)

-- TODO show the type description here too once that's ready
--      (add to the pretty instance?)
cmdType :: String -> ReplM ()
cmdType s = do
  (script, cfg) <- get
  print $ case iExpr cfg script s of
    Right expr -> prettyShow $ typeOf expr
    Left  err  -> show err

cmdShow :: String -> ReplM ()
cmdShow [] = get >>= \(s, _) -> liftIO $ mapM_ (putStrLn . prettyShow) $ s
cmdShow var = do
  (s, _) <- get
  print $ case lookup (VarName var) s of
    Nothing -> "VarName '" ++ var ++ "' not found"
    Just e  -> prettyShow e

cmdQuit :: String -> ReplM ()
cmdQuit _ = ReplM mzero

cmdBang :: String -> ReplM ()
cmdBang cmd = liftIO (runCommand cmd >>= waitForProcess) >> return ()

cmdSet :: String -> ReplM ()
cmdSet = undefined
  -- TODO split string into first word and the rest
  -- TODO case statement for first word: verbose, workdir, tmpdir, script?
  -- TODO script sets the default for cmdSave?
  -- TODO don't bother with script yet; start with the obvious ones

cmdConfig :: String -> ReplM ()
cmdConfig s = do
  let ws = words s
  case length ws of
    0 -> print "no variable" >> return ()
    1 -> undefined -- TODO write after making the config mutable
    _ -> print "too many variables" >> return ()
  return ()
