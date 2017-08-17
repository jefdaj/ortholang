{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO could simplify to the same code everywhere except you pass the handle (file vs stdout)?

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
-- import Debug.Trace
import Prelude           hiding (print)
-- import ShortCut.Core.Compile    (compileScript)
import ShortCut.Core.Eval       (evalScript)
import ShortCut.Core.Parse      (isExpr, parseExpr, parseStatement, parseFile)
import ShortCut.Core.Types
import ShortCut.Core.Pretty     (prettyShow)
import ShortCut.Core.Util       (absolutize, stripWhiteSpace)
import System.Command           (runCommand, waitForProcess)
-- import System.IO.Silently       (capture_)
import System.IO                (Handle, hPutStrLn, stdout)
import System.Directory         (doesFileExist)

--------------------
-- main interface --
--------------------

-- TODO load script from cfg if one was given on the command line
runRepl :: CutConfig -> IO ()
runRepl = mkRepl (repeat prompt) stdout
-- repl = mockRepl ["1 + 1", ":q"]

-- Like runRepl, but allows overriding the prompt function for golden testing.
-- Used by mockRepl in ShortCut/Core/Repl/Tests.hs
-- TODO pass modules list on here
-- TODO try passing my own print function here that sends to a file instead
mkRepl :: [(String -> ReplM (Maybe String))] -> Handle -> CutConfig -> IO ()
-- mkRepl promptFns cfg = welcome >> runReplM ((lift $ lift $ getExternalPrint) >>= loop promptFns) ([], cfg) >> goodbye
mkRepl promptFns hdl cfg = do
  -- load initial state, if any
  let blank = ([], cfg)
      start = case cfgScript cfg of
                Nothing   -> return () 
                Just path -> cmdLoad hdl path
  state <- runReplM start blank
  -- run main repl with initial state
  hPutStrLn hdl
    "Welcome to the ShortCut interpreter!\n\
    \Type :help for a list of the available commands."
  _ <- runReplM (loop promptFns hdl) (fromMaybe blank state)
  hPutStrLn hdl "Bye for now!"

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
loop :: [(String -> ReplM (Maybe String))] -> Handle -> ReplM ()
loop [] hdl = runCmd hdl "quit"
loop (promptFn:promptFns) hdl = do
  mline <- promptFn "shortcut >> "
  case stripWhiteSpace (fromJust mline) of -- can this ever be Nothing??
    ""        -> return () -- TODO also handle comments this way (for examples mostly)
    (':':cmd) -> runCmd hdl cmd
    line      -> do
      st@(scr, cfg) <- get
      case parseStatement st line of
        Left  e -> liftIO $ hPutStrLn hdl $ show e
        Right r -> do
          let scr' = updateScript scr r
          put (scr', cfg)
          -- even though result gets added to the script either way,
          -- still have to check whether to print it
          -- TODO should be able to factor this out and put in Eval.hs
          -- TODO nothing should be run when manually assigning result!
          when (isExpr st line) (liftIO $ evalScript hdl (scr',cfg)) -- TODO return only a string and print it here?
  loop promptFns hdl

-- this is needed to avoid assigning a variable to itself,
-- which is especially a problem when auto-assigning "result"
-- TODO also catch variables assigned to things depending on themselves
--      (later, with the "which variables does this depend on" function)
updateScript :: CutScript -> CutAssign -> CutScript
updateScript scr asn@(var, expr) =
  case expr of
    (CutRef _ _ _ var') -> if var' == var then scr else scr'
    _ -> scr'
    where
      scr' = delFromAL scr var ++ [asn]


--------------------------
-- dispatch to commands --
--------------------------

runCmd :: Handle -> String -> ReplM ()
runCmd hdl line = case matches of
  [(_, fn)] -> fn hdl $ stripWhiteSpace args
  []        -> liftIO $ hPutStrLn hdl $ "unknown command: "   ++ cmd
  _         -> liftIO $ hPutStrLn hdl $ "ambiguous command: " ++ cmd
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) cmds

cmds :: [(String, Handle -> String -> ReplM ())]
cmds =
  [ ("help"    , cmdHelp  )
  , ("load"    , cmdLoad  )
  , ("write"   , cmdSave  )
  , ("depends" , cmdDeps  )
  , ("rdepends", cmdRDeps )
  , ("drop"    , cmdDrop  )
  , ("type"    , cmdType  )
  , ("show"    , cmdShow  )
  , ("set"     , cmdSet   )
  , ("quit"    , cmdQuit  )
  , ("!"       , cmdBang  )
  , ("config"  , cmdConfig)
  ]

---------------------------
-- run specific commands --
---------------------------

cmdHelp :: Handle -> String -> ReplM ()
cmdHelp hdl _ = liftIO $ hPutStrLn hdl
  "You can type or paste ShortCut code here to run it, same as in a script.\n\
  \There are also some extra commands:\n\n\
  \:help     to print this help text\n\
  \:load     to load a script (same as typing the file contents)\n\
  \:write    to write the current script to a file\n\
  \:depends  to show which variables a given variable depends on\n\
  \:rdepends to show which variables depend on the given variable\n\
  \:drop     to discard the current script (or a specific variable)\n\
  \:quit     to discard the current script and exit the interpreter\n\
  \:type     to print the type of an expression\n\
  \:show     to print an expression along with its type\n\
  \:!        to run the rest of the line as a shell command"

-- TODO this is totally duplicating code from putAssign; factor out
cmdLoad :: Handle -> String -> ReplM ()
cmdLoad hdl path = do
  (_, cfg)  <- get
  dfe <- liftIO $ doesFileExist path
  if not dfe
    then liftIO $ hPutStrLn hdl $ "no such file: " ++ path
    else do
      new <- liftIO $ parseFile cfg path
      case new of
        Left  e -> liftIO $ hPutStrLn hdl $ show e
        Right s -> put (s, cfg)

-- TODO this needs to read a second arg for the var to be main?
--      or just tell people to define main themselves?
-- TODO replace showHack with something nicer
cmdSave :: Handle -> String -> ReplM ()
cmdSave _ path = do
  path' <- liftIO $ absolutize path
  get >>= \s -> liftIO $ writeFile path' $ showHack $ fst s
  where
    showHack = unlines . map prettyShow

-- TODO factor out the variable lookup stuff
-- TODO except, this should work with expressions too!
cmdDeps :: Handle -> String -> ReplM ()
cmdDeps hdl var = do
  (scr, _) <- get
  liftIO $ hPutStrLn hdl $ case lookup (CutVar var) scr of
    Nothing -> "Var '" ++ var ++ "' not found"
    Just e  -> prettyAssigns (\(v,_) -> elem v $ depsOf e) scr

-- TODO move to Pretty.hs
prettyAssigns :: (CutAssign -> Bool) -> CutScript -> String
prettyAssigns fn scr = stripWhiteSpace $ unlines $ map prettyShow $ filter fn scr

cmdRDeps :: Handle -> String -> ReplM ()
cmdRDeps hdl var = do
  (scr, _) <- get
  let var' = CutVar var
  liftIO $ hPutStrLn hdl $ case lookup var' scr of
    Nothing -> "Var '" ++ var ++ "' not found"
    Just _  -> prettyAssigns (\(v,_) -> elem v $ rDepsOf scr var') scr

-- TODO factor out the variable lookup stuff
cmdDrop :: Handle -> String -> ReplM ()
cmdDrop _ [] = get >>= \(_, cfg) -> put ([], cfg)
cmdDrop hdl var = do
  (scr, cfg) <- get
  let v = CutVar var
  case lookup v scr of
    Nothing -> liftIO $ hPutStrLn hdl $ "Var '" ++ var ++ "' not found"
    Just _  -> put (delFromAL scr v, cfg)

-- TODO show the type description here too once that's ready
--      (add to the pretty instance?)
cmdType :: Handle -> String -> ReplM ()
cmdType hdl s = do
  state <- get
  liftIO $ hPutStrLn hdl $ case parseExpr state s of
    Right expr -> show $ typeOf expr
    Left  err  -> show err

-- TODO factor out the variable lookup stuff
cmdShow :: Handle -> String -> ReplM ()
cmdShow hdl [] = get >>= \(s, _) -> liftIO $ mapM_ (hPutStrLn hdl . prettyShow) s
cmdShow hdl var = do
  (scr, _) <- get
  liftIO $ hPutStrLn hdl $ case lookup (CutVar var) scr of
    Nothing -> "Var '" ++ var ++ "' not found"
    Just e  -> prettyShow e

cmdQuit :: Handle -> String -> ReplM ()
cmdQuit _ _ = mzero

cmdBang :: Handle -> String -> ReplM ()
cmdBang _ cmd = liftIO (runCommand cmd >>= waitForProcess) >> return ()

-- TODO split string into first word and the rest
-- TODO case statement for first word: verbose, workdir, tmpdir, script?
-- TODO script sets the default for cmdSave?
-- TODO don't bother with script yet; start with the obvious ones
cmdSet :: Handle -> String -> ReplM ()
cmdSet _ = undefined

-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: Handle -> String -> ReplM ()
cmdConfig hdl s = do
  (_, cfg) <- get
  let ws = words s
  if (length ws == 0)
    then (liftIO $ hPutStrLn hdl (prettyShow cfg) >> return ()) -- TODO Pretty instance
    else if (length ws  > 2)
      then (liftIO $ hPutStrLn hdl "too many variables" >> return ())
      -- TODO separate into get/set cases:
      else if (length ws == 1)
        then (cmdConfigShow hdl (head ws))
        else (cmdConfigSet hdl (head ws) (last ws))

cmdConfigShow :: Handle -> String -> ReplM ()
cmdConfigShow hdl key = get >>= \(_, cfg) -> liftIO $ hPutStrLn hdl $ fn cfg
  where
    fn = case key of
          "script"  -> (\c -> fromMaybe "none" $ cfgScript c)
          "verbose" -> (\c -> show $ cfgDebug c)
          "tmpdir"  -> cfgTmpDir
          _ -> \_ -> "no such config entry"

cmdConfigSet :: Handle -> String -> String -> ReplM ()
cmdConfigSet _ key val = do
  (scr, cfg) <- get
  case key of
    "script"  -> put (scr, cfg { cfgScript  = Just val })
    "verbose" -> put (scr, cfg { cfgDebug = read val })
    "tmpdir"  -> put (scr, cfg { cfgTmpDir  = val })
    _ -> fail $ "no such variable '" ++ key ++ "'"
