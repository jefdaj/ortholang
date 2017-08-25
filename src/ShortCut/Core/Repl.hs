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
  -- ( mkRepl
  -- , runRepl
  -- )
  where

import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO, MonadIO)
import Control.Monad.Identity   (mzero)
import Control.Monad.State.Lazy (get, put)
import Data.Char                (isSpace)
import Data.List                (isPrefixOf, filter)
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
import ShortCut.Core.Config     (showConfigField, setConfigField)
import System.Command           (runCommand, waitForProcess)
-- import System.IO.Silently       (capture_)
import System.IO                (Handle, hPutStrLn, stdout)
import System.Directory         (doesFileExist)
import System.Console.Haskeline (Settings(..), Completion, simpleCompletion, completeWord,
                                 CompletionFunc, listFiles, completeQuotedWord, filenameWordBreakChars)
import System.FilePath.Posix    ((</>))

--------------------
-- main interface --
--------------------

-- TODO load script from cfg if one was given on the command line
runRepl :: CutConfig -> IO ()
runRepl = mkRepl (repeat prompt) stdout
-- repl = mockRepl ["1 + 1", ":q"]

-- TODO now, just need to update the repl settings at each iteration! so needs to be in loop?

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
  state <- runReplM (replSettings blank) start blank
  let state' = (fromMaybe blank state)
  -- run main repl with initial state
  hPutStrLn hdl
    "Welcome to the ShortCut interpreter!\n\
    \Type :help for a list of the available commands."
  _ <- runReplM (replSettings state') (loop promptFns hdl) state'
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
loop [] hdl = runCmd hdl "quit" -- only happens when mock repl input runs out
loop (promptFn:promptFns) hdl = do
  mline <- promptFn "shortcut >> "
  runStep hdl mline
  loop promptFns hdl

-- TODO print + ignore all errors in here!
runStep :: Handle -> Maybe String -> ReplM ()
runStep hdl mline = case stripWhiteSpace (fromJust mline) of -- can this ever be Nothing??
  ""        -> return ()
  ('#':_  ) -> return ()
  (':':cmd) -> runCmd  hdl cmd
  line      -> runStatement hdl line

runStatement :: Handle -> String -> ReplM ()
runStatement hdl line = do
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
      when (isExpr st line)
        (liftIO $ evalScript hdl (scr',cfg)) -- TODO return only a string and print it here?

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
  -- , ("set"     , cmdSet   ) -- TODO is this obsolete?
  , ("quit"    , cmdQuit  )
  , ("!"       , cmdBang  )
  , ("config"  , cmdConfig)
  ]

---------------------------
-- run specific commands --
---------------------------

-- TODO load this from a file?
-- TODO update to include :config getting + setting
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
  path' <- liftIO $ absolutize path
  dfe   <- liftIO $ doesFileExist path'
  if not dfe
    then liftIO $ hPutStrLn hdl $ "no such file: " ++ path'
    else do
      new <- liftIO $ parseFile cfg path'
      case new of
        Left  e -> liftIO $ hPutStrLn hdl $ show e
        Right s -> put (s, cfg { cfgScript = Just path' })

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
-- cmdSet :: Handle -> String -> ReplM ()
-- cmdSet _ = undefined

-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: Handle -> String -> ReplM ()
cmdConfig hdl s = do
  (scr, cfg) <- get
  let ws = words s
  if (length ws == 0)
    then (liftIO $ hPutStrLn hdl (prettyShow cfg) >> return ()) -- TODO Pretty instance
    else if (length ws  > 2)
      then (liftIO $ hPutStrLn hdl "too many variables" >> return ())
      else if (length ws == 1)
        then liftIO $ hPutStrLn hdl $ showConfigField cfg $ head ws
        else case setConfigField cfg (head ws) (last ws) of
               Left err -> liftIO $ hPutStrLn hdl err
               Right cfg' -> put (scr, cfg')

--------------------
-- tab completion --
--------------------

-- TODO decide when to complete files vs variables and stuff:
--      files only if inside quotation marks?
--      or if started with one of the file-related commands (:!, :write, etc.)
--      otherwise shortcut entities only?

listCompletions :: MonadIO m => CutState -> String -> m [Completion]
listCompletions (scr,cfg) txt = do
  files <- listFiles txt
  let misc = map simpleCompletion $ filter (txt `isPrefixOf`) wordList
  return (files ++ misc)
  where
    wordList = fnNames ++ varNames ++ cmdNames
    fnNames  = concatMap (map fName . mFunctions) (cfgModules cfg)
    varNames = map ((\(CutVar v) -> v) . fst) scr
    cmdNames = map ((':':) . fst) cmds

-- this is mostly lifted from Haskeline's completeFile
myComplete :: MonadIO m => CutState -> CompletionFunc m
myComplete s = completeQuotedWord (Just '\\') "\"'" (listCompletions s)
             $ completeWord (Just '\\') ("\"\'" ++ filenameWordBreakChars)
                            (listCompletions s)

-- This is separate from the CutConfig because it shouldn't need changing.
-- TODO do we actually need the script here? only if we're recreating it every loop i guess
replSettings :: CutState -> Settings IO
replSettings s@(_,cfg) = Settings
  { complete       = myComplete s
  , historyFile    = Just $ cfgTmpDir cfg </> "history.txt"
  , autoAddHistory = True
  }
