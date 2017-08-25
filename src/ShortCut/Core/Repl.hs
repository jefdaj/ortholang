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

import System.Console.Haskeline

import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO, MonadIO)
import Control.Monad.Identity   (mzero)
import Control.Monad.State.Lazy (get, put)
import Data.Char                (isSpace)
import Data.List                (isPrefixOf, filter)
import Data.List.Utils          (delFromAL)
import Data.Maybe               (fromJust)
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
  state <- case cfgScript cfg of
             Nothing   -> return blank
             Just path -> cmdLoad blank hdl path
  -- state <- runReplM (replSettings blank') blank'
  -- let state' = fromMaybe loaded state
  -- run main repl with initial state
  hPutStrLn hdl
    "Welcome to the ShortCut interpreter!\n\
    \Type :help for a list of the available commands."
  _ <- runReplM (replSettings state) (loop promptFns hdl) state
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
loop [] hdl = do -- only happens when mock repl input runs out
  st <- get
  _  <- liftIO $ runCmd st hdl "quit"
  return ()
loop (promptFn:promptFns) hdl = do
  mline <- promptFn "shortcut >> "
  st  <- get
  st' <- liftIO $ step st hdl $ fromJust mline -- can this ever be Nothing??
  put st'
  loop promptFns hdl

-- TODO try to lift this into ReplM and wrap the entire `step` in it!
--      turns out you have to do that to catch the parse errors!
--      Would making ReplM a newtype with some derivations help?
-- TODO once that works, should it move to Types.hs by ReplM?
printErrors :: Handle -> IO () -> IO ()
printErrors hdl = handle handler
  where
    handler :: SomeException -> IO ()
    handler e = do
      liftIO $ hPutStrLn hdl $ "error! " ++ show e
      return ()

-- Attempts to process a line of input, but prints an error and falls back to
-- the current state if anything goes wrong. This should eventually be the only
-- place exceptions are caught.
step :: CutState -> Handle -> String -> IO CutState
step st hdl line = case stripWhiteSpace line of
  ""        -> return st
  ('#':_  ) -> return st
  (':':cmd) -> runCmd st hdl cmd
  statement -> runStatement st hdl statement

runStatement :: CutState -> Handle -> String -> IO CutState
runStatement st@(scr,cfg) hdl line = do
  case parseStatement st line of
    Left  e -> hPutStrLn hdl (show e) >> return st
    Right r -> do
      let st' = (updateScript scr r, cfg)
      when (isExpr st line) (evalScript hdl st')
      return st'

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

runCmd :: CutState -> Handle -> String -> IO CutState
runCmd st hdl line = case matches of
  [(_, fn)] -> fn st hdl $ stripWhiteSpace args
  []        -> hPutStrLn hdl ("unknown command: "   ++ cmd) >> return st
  _         -> hPutStrLn hdl ("ambiguous command: " ++ cmd) >> return st
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) cmds

cmds :: [(String, CutState -> Handle -> String -> IO CutState)]
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
cmdHelp :: CutState -> Handle -> String -> IO CutState
cmdHelp st hdl _ = hPutStrLn hdl msg >> return st
  where
    -- TODO extract this to a file alonside usage.txt
    msg = "You can type or paste ShortCut code here to run it,\
          \same as in a script.\n\
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
cmdLoad :: CutState -> Handle -> String -> IO CutState
cmdLoad st@(_,cfg) hdl path = do
  path' <- absolutize path
  dfe   <- doesFileExist path'
  if not dfe
    then hPutStrLn hdl ("no such file: " ++ path') >> return st
    else do
      new <- parseFile cfg path'
      case new of
        Left  e -> hPutStrLn hdl (show e) >> return st
        Right s -> return (s, cfg { cfgScript = Just path' })

-- TODO this needs to read a second arg for the var to be main?
--      or just tell people to define main themselves?
-- TODO replace showHack with something nicer
cmdSave :: CutState -> Handle -> String -> IO CutState
cmdSave st _ path = do
  path' <- absolutize path
  writeFile path' $ showHack $ fst st
  return st
  where
    showHack = unlines . map prettyShow

-- TODO factor out the variable lookup stuff
-- TODO except, this should work with expressions too!
cmdDeps :: CutState -> Handle -> String -> IO CutState
cmdDeps st@(scr,_) hdl var = do
  hPutStrLn hdl $ case lookup (CutVar var) scr of
    Nothing -> "Var '" ++ var ++ "' not found"
    Just e  -> prettyAssigns (\(v,_) -> elem v $ depsOf e) scr
  return st

-- TODO move to Pretty.hs
prettyAssigns :: (CutAssign -> Bool) -> CutScript -> String
prettyAssigns fn scr = stripWhiteSpace $ unlines $ map prettyShow $ filter fn scr

cmdRDeps :: CutState -> Handle -> String -> IO CutState
cmdRDeps st@(scr,_) hdl var = do
  let var' = CutVar var
  hPutStrLn hdl $ case lookup var' scr of
    Nothing -> "Var '" ++ var ++ "' not found"
    Just _  -> prettyAssigns (\(v,_) -> elem v $ rDepsOf scr var') scr
  return st

-- TODO factor out the variable lookup stuff
cmdDrop :: CutState -> Handle -> String -> IO CutState
cmdDrop (_,cfg) _ [] = return ([], cfg)
cmdDrop st@(scr,cfg) hdl var = do
  let v = CutVar var
  case lookup v scr of
    Nothing -> hPutStrLn hdl ("Var '" ++ var ++ "' not found") >> return st
    Just _  -> return (delFromAL scr v, cfg)

-- TODO show the type description here too once that's ready
--      (add to the pretty instance?)
cmdType :: CutState -> Handle -> String -> IO CutState
cmdType state hdl s = do
  hPutStrLn hdl $ case parseExpr state s of
    Right expr -> show $ typeOf expr
    Left  err  -> show err
  return state

-- TODO factor out the variable lookup stuff
cmdShow :: CutState -> Handle -> String -> IO CutState
cmdShow st@(s,_) hdl [] = mapM_ (hPutStrLn hdl . prettyShow) s >> return st
cmdShow st@(scr,_) hdl var = do
  hPutStrLn hdl $ case lookup (CutVar var) scr of
    Nothing -> "Var '" ++ var ++ "' not found"
    Just e  -> prettyShow e
  return st

-- TODO does this one need to be a special case now?
cmdQuit :: CutState -> Handle -> String -> IO CutState
cmdQuit _ _ _ = mzero

cmdBang :: CutState -> Handle -> String -> IO CutState
cmdBang st _ cmd = (runCommand cmd >>= waitForProcess) >> return st

-- TODO split string into first word and the rest
-- TODO case statement for first word: verbose, workdir, tmpdir, script?
-- TODO script sets the default for cmdSave?
-- TODO don't bother with script yet; start with the obvious ones
-- cmdSet :: CutState -> Handle -> String -> IO CutState
-- cmdSet _ = undefined

-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: CutState -> Handle -> String -> IO CutState
cmdConfig st@(scr,cfg) hdl s = do
  let ws = words s
  if (length ws == 0)
    then hPutStrLn hdl (prettyShow cfg) >> return st -- TODO Pretty instance
    else if (length ws  > 2)
      then hPutStrLn hdl "too many variables" >> return st
      else if (length ws == 1)
        then hPutStrLn hdl (showConfigField cfg $ head ws) >> return st
        else case setConfigField cfg (head ws) (last ws) of
               Left err -> hPutStrLn hdl err >> return st
               Right cfg' -> return (scr, cfg')

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
