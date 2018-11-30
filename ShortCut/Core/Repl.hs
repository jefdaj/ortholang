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

import qualified Data.Map as M
import System.Console.Haskeline hiding (catch)

import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO, MonadIO)
import Control.Monad.State.Lazy (get, put)
import Data.Char                (isSpace)
import Data.List                (isPrefixOf, isInfixOf, isSuffixOf, filter)
import Data.List.Split          (splitOn)
import Data.List.Utils          (delFromAL)
import Data.Maybe               (catMaybes)
-- import Data.Map                 (empty)
import Prelude           hiding (print)
import ShortCut.Core.Eval       (evalScript)
import ShortCut.Core.Parse      (isExpr, parseExpr, parseStatement, parseFile)
import ShortCut.Core.Types
import ShortCut.Core.Pretty     (pPrint, render, pPrintHdl, writeScript)
import ShortCut.Core.Util       (absolutize, stripWhiteSpace)
import ShortCut.Core.Config     (showConfigField, setConfigField)
import System.Command           (runCommand, waitForProcess)
import System.IO                (Handle, hPutStrLn, stdout)
import System.Directory         (doesFileExist)
import System.FilePath.Posix    ((</>))
import Control.Exception.Safe   -- (throwM)
import System.Console.ANSI      (clearScreen, cursorUp)
import Data.IORef               (readIORef)

--------------------
-- main interface --
--------------------

reallyClearScreen :: IO ()
reallyClearScreen = clearScreen >> cursorUp 1000

runRepl :: CutConfig -> Locks -> HashedSeqIDsRef -> IO ()
runRepl = mkRepl (repeat prompt) stdout

-- Like runRepl, but allows overriding the prompt function for golden testing.
-- Used by mockRepl in ShortCut/Core/Repl/Tests.hs
mkRepl :: [(String -> ReplM (Maybe String))] -> Handle
       -> CutConfig -> Locks -> HashedSeqIDsRef -> IO ()
mkRepl promptFns hdl cfg ref ids = do
  reallyClearScreen
  hPutStrLn hdl
    "Welcome to the ShortCut interpreter!\n\
    \Type :help for a list of the available commands."
  -- load initial script if any
  st <- case cfgScript cfg of
          Nothing   -> return  ([], cfg, ref, ids)
          Just path -> cmdLoad ([], cfg, ref, ids) hdl path
  -- run repl with initial state
  _ <- runReplM (replSettings st) (loop promptFns hdl) st
  return ()

shortCutPrompt :: String
-- shortCutPrompt = "--‣ "
-- shortCutPrompt = "❱❱❱ "
-- shortCutPrompt = "--❱ "
-- shortCutPrompt = "⋺  "
shortCutPrompt = ">>> "

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
-- loop [] hdl = get >>= \st -> liftIO (runCmd st hdl "quit") >> return ()
loop [] _ = return ()
loop (promptFn:promptFns) hdl = do
  Just line <- promptFn shortCutPrompt -- TODO can this fail?
  st  <- get
  st' <- liftIO $ try $ step st hdl line
  case st' of
    Right s -> put s >> loop promptFns hdl
    Left (SomeException e) -> do
      liftIO $ hPutStrLn hdl $ show e
      return () -- TODO *only* return if it's QuitRepl; ignore otherwise

-- handler :: Handle -> SomeException -> IO (Maybe a)
-- handler hdl e = hPutStrLn hdl ("error! " ++ show e) >> return Nothing

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
step :: CutState -> Handle -> String -> IO CutState
step st hdl line = case stripWhiteSpace line of
  ""        -> return st
  ('#':_  ) -> return st
  (':':cmd) -> runCmd st hdl cmd
  statement -> runStatement st hdl statement

runStatement :: CutState -> Handle -> String -> IO CutState
runStatement st@(scr, cfg, ref, ids) hdl line = case parseStatement st line of
  Left  e -> hPutStrLn hdl (show e) >> return st
  Right r -> do
    let st' = (updateScript scr r, cfg, ref, ids)
    when (isExpr st line) (evalScript hdl st')
    return st'

-- this is needed to avoid assigning a variable to itself,
-- which is especially a problem when auto-assigning "result"
-- TODO you should actually be able to do that, but it should replace
--      the `result` var with its current expression first
updateScript :: CutScript -> CutAssign -> CutScript
updateScript scr asn@(var, expr) =
  if var `elem` depsOf expr then scr else scr'
  where
    scr' = if var /= CutVar "result" && var `elem` map fst scr
             then replaceVar asn scr
             else delFromAL scr var ++ [asn]

-- replace an existing var in a script
replaceVar :: CutAssign -> CutScript -> CutScript
replaceVar a1@(v1, _) = map $ \a2@(v2, _) -> if v1 == v2 then a1 else a2

--------------------------
-- dispatch to commands --
--------------------------

runCmd :: CutState -> Handle -> String -> IO CutState
runCmd st@(_, cfg, _, _) hdl line = case matches of
  [(_, fn)] -> fn st hdl $ stripWhiteSpace args
  []        -> hPutStrLn hdl ("unknown command: "   ++ cmd) >> return st
  _         -> hPutStrLn hdl ("ambiguous command: " ++ cmd) >> return st
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) (cmds cfg)

cmds :: CutConfig -> [(String, CutState -> Handle -> String -> IO CutState)]
cmds cfg =
  [ ("help"    , cmdHelp  )
  , ("load"    , cmdLoad  )
  , ("write"   , cmdSave  )
  , ("depends" , cmdDeps  )
  , ("rdepends", cmdRDeps )
  , ("drop"    , cmdDrop  )
  , ("type"    , cmdType  )
  , ("show"    , cmdShow  )
  , ("quit"    , cmdQuit  )
  , ("config"  , cmdConfig)
  ]
  ++ if cfgSecure cfg then [] else [("!", cmdBang)]

---------------------------
-- run specific commands --
---------------------------

-- TODO load this from a file?
-- TODO update to include :config getting + setting
cmdHelp :: CutState -> Handle -> String -> IO CutState
cmdHelp st@(_, cfg, _, _) hdl line = hPutStrLn hdl msg >> return st
  where
    fHelp f = fTypeDesc f ++ case fDesc f of
                Nothing -> ""
                Just s  -> "\n\n" ++ s ++ "\n"
    tHelp t = "The " ++ tExt t ++ " extension is for " ++ tDesc t ++ " files.\n\n" ++ tFnList t
    tFnList t = unlines $ ["You can create them with these functions:"] ++ outputs ++ ["", "And use them with these functions:"] ++ inputs
                where
                  descs = map (\f -> "  " ++ fTypeDesc f) (listFunctions cfg)
                  outputs = filter (\d -> (tExt t) `isInfixOf` (unwords $ tail $ splitOn ">" $ unwords $ tail $ splitOn ":" d)) descs
                  inputs  = filter (\d -> (tExt t) `isInfixOf` (head $ splitOn ">" $ unwords $ tail $ splitOn ":" d)) descs
    msg = case words line of
            [w] -> head $ catMaybes
                     [ fmap fHelp $ findFunction cfg w
                     , fmap tHelp $ findType     cfg w
                     , Just $ "sorry, no function or filetype is named '" ++ w ++ "'"
                     ]
            _ -> def
    -- TODO extract this to a file alonside usage.txt?
    def = "You can type or paste ShortCut code here to run it, \
          \same as in a script.\n\
          \There are also some extra commands:\n\n\
          \:help     to print info about a function or filetype\n\
          \:load     to load a script (same as typing the file contents)\n\
          \:write    to write the current script to a file\n\
          \:depends  to show which variables a given variable depends on\n\
          \:rdepends to show which variables depend on the given variable\n\
          \:drop     to discard the current script (or a specific variable)\n\
          \:quit     to discard the current script and exit the interpreter\n\
          \:type     to print the type of an expression\n\
          \:show     to print an expression along with its type"
          ++ if cfgSecure cfg
               then ""
               else "\n:!        to run the rest of the line as a shell command"

-- TODO this is totally duplicating code from putAssign; factor out
cmdLoad :: CutState -> Handle -> String -> IO CutState
cmdLoad st@(_, cfg, ref, ids) hdl path = do
  path' <- absolutize path
  dfe   <- doesFileExist path'
  if not dfe
    then hPutStrLn hdl ("no such file: " ++ path') >> return st
    else do
      let cfg' = cfg { cfgScript = Just path' }
      new <- parseFile cfg' ref ids path'
      case new of
        Left  e -> hPutStrLn hdl (show e) >> return st
        Right s -> return (s, cfg', ref, ids)

cmdSave :: CutState -> Handle -> String -> IO CutState
cmdSave st@(scr, _, _, _) hdl line = do
  case words line of
    [path] -> saveScript scr path
    [var, path] -> case lookup (CutVar var) scr of
      Nothing -> hPutStrLn hdl $ "Var '" ++ var ++ "' not found"
      Just e  -> saveScript (depsOnly e scr) path
    _ -> hPutStrLn hdl $ "invalid save command: '" ++ line ++ "'"
  return st

-- TODO where should this go?
depsOnly :: CutExpr -> CutScript -> CutScript
depsOnly expr scr = deps ++ [res]
  where
    deps = filter (\(v,_) -> (elem v $ depsOf expr)) scr
    res  = (CutVar "result", expr)

-- TODO where should this go?
saveScript :: CutScript -> FilePath -> IO ()
saveScript scr path = absolutize path >>= \p -> writeScript p scr

-- TODO factor out the variable lookup stuff
-- TODO except, this should work with expressions too!
cmdDeps :: CutState -> Handle -> String -> IO CutState
cmdDeps st@(scr, cfg, _, _) hdl var = do
  case lookup (CutVar var) scr of
    Nothing -> hPutStrLn hdl $ "Var '" ++ var ++ "' not found"
    -- Just e  -> prettyAssigns hdl (\(v,_) -> elem v $ (CutVar var):depsOf e) scr
    Just e  -> pPrintHdl cfg hdl $ filter (\(v,_) -> elem v $ (CutVar var):depsOf e) scr
  return st

-- TODO move to Pretty.hs
-- prettyAssigns :: Handle -> (CutAssign -> Bool) -> CutScript -> IO ()
-- prettyAssigns hdl fn scr = do
  -- txt <- renderIO $ pPrint $ filter fn scr
  -- hPutStrLn hdl txt

cmdRDeps :: CutState -> Handle -> String -> IO CutState
cmdRDeps st@(scr, cfg, _, _) hdl var = do
  let var' = CutVar var
  case lookup var' scr of
    Nothing -> hPutStrLn hdl $ "Var '" ++ var ++ "' not found"
    Just _  -> pPrintHdl cfg hdl $ filter (\(v,_) -> elem v $ (CutVar var):rDepsOf scr var') scr
  return st

-- TODO factor out the variable lookup stuff
cmdDrop :: CutState -> Handle -> String -> IO CutState
cmdDrop (_, cfg, ref, ids) _ [] = return ([], cfg, ref, ids) -- TODO drop ids too?
cmdDrop st@(scr, cfg, ref, ids) hdl var = do
  let v = CutVar var
  case lookup v scr of
    Nothing -> hPutStrLn hdl ("Var '" ++ var ++ "' not found") >> return st
    Just _  -> return (delFromAL scr v, cfg, ref, ids)

cmdType :: CutState -> Handle -> String -> IO CutState
cmdType st@(scr, cfg, _, _) hdl s = hPutStrLn hdl typeInfo >> return st
  where
    typeInfo = case stripWhiteSpace s of
      "" -> allTypes
      s' -> oneType s'
    oneType e = case findFunction cfg e of
      Just f  -> fTypeDesc f
      Nothing -> showExprType st e -- TODO also show the expr itself?
    allTypes = init $ unlines $ map showAssignType scr

showExprType :: CutState -> String -> String
showExprType st e = case parseExpr st e of
  Right expr -> show $ typeOf expr
  Left  err  -> show err

showAssignType :: CutAssign -> String
showAssignType (CutVar v, e) = unwords [typedVar, "=", prettyExpr]
  where
    -- parentheses also work:
    -- typedVar = v ++ " (" ++ show (typeOf e) ++ ")"
    typedVar = v ++ "." ++ show (typeOf e)
    prettyExpr = render $ pPrint e

-- TODO factor out the variable lookup stuff
cmdShow :: CutState -> Handle -> String -> IO CutState
cmdShow st@(s, c, _, _) hdl [] = mapM_ (pPrintHdl c hdl) s >> return st
cmdShow st@(scr, cfg, _, _) hdl var = do
  case lookup (CutVar var) scr of
    Nothing -> hPutStrLn hdl $ "Var '" ++ var ++ "' not found"
    Just e  -> pPrintHdl cfg hdl e
  return st

-- TODO does this one need to be a special case now?
cmdQuit :: CutState -> Handle -> String -> IO CutState
cmdQuit _ _ _ = throw QuitRepl
-- cmdQuit _ _ _ = ioError $ userError "Bye for now!"

cmdBang :: CutState -> Handle -> String -> IO CutState
cmdBang st _ cmd = (runCommand cmd >>= waitForProcess) >> return st

-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: CutState -> Handle -> String -> IO CutState
cmdConfig st@(scr, cfg, ref, ids) hdl s = do
  let ws = words s
  if (length ws == 0)
    then pPrintHdl cfg hdl cfg >> return st -- TODO Pretty instance
    else if (length ws  > 2)
      then hPutStrLn hdl "too many variables" >> return st
      else if (length ws == 1)
        then hPutStrLn hdl (showConfigField cfg $ head ws) >> return st
        else case setConfigField cfg (head ws) (last ws) of
               Left err -> hPutStrLn hdl err >> return st
               Right cfg' -> return (scr, cfg', ref, ids)

--------------------
-- tab completion --
--------------------

-- complete things in quotes: filenames, seqids
quotedCompletions :: MonadIO m => CutState -> String -> m [Completion]
quotedCompletions (_, _, _, idRef) wordSoFar = do
  files  <- listFiles wordSoFar
  seqIDs <- fmap (map $ head . words) $ fmap M.elems $ liftIO $ readIORef idRef
  let seqIDs' = map simpleCompletion $ filter (wordSoFar `isPrefixOf`) seqIDs
  return $ files ++ seqIDs'

-- complete everything else: fn names, var names, :commands, types
-- these can be filenames too, but only if the line starts with a :command
nakedCompletions :: MonadIO m => CutState -> String -> String -> m [Completion]
nakedCompletions (scr, cfg, _, _) lineReveresed wordSoFar = do
  files <- if ":" `isSuffixOf` lineReveresed then listFiles wordSoFar else return []
  return $ files ++ (map simpleCompletion $ filter (wordSoFar `isPrefixOf`) wordSoFarList)
  where
    wordSoFarList = fnNames ++ varNames ++ cmdNames ++ typeExts
    fnNames  = concatMap (map fName . mFunctions) (cfgModules cfg)
    varNames = map ((\(CutVar v) -> v) . fst) scr
    cmdNames = map ((':':) . fst) (cmds cfg)
    typeExts = map extOf $ concatMap mTypes $ cfgModules cfg

-- this is mostly lifted from Haskeline's completeFile
myComplete :: MonadIO m => CutState -> CompletionFunc m
myComplete s = completeQuotedWord   (Just '\\') "\"'" (quotedCompletions s)
             $ completeWordWithPrev (Just '\\') ("\"\'" ++ filenameWordBreakChars)
                                    (nakedCompletions s)

-- This is separate from the CutConfig because it shouldn't need changing.
-- TODO do we actually need the script here? only if we're recreating it every loop i guess
replSettings :: CutState -> Settings IO
replSettings s@(_, cfg, _, _) = Settings
  { complete       = myComplete s
  , historyFile    = Just $ cfgTmpDir cfg </> "history.txt"
  , autoAddHistory = True
  }
