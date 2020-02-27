{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO no welcome if going to load a file + clear the screen anyway

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

module OrthoLang.Core.Repl
  -- ( mkRepl
  -- , runRepl
  -- )
  where

import qualified Data.Map.Strict as M
import System.Console.Haskeline hiding (catch)

import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO, MonadIO)
import Control.Monad.State.Lazy (get, put)
import Data.Char                (isSpace)
import Data.List                (isPrefixOf, isSuffixOf, filter, delete)
import Data.List.Split          (splitOn)
import Data.List.Utils          (delFromAL)
import Data.Maybe               (catMaybes)
-- import Data.Map.Strict                 (empty)
import Prelude           hiding (print)
import OrthoLang.Core.Eval       (evalScript)
import OrthoLang.Core.Parse      (isExpr, parseExpr, parseStatement, parseFile)
import OrthoLang.Core.Types
import OrthoLang.Core.Pretty     (pPrint, render, pPrintHdl, writeScript)
import OrthoLang.Core.Util       (absolutize, stripWhiteSpace, justOrDie, headOrDie)
import OrthoLang.Core.Config     (showConfigField, setConfigField, getDoc)
-- import System.Command           (runCommand, waitForProcess)
import System.Process           (runCommand, waitForProcess)
import System.IO                (Handle, hPutStrLn, stdout)
import System.Directory         (doesFileExist)
import System.FilePath.Posix    ((</>))
import Control.Exception.Safe   (Typeable, throw, try)
import System.Console.ANSI      (clearScreen, cursorUp)
import Data.IORef               (readIORef)
import Development.Shake.FilePath (takeFileName)

--------------------
-- main interface --
--------------------

clear :: IO ()
clear = clearScreen >> cursorUp 1000

runRepl :: OrthoLangConfig -> Locks -> HashedIDsRef -> IO ()
runRepl = mkRepl (repeat prompt) stdout

-- Like runRepl, but allows overriding the prompt function for golden testing.
-- Used by mockRepl in OrthoLang/Core/Repl/Tests.hs
mkRepl :: [(String -> ReplM (Maybe String))] -> Handle
       -> OrthoLangConfig -> Locks -> HashedIDsRef -> IO ()
mkRepl promptFns hdl cfg ref ids = do
  -- load initial script if any
  st <- case cfgScript cfg of
          Nothing -> do
            clear
            hPutStrLn hdl
              "Welcome to the OrthoLang interpreter!\n\
              \Type :help for a list of the available commands."
            return  ([], cfg, ref, ids)
          Just path -> cmdLoad ([], cfg, ref, ids) hdl path
  -- run repl with initial state
  _ <- runReplM (replSettings st) (loop promptFns hdl) st
  return ()

-- promptArrow = " --‣ "
-- promptArrow = " ❱❱❱ "
-- promptArrow = " --❱ "
-- promptArrow = " ⋺  "
-- promptArrow = " >> "
-- promptArrow = "-> "
promptArrow :: String
promptArrow = " —▶ "

shortOrthoLangPrompt :: OrthoLangConfig -> String
shortOrthoLangPrompt cfg = "\n" ++ name ++ promptArrow -- TODO no newline if last command didn't print anything
  where
    name = case cfgScript cfg of
      Nothing -> "ortholang"
      Just s  -> takeFileName s

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
  st@(_, cfg, _, _)  <- get
  Just line <- promptFn $ shortOrthoLangPrompt cfg -- TODO can this fail?
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
step :: OrthoLangState -> Handle -> String -> IO OrthoLangState
step st hdl line = case stripWhiteSpace line of
  ""        -> return st
  ('#':_  ) -> return st
  (':':cmd) -> runCmd st hdl cmd
  statement -> runStatement st hdl statement

runStatement :: OrthoLangState -> Handle -> String -> IO OrthoLangState
runStatement st@(scr, cfg, ref, ids) hdl line = case parseStatement st line of
  Left  e -> hPutStrLn hdl (show e) >> return st
  Right r -> do
    let st' = (updateVars scr r, cfg, ref, ids)
    when (isExpr st line) (evalScript hdl st')
    return st'

-- this is needed to avoid assigning a variable literally to itself,
-- which is especially a problem when auto-assigning "result"
-- TODO is this where we can easily require the replacement var's type to match if it has deps?
-- TODO what happens if you try that in a script? it should fail i guess?
updateVars :: OrthoLangScript -> OrthoLangAssign -> OrthoLangScript
updateVars scr asn@(var, _) =
  if var /= res && var `elem` map fst scr -- TODO what's the map part for?
    then replaceVar asn' scr
    else delFromAL scr var ++ [asn']
  where
    res = OrthoLangVar (ReplaceID Nothing) "result"
    asn' = removeSelfReferences scr asn

-- replace an existing var in a script
replaceVar :: OrthoLangAssign -> OrthoLangScript -> OrthoLangScript
replaceVar a1@(v1, _) = map $ \a2@(v2, _) -> if v1 == v2 then a1 else a2

-- makes it ok to assign a var to itself in the repl
-- by replacing the reference with its value at that point
-- TODO forbid this in scripts though
removeSelfReferences :: OrthoLangScript -> OrthoLangAssign -> OrthoLangAssign
removeSelfReferences s a@(v, e) = if not (v `elem` depsOf e) then a else (v, dereference s v e)

-- does the actual work of removing self-references
dereference :: OrthoLangScript -> OrthoLangVar -> OrthoLangExpr -> OrthoLangExpr
dereference scr var e@(OrthoLangRef _ _ _ v2)
  | var == v2 = justOrDie "failed to dereference variable!" $ lookup var scr
  | otherwise = e
dereference _   _   e@(OrthoLangLit _ _ _) = e
dereference _   _   (OrthoLangRules _) = error "implement this! or rethink?"
dereference scr var (OrthoLangBop  t n vs s e1 e2) = OrthoLangBop  t n (delete var vs) s (dereference scr var e1) (dereference scr var e2)
dereference scr var (OrthoLangFun  t n vs s es   ) = OrthoLangFun  t n (delete var vs) s (map (dereference scr var) es)
dereference scr var (OrthoLangList t n vs   es   ) = OrthoLangList t n (delete var vs)   (map (dereference scr var) es)

--------------------------
-- dispatch to commands --
--------------------------

runCmd :: OrthoLangState -> Handle -> String -> IO OrthoLangState
runCmd st@(_, cfg, _, _) hdl line = case matches of
  [(_, fn)] -> fn st hdl $ stripWhiteSpace args
  []        -> hPutStrLn hdl ("unknown command: "   ++ cmd) >> return st
  _         -> hPutStrLn hdl ("ambiguous command: " ++ cmd) >> return st
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) (cmds cfg)

cmds :: OrthoLangConfig -> [(String, OrthoLangState -> Handle -> String -> IO OrthoLangState)]
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

---------------------------
-- run specific commands --
---------------------------

-- TODO load this from a file?
-- TODO update to include :config getting + setting
-- TODO if possible, make this open in `less`?
-- TODO why does this one have a weird path before the :help text?
cmdHelp :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdHelp st@(_, cfg, _, _) hdl line = do
  doc <- case words line of
           [w] -> headOrDie "failed to look up cmdHelp content" $ catMaybes
                    [ fmap fHelp $ findFunction cfg w
                    , fmap mHelp $ findModule   cfg w
                    , fmap (tHelp cfg) $ findType cfg w
                    , Just $ getDoc ["notfound"]
                    ]
           _ -> getDoc ["repl"]
  hPutStrLn hdl doc >> return st

mHelp :: OrthoLangModule -> IO String
mHelp m = getDoc ["modules" </> mName m]

-- TODO move somewhere better
fHelp :: OrthoLangFunction -> IO String
fHelp f = do
  doc <- getDoc $ map ("functions" </>) (fNames f)
  let msg = "\n" ++ fTypeDesc f ++ "\n\n" ++ doc
  return msg

-- TODO move somewhere better
tHelp :: OrthoLangConfig -> OrthoLangType -> IO String
tHelp cfg t = do
  doc <- getDoc ["types" </> extOf t]
  let msg = "The ." ++ extOf t ++ " extension is for " ++ descOf t ++ " files.\n\n"
            ++ doc ++ "\n\n"
            ++ tFnList
  return msg
  where
    outputs = listFunctionTypesWithOutput cfg t
    inputs  = listFunctionTypesWithInput  cfg t
    tFnList = unlines
                 $ ["You can create them with these functions:"]
                ++ outputs
                ++ ["", "And use them with these functions:"]
                ++ inputs

-- TODO move somewhere better
listFunctionTypesWithInput :: OrthoLangConfig -> OrthoLangType -> [String]
listFunctionTypesWithInput cfg cType = filter matches descs
  where
    -- TODO match more carefully because it should have to be an entire word
    matches d = (extOf cType) `elem` (words $ headOrDie "listFuncionTypesWithInput failed" $ splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ fTypeDesc f) (listFunctions cfg)

-- TODO move somewhere better
listFunctionTypesWithOutput :: OrthoLangConfig -> OrthoLangType -> [String]
listFunctionTypesWithOutput cfg cType = filter matches descs
  where
    matches d = (extOf cType) `elem` (words $ unwords $ tail $ splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ fTypeDesc f) (listFunctions cfg)

-- TODO this is totally duplicating code from putAssign; factor out
cmdLoad :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdLoad st@(_, cfg, ref, ids) hdl path = do
  clear
  path' <- absolutize path
  dfe   <- doesFileExist path'
  if not dfe
    then hPutStrLn hdl ("no such file: " ++ path') >> return st
    else do
      let cfg' = cfg { cfgScript = Just path' } -- TODO why the False??
      new <- parseFile cfg' ref ids path'
      case new of
        Left  e -> hPutStrLn hdl (show e) >> return st
        -- TODO put this back? not sure if it makes repl better
        Right s -> cmdShow (s, cfg', ref, ids) hdl ""
        -- Right s -> return (s, cfg', ref, ids)

cmdReload :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdReload st@(_, cfg, _, _) hdl _ = case cfgScript cfg of
  Nothing -> cmdDrop st hdl ""
  Just s  -> cmdLoad st hdl s

cmdWrite :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdWrite st@(scr, cfg, locks, ids) hdl line = case words line of
  [path] -> do
    saveScript cfg scr path
    return (scr, cfg { cfgScript = Just path }, locks, ids)
  [var, path] -> case lookup (OrthoLangVar (ReplaceID Nothing) var) scr of
    Nothing -> hPutStrLn hdl ("Var '" ++ var ++ "' not found") >> return st
    Just e  -> saveScript cfg (depsOnly e scr) path >> return st
  _ -> hPutStrLn hdl ("invalid save command: '" ++ line ++ "'") >> return st

-- TODO where should this go?
depsOnly :: OrthoLangExpr -> OrthoLangScript -> OrthoLangScript
depsOnly expr scr = deps ++ [res]
  where
    deps = filter (\(v,_) -> (elem v $ depsOf expr)) scr
    res  = (OrthoLangVar (ReplaceID Nothing) "result", expr)

-- TODO where should this go?
saveScript :: OrthoLangConfig -> OrthoLangScript -> FilePath -> IO ()
saveScript cfg scr path = absolutize path >>= \p -> writeScript cfg scr p

-- TODO factor out the variable lookup stuff
-- TODO except, this should work with expressions too!
cmdNeededBy :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdNeededBy st@(scr, cfg, _, _) hdl var = do
  case lookup (OrthoLangVar (ReplaceID Nothing) var) scr of
    Nothing -> hPutStrLn hdl $ "Var '" ++ var ++ "' not found"
    -- Just e  -> prettyAssigns hdl (\(v,_) -> elem v $ (OrthoLangVar Nothing var):depsOf e) scr
    Just e  -> pPrintHdl cfg hdl $ filter (\(v,_) -> elem v $ (OrthoLangVar (ReplaceID Nothing) var):depsOf e) scr
  return st

-- TODO move to Pretty.hs
-- prettyAssigns :: Handle -> (OrthoLangAssign -> Bool) -> OrthoLangScript -> IO ()
-- prettyAssigns hdl fn scr = do
  -- txt <- renderIO $ pPrint $ filter fn scr
  -- hPutStrLn hdl txt

cmdNeeds :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdNeeds st@(scr, cfg, _, _) hdl var = do
  let var' = OrthoLangVar (ReplaceID Nothing) var
  case lookup var' scr of
    Nothing -> hPutStrLn hdl $ "Var '" ++ var ++ "' not found"
    Just _  -> pPrintHdl cfg hdl $ filter (\(v,_) -> elem v $ (OrthoLangVar (ReplaceID Nothing) var):rDepsOf scr var') scr
  return st

-- TODO factor out the variable lookup stuff
cmdDrop :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdDrop (_, cfg, ref, ids) _ [] = clear >> return ([], cfg { cfgScript = Nothing }, ref, ids) -- TODO drop ids too?
cmdDrop st@(scr, cfg, ref, ids) hdl var = do
  let v = OrthoLangVar (ReplaceID Nothing) var
  case lookup v scr of
    Nothing -> hPutStrLn hdl ("Var '" ++ var ++ "' not found") >> return st
    Just _  -> return (delFromAL scr v, cfg, ref, ids)

cmdType :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdType st@(scr, cfg, _, _) hdl s = hPutStrLn hdl typeInfo >> return st
  where
    typeInfo = case stripWhiteSpace s of
      "" -> allTypes
      s' -> oneType s'
    oneType e = case findFunction cfg e of
      Just f  -> fTypeDesc f
      Nothing -> showExprType st e -- TODO also show the expr itself?
    allTypes = init $ unlines $ map showAssignType scr

showExprType :: OrthoLangState -> String -> String
showExprType st e = case parseExpr st e of
  Right expr -> show $ typeOf expr
  Left  err  -> show err

showAssignType :: OrthoLangAssign -> String
showAssignType (OrthoLangVar _ v, e) = unwords [typedVar, "=", prettyExpr]
  where
    -- parentheses also work:
    -- typedVar = v ++ " (" ++ show (typeOf e) ++ ")"
    typedVar = v ++ "." ++ show (typeOf e)
    prettyExpr = render $ pPrint e

-- TODO factor out the variable lookup stuff
cmdShow :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdShow st@(s, c, _, _) hdl [] = mapM_ (pPrintHdl c hdl) s >> return st
cmdShow st@(scr, cfg, _, _) hdl var = do
  case lookup (OrthoLangVar (ReplaceID Nothing) var) scr of
    Nothing -> hPutStrLn hdl $ "Var '" ++ var ++ "' not found"
    Just e  -> pPrintHdl cfg hdl e
  return st

-- TODO does this one need to be a special case now?
cmdQuit :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdQuit _ _ _ = throw QuitRepl
-- cmdQuit _ _ _ = ioError $ userError "Bye for now!"

cmdBang :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdBang st _ cmd = (runCommand cmd >>= waitForProcess) >> return st

-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: OrthoLangState -> Handle -> String -> IO OrthoLangState
cmdConfig st@(scr, cfg, ref, ids) hdl s = do
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
                 return (scr, cfg', ref, ids)

--------------------
-- tab completion --
--------------------

-- complete things in quotes: filenames, seqids
quotedCompletions :: MonadIO m => OrthoLangState -> String -> m [Completion]
quotedCompletions (_, _, _, idRef) wordSoFar = do
  files  <- listFiles wordSoFar
  seqIDs <- fmap (map $ headOrDie "quotedCompletions failed" . words) $ fmap M.elems $ fmap (M.unions . M.elems . hSeqIDs) $ liftIO $ readIORef idRef
  let seqIDs' = map simpleCompletion $ filter (wordSoFar `isPrefixOf`) seqIDs
  return $ files ++ seqIDs'

-- complete everything else: fn names, var names, :commands, types
-- these can be filenames too, but only if the line starts with a :command
nakedCompletions :: MonadIO m => OrthoLangState -> String -> String -> m [Completion]
nakedCompletions (scr, cfg, _, _) lineReveresed wordSoFar = do
  files <- if ":" `isSuffixOf` lineReveresed then listFiles wordSoFar else return []
  return $ files ++ (map simpleCompletion $ filter (wordSoFar `isPrefixOf`) wordSoFarList)
  where
    wordSoFarList = fnNames ++ varNames ++ cmdNames ++ typeExts
    fnNames  = concatMap (map (head . fNames) . mFunctions) (cfgModules cfg)
    varNames = map ((\(OrthoLangVar _ v) -> v) . fst) scr
    cmdNames = map ((':':) . fst) (cmds cfg)
    typeExts = map extOf $ concatMap mTypes $ cfgModules cfg

-- this is mostly lifted from Haskeline's completeFile
myComplete :: MonadIO m => OrthoLangState -> CompletionFunc m
myComplete s = completeQuotedWord   (Just '\\') "\"'" (quotedCompletions s)
             $ completeWordWithPrev (Just '\\') ("\"\'" ++ filenameWordBreakChars)
                                    (nakedCompletions s)

-- This is separate from the OrthoLangConfig because it shouldn't need changing.
-- TODO do we actually need the script here? only if we're recreating it every loop i guess
replSettings :: OrthoLangState -> Settings IO
replSettings s@(_, cfg, _, _) = Settings
  { complete       = myComplete s
  , historyFile    = Just $ cfgTmpDir cfg </> "history.txt"
  , autoAddHistory = True
  }
