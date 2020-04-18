-- TODO no welcome if going to load a file + clear the screen anyway
-- TODO could simplify to the same code everywhere except you pass the handle (file vs stdout)?

-- Based on:
-- http://dev.stephendiehl.com/hask/ (the Haskeline section)
-- https://github.com/goldfirere/glambda

-- TODO prompt to remove any bindings dependent on one the user is changing
-- TODO you should be able to write comments in the REPL

module OrthoLang.Core.Repl
  (

  -- * Used in main
    runRepl

  -- * Used in tests
  , mkRepl
  , ReplM
  , promptArrow

  -- * Implementation details
  , clear
  , cmdBang
  , cmdConfig
  , cmdDrop
  , cmdHelp
  , cmdLoad
  , cmdNeededBy
  , cmdNeeds
  , cmdQuit
  , cmdReload
  , cmdShow
  , cmdType
  , cmdWrite
  , cmds
  , depsOnly
  , dereference
  , loop
  , myComplete
  , nakedCompletions
  , prompt
  , quotedCompletions
  , removeSelfReferences
  , replSettings2
  , replaceVar
  , runCmd
  , runReplM
  , runStatement
  , saveScript
  , shortPrompt
  , showAssignType
  , showExprType
  , step
  , updateVars

  )
  where

import Prelude                  hiding (print)
import System.Console.Haskeline hiding (catch)

import qualified Data.Map.Strict as M

import OrthoLang.Core.Types
import OrthoLang.Core.Config (showConfigField, setConfigField)
import OrthoLang.Core.Eval   (evalScript)
import OrthoLang.Core.Help   (help, renderTypeSig)
import OrthoLang.Core.Parse  (isExpr, parseExpr, parseStatement, parseFile)
import OrthoLang.Core.Pretty (pPrint, render, pPrintHdl, writeScript)
import OrthoLang.Util        (absolutize, stripWhiteSpace, justOrDie, headOrDie)

import Control.Exception.Safe     (Typeable, throw)
import Control.Monad              (when)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.State.Strict (StateT, execStateT, lift, get, put)
import Data.Char                  (isSpace)
import Data.IORef                 (readIORef)
import Data.List                  (isPrefixOf, isSuffixOf, filter, delete)
import Data.List.Utils            (delFromAL)
import Development.Shake.FilePath (takeFileName)
import System.Console.ANSI        (clearScreen, cursorUp)
import System.Directory           (doesFileExist)
import System.FilePath.Posix      ((</>))
import System.IO                  (Handle, hPutStrLn, stdout)
import System.Process             (runCommand, waitForProcess)

-----------------
-- Repl monad --
----------------

type ReplM = StateT GlobalEnv IO

runReplM :: InputT ReplM (Maybe String) -> GlobalEnv -> IO ()
runReplM myLoop st@(_, cfg, _, _, _) = do
  _ <- execStateT (runInputT (replSettings2 cfg) myLoop) st
  return ()

prompt :: String -> InputT ReplM (Maybe String)
prompt = getInputLine

-------------------
-- main interface --
--------------------

clear :: IO ()
clear = clearScreen >> cursorUp 1000

welcome :: Handle -> IO ()
welcome hdl = hPutStrLn hdl
  "Welcome to the OrthoLang interpreter!\n\
  \Type :help for a list of the available commands."

runRepl :: GlobalEnv -> IO ()
runRepl = mkRepl (repeat prompt) stdout

-- Like runRepl, but allows overriding the prompt function for golden testing.
-- Used by mockRepl in OrthoLang/Core/Repl/Tests.hs
-- TODO separate script from rest of GlobalEnv
mkRepl :: [String -> InputT ReplM (Maybe String)] -> Handle -> GlobalEnv -> IO ()
mkRepl promptFns hdl (_, cfg, ref, ids, dRef) = do
  let st = (emptyScript, cfg, ref, ids, dRef)
  st' <- case cfgScript cfg of
          Nothing -> clear >> welcome hdl >> return st
          Just path -> cmdLoad st hdl path
  runReplM (loop promptFns hdl) st'

promptArrow :: String
promptArrow = " —▶ "

shortPrompt :: Config -> String
shortPrompt cfg = "\n" ++ name ++ promptArrow -- TODO no newline if last command didn't print anything
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
loop :: [String -> InputT ReplM (Maybe String)] -> Handle -> InputT ReplM (Maybe String)
loop [] _ = return Nothing
loop (promptFn:promptFns) hdl = do
  st@(_, cfg, _, _, _)  <- lift $ get
  mLine <- promptFn $ shortPrompt cfg
  st' <- liftIO $ step st hdl mLine -- TODO what kind of lift is appropriate here?
  lift $ put st'
  loop promptFns hdl

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
step :: GlobalEnv -> Handle -> Maybe String -> IO GlobalEnv
step st hdl mLine = case mLine of
  Nothing -> return st
  Just line -> case stripWhiteSpace line of
    ""        -> return st
    ('#':_  ) -> return st
    (':':cmd) -> runCmd st hdl cmd
    statement -> runStatement st hdl statement

-- TODO insert ids
runStatement :: GlobalEnv -> Handle -> String -> IO GlobalEnv
runStatement st@(scr, cfg, ref, ids, dRef) hdl line = case parseStatement (cfg, scr) line of
  Left  e -> hPutStrLn hdl e >> return st
  Right r -> do
    let st' = (updateVars scr r, cfg, ref, ids, dRef)
    when (isExpr (cfg, scr) line) (evalScript hdl st')
    return st'

-- this is needed to avoid assigning a variable literally to itself,
-- which is especially a problem when auto-assigning "result"
-- TODO is this where we can easily require the replacement var's type to match if it has deps?
-- TODO what happens if you try that in a script? it should fail i guess?
updateVars :: Script -> Assign -> Script
updateVars scr asn@(var, _) = as'
  where
    res = Var (RepID Nothing) "result"
    asn' = removeSelfReferences scr asn
    as' = if var /= res && var `elem` map fst scr
            then replaceVar asn' scr
            else delFromAL scr var ++ [asn']

-- replace an existing var in a script
replaceVar :: Assign -> [Assign] -> [Assign]
replaceVar a1@(v1, _) = map $ \a2@(v2, _) -> if v1 == v2 then a1 else a2

-- makes it ok to assign a var to itself in the repl
-- by replacing the reference with its value at that point
-- TODO forbid this in scripts though
removeSelfReferences :: Script -> Assign -> Assign
removeSelfReferences s a@(v, e) = if not (v `elem` depsOf e) then a else (v, dereference s v e)

-- does the actual work of removing self-references
dereference :: Script -> Var -> Expr -> Expr
dereference scr var e@(Ref _ _ _ v2)
  | var == v2 = justOrDie "failed to dereference variable!" $ lookup var scr
  | otherwise = e
dereference _   _   e@(Lit _ _) = e
dereference _   _   (Com _) = error "implement this! or rethink?"
dereference scr var (Bop  t ms vs s e1 e2) = Bop  t ms (delete var vs) s (dereference scr var e1) (dereference scr var e2)
dereference scr var (Fun  t ms vs s es   ) = Fun  t ms (delete var vs) s (map (dereference scr var) es)
dereference scr var (Lst t vs   es   ) = Lst t (delete var vs)   (map (dereference scr var) es)

--------------------------
-- dispatch to commands --
--------------------------

-- TODO can this use tab completion?
runCmd :: GlobalEnv -> Handle -> String -> IO GlobalEnv
runCmd st@(_, cfg, _, _, _) hdl line = case matches of
  [(_, fn)] -> fn st hdl $ stripWhiteSpace args
  []        -> hPutStrLn hdl ("unknown command: "   ++ cmd) >> return st
  _         -> hPutStrLn hdl ("ambiguous command: " ++ cmd) >> return st
  where
    (cmd, args) = break isSpace line
    matches = filter ((isPrefixOf cmd) . fst) (cmds cfg)

cmds :: Config -> [(String, GlobalEnv -> Handle -> String -> IO GlobalEnv)]
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
-- TODO bop help by mapping to the prefixOf version
cmdHelp :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdHelp st@(_, cfg, _, _, _) hdl line = do
  doc <- help cfg line
  hPutStrLn hdl doc >> return st

-- TODO this is totally duplicating code from putAssign; factor out
-- TODO should it be an error for the new script not to play well with an existing one?
cmdLoad :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdLoad st@(scr, cfg, ref, ids, dRef) hdl path = do
  clear
  path' <- absolutize path
  dfe   <- doesFileExist path'
  if not dfe
    then hPutStrLn hdl ("no such file: " ++ path') >> return st
    else do
      let cfg' = cfg { cfgScript = Just path' } -- TODO why the False??
      new <- parseFile (scr, cfg', ref, ids, dRef) path' -- TODO insert ids
      case new of
        Left  e -> hPutStrLn hdl (show e) >> return st
        -- TODO put this back? not sure if it makes repl better
        Right s -> cmdShow (s, cfg', ref, ids, dRef) hdl ""
        -- Right s -> return (s, cfg', ref, ids, dRef)

cmdReload :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdReload st@(_, cfg, _, _, _) hdl _ = case cfgScript cfg of
  Nothing -> cmdDrop st hdl ""
  Just s  -> cmdLoad st hdl s

cmdWrite :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdWrite st@(scr, cfg, locks, ids, dRef) hdl line = case words line of
  [path] -> do
    saveScript cfg scr path
    return (scr, cfg { cfgScript = Just path }, locks, ids, dRef)
  [var, path] -> case lookup (Var (RepID Nothing) var) scr of
    Nothing -> hPutStrLn hdl ("Var \"" ++ var ++ "' not found") >> return st
    Just e  -> saveScript cfg (depsOnly e scr) path >> return st
  _ -> hPutStrLn hdl ("invalid save command: \"" ++ line ++ "\"") >> return st

-- TODO where should this go?
depsOnly :: Expr -> Script -> Script
depsOnly expr scr = deps ++ [res]
  where
    deps = filter (\(v,_) -> (elem v $ depsOf expr)) scr
    res  = (Var (RepID Nothing) "result", expr)

-- TODO where should this go?
saveScript :: Config -> Script -> FilePath -> IO ()
saveScript cfg scr path = absolutize path >>= \p -> writeScript cfg scr p

-- TODO factor out the variable lookup stuff
-- TODO except, this should work with expressions too!
cmdNeededBy :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdNeededBy st@(scr, cfg, _, _, _) hdl var = do
  case lookup (Var (RepID Nothing) var) scr of
    Nothing -> hPutStrLn hdl $ "Var \"" ++ var ++ "' not found"
    -- Just e  -> prettyAssigns hdl (\(v,_) -> elem v $ (Var Nothing var):depsOf e) scr
    Just e  -> pPrintHdl cfg hdl $ filter (\(v,_) -> elem v $ (Var (RepID Nothing) var):depsOf e) scr
  return st

-- TODO move to Pretty.hs
-- prettyAssigns :: Handle -> (Assign -> Bool) -> Script -> IO ()
-- prettyAssigns hdl fn scr = do
  -- txt <- renderIO $ pPrint $ filter fn scr
  -- hPutStrLn hdl txt

cmdNeeds :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdNeeds st@(scr, cfg, _, _, _) hdl var = do
  let var' = Var (RepID Nothing) var
  case lookup var' scr of
    Nothing -> hPutStrLn hdl $ "Var \"" ++ var ++ "' not found"
    Just _  -> pPrintHdl cfg hdl $ filter (\(v,_) -> elem v $ (Var (RepID Nothing) var):rDepsOf scr var') scr
  return st

-- TODO factor out the variable lookup stuff
cmdDrop :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdDrop (_, cfg, ref, ids, dRef) _ [] = clear >> return (emptyScript, cfg { cfgScript = Nothing }, ref, ids, dRef) -- TODO drop ids too?
cmdDrop st@(scr, cfg, ref, ids, dRef) hdl var = do
  let v = Var (RepID Nothing) var
  case lookup v scr of
    Nothing -> hPutStrLn hdl ("Var \"" ++ var ++ "' not found") >> return st
    Just _  -> return (delFromAL scr v, cfg, ref, ids, dRef)

cmdType :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdType st@(scr, cfg, _, _, _) hdl s = hPutStrLn hdl typeInfo >> return st
  where
    typeInfo = case stripWhiteSpace s of
      "" -> allTypes
      s' -> oneType s'
    oneType e = case findFunction cfg e of
      Just f  -> renderTypeSig f
      Nothing -> showExprType st e -- TODO also show the expr itself?
    allTypes = init $ unlines $ map showAssignType scr

-- TODO insert id?
showExprType :: GlobalEnv -> String -> String
showExprType (s, c, _, _, _) e = case parseExpr (c, s) e of
  Right expr -> show $ typeOf expr
  Left  err  -> show err

showAssignType :: Assign -> String
showAssignType (Var _ v, e) = unwords [typedVar, "=", prettyExpr]
  where
    -- parentheses also work:
    -- typedVar = v ++ " (" ++ show (typeOf e) ++ ")"
    typedVar = v ++ "." ++ show (typeOf e)
    prettyExpr = render $ pPrint e

-- TODO factor out the variable lookup stuff
-- TODO show the whole script, since that only shows sAssigns now anyway?
cmdShow :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdShow st@(s, c, _, _, _) hdl [] = mapM_ (pPrintHdl c hdl) s >> return st
cmdShow st@(scr, cfg, _, _, _) hdl var = do
  case lookup (Var (RepID Nothing) var) scr of
    Nothing -> hPutStrLn hdl $ "Var \"" ++ var ++ "' not found"
    Just e  -> pPrintHdl cfg hdl e
  return st

-- TODO does this one need to be a special case now?
cmdQuit :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdQuit _ _ _ = throw QuitRepl
-- cmdQuit _ _ _ = ioError $ userError "Bye for now!"

cmdBang :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdBang st _ cmd = (runCommand cmd >>= waitForProcess) >> return st

-- TODO if no args, dump whole config by pretty-printing
-- TODO wow much staircase get rid of it
cmdConfig :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdConfig st@(scr, cfg, ref, ids, dRef) hdl s = do
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
                 return (scr, cfg', ref, ids, dRef)

--------------------
-- tab completion --
--------------------

-- complete things in quotes: filenames, seqids
quotedCompletions :: String -> ReplM [Completion]
quotedCompletions wordSoFar = do
  (_, _, _, idRef, _) <- get
  files  <- listFiles wordSoFar
  seqIDs <- fmap (map $ headOrDie "quotedCompletions failed" . words) $ fmap M.elems $ fmap (M.unions . M.elems . hSeqIDs) $ liftIO $ readIORef idRef
  let seqIDs' = map simpleCompletion $ filter (wordSoFar `isPrefixOf`) seqIDs
  return $ files ++ seqIDs'

-- complete everything else: fn names, var names, :commands, types
-- these can be filenames too, but only if the line starts with a :command
nakedCompletions :: String -> String -> ReplM [Completion]
nakedCompletions lineReveresed wordSoFar = do
  (scr, cfg, _, _, _) <- get
  let wordSoFarList = fnNames ++ varNames ++ cmdNames ++ typeExts
      fnNames  = concatMap (map fName . mFunctions) (cfgModules cfg)
      varNames = map ((\(Var _ v) -> v) . fst) scr
      cmdNames = map ((':':) . fst) (cmds cfg)
      typeExts = map tExtOf $ concatMap mTypes $ cfgModules cfg
  files <- if ":" `isSuffixOf` lineReveresed then listFiles wordSoFar else return []
  return $ files ++ (map simpleCompletion $ filter (wordSoFar `isPrefixOf`) wordSoFarList)

-- this is mostly lifted from Haskeline's completeFile
myComplete :: CompletionFunc ReplM
myComplete
  = completeQuotedWord   escChars quotes quotedCompletions
  $ completeWordWithPrev escChars (quotes ++ filenameWordBreakChars) nakedCompletions
  where
    escChars = Just '\\'
    quotes = "\"\""

replSettings2 :: Config -> Settings ReplM
replSettings2 cfg = Settings
  { complete       = myComplete
  , historyFile    = Just $ cfgTmpDir cfg </> "history.txt"
  , autoAddHistory = True
  }
