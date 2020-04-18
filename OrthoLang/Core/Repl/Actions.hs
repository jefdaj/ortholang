module OrthoLang.Core.Repl.Actions
  where

import Prelude                  hiding (print)

import qualified Data.Map.Strict as M

import OrthoLang.Core.Repl.Help   (help, renderTypeSig)

import OrthoLang.Core.Types
import OrthoLang.Core.Config (showConfigField, setConfigField)
import OrthoLang.Core.Eval   (evalScript)
import OrthoLang.Core.Parse  (isExpr, parseExpr, parseStatement, parseFile)
import OrthoLang.Core.Pretty (pPrint, render, pPrintHdl, writeScript)
import OrthoLang.Util        (absolutize, stripWhiteSpace, justOrDie, headOrDie)

import Control.Exception.Safe     (Exception, Typeable, throw)
import Control.Monad              (when)
import Control.Monad.IO.Class     (liftIO)
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
import System.Console.Haskeline hiding (catch)
import Control.Monad.State.Strict (StateT, execStateT, lift, get, put)

clear :: IO ()
clear = clearScreen >> cursorUp 1000

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
        Right s -> return st -- cmdShow (s, cfg', ref, ids, dRef) hdl ""
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
