module OrthoLang.Interpreter.Repl.Edit
  (

  -- * Repl commands
    cmdDrop
  , cmdLoad
  , cmdReload
  , cmdWrite

  -- * Implementation details
  , clear
  , depsOnly
  , removeSelfReferences
  , replaceVar
  , rmRef
  -- , runStatement
  , saveScript
  , updateVars

  )
  where

import Prelude hiding (print)

import OrthoLang.Types
import OrthoLang.Interpreter.Repl.Info (cmdShow)
import OrthoLang.Interpreter.Parse         (parseFile)
import OrthoLang.Util               (absolutize, justOrDie)

import Data.List           (filter, delete)
import Data.List.Utils     (delFromAL)
import System.Console.ANSI (clearScreen, cursorUp)
import System.Directory    (doesFileExist)
import System.IO           (Handle, hPutStrLn)

import Test.Tasty.Golden (writeBinaryFile)

clear :: IO ()
clear = clearScreen >> cursorUp 1000

-- TODO this is totally duplicating code from putAssign; factor out
-- TODO should it be an error for the new script not to play well with an existing one?
cmdLoad :: [Module] -> GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdLoad mods st@(scr, cfg, ref, ids, dRef) hdl path = do
  clear
  path' <- absolutize path
  dfe   <- doesFileExist path'
  if not dfe
    then hPutStrLn hdl ("no such file: " ++ path') >> return st
    else do
      let cfg' = cfg { script = Just path' } -- TODO why the False??
      new <- parseFile mods (scr, cfg', ref, ids, dRef) path' -- TODO insert ids
      case new of
        Left  e -> hPutStrLn hdl (show e) >> return st
        Right s -> do
          clear
          let st' = (s, cfg', ref, ids, dRef)
          cmdShow mods st' hdl ""
          return st'

cmdReload :: [Module] -> GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdReload mods st@(_, cfg, _, _, _) hdl _ = case script cfg of
  Nothing -> cmdDrop mods st hdl ""
  Just s  -> cmdLoad mods st hdl s

cmdWrite :: [Module] -> GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdWrite mods st@(scr, cfg, locks, ids, dRef) hdl line = case words line of
  [path] -> do
    saveScript cfg scr path
    return (scr, cfg { script = Just path }, locks, ids, dRef)
  [var, path] -> case lookupVar (Var (RepID Nothing) var) (sAssigns scr) of
    Nothing -> hPutStrLn hdl ("Var \"" ++ var ++ "' not found") >> return st
    Just e  -> saveScript cfg (depsOnly e scr) path >> return st
  _ -> hPutStrLn hdl ("invalid save command: \"" ++ line ++ "\"") >> return st

-- TODO where should this go?
depsOnly :: Expr -> Script -> Script
depsOnly expr scr = scr {sAssigns = deps ++ [res]}
  where
    deps = filter (\a -> (elem (aVar a) $ depsOf expr)) (sAssigns scr)
    res  = Assign {aVar = Var (RepID Nothing) "result", aExpr = expr}

-- TODO move to a "files/io" module along with debug fns?
-- TODO use safe write here?
writeScript :: Config -> Script -> FilePath -> IO ()
writeScript cfg scr path = do
  txt <- renderIO cfg $ pPrint scr
  writeBinaryFile path $ txt ++ "\n"

-- TODO where should this go?
saveScript :: Config -> Script -> FilePath -> IO ()
saveScript cfg scr path = absolutize path >>= \p -> writeScript cfg scr p

-- TODO factor out the variable lookup stuff
-- TODO except, this should work with expressions too!
cmdNeededBy :: ReplCmd
cmdNeededBy _ st@(scr, cfg, _, _, _) hdl var = do
  case lookupVar (Var (RepID Nothing) var) (sAssigns scr) of
    Nothing -> hPutStrLn hdl $ "Var \"" ++ var ++ "' not found"
    Just e  -> pPrintHdl cfg hdl $ filter (\a -> elem (aVar a) $ (Var (RepID Nothing) var):depsOf e) (sAssigns scr)
  return st

cmdNeededFor :: ReplCmd
cmdNeededFor _ st@(scr, cfg, _, _, _) hdl var = do
  let var' = Var (RepID Nothing) var
  case lookupVar var' (sAssigns scr) of
    Nothing -> hPutStrLn hdl $ "Var \"" ++ var ++ "' not found"
    Just _  -> pPrintHdl cfg hdl $ filter (\a -> elem (aVar a) $ (Var (RepID Nothing) var):rDepsOf scr var') (sAssigns scr)
  return st

-- TODO factor out the variable lookup stuff
cmdDrop :: ReplCmd
cmdDrop _ (_, cfg, ref, ids, dRef) _ [] = clear >> return (emptyScript, cfg { script = Nothing }, ref, ids, dRef)
cmdDrop _ st@(scr, cfg, ref, ids, dRef) hdl var = do
  let v = Var (RepID Nothing) var
  case lookupVar v (sAssigns scr) of
    Nothing -> hPutStrLn hdl ("Var \"" ++ var ++ "' not found") >> return st
    Just _  -> return (scr {sAssigns = delVar (sAssigns scr) var}, cfg, ref, ids, dRef)

-- this is needed to avoid assigning a variable literally to itself,
-- which is especially a problem when auto-assigning "result"
-- TODO is this where we can easily require the replacement var's type to match if it has deps?
-- TODO what happens if you try that in a script? it should fail i guess?
updateVars :: Script -> Assign -> Script
updateVars scr asn@(Assign {aVar = Var _ vName}) = scr {sAssigns = as'}
  where
    res = Var (RepID Nothing) "result"
    asn' = removeSelfReferences scr asn
    as  = sAssigns scr
    as' = if aVar asn /= res && aVar asn `elem` map aVar as
            then replaceVar asn' as
            else delVar as vName ++ [asn']

-- replace an existing var in a script
replaceVar :: Assign -> [Assign] -> [Assign]
replaceVar a1 = map $ \a2 -> if aVar a1 == aVar a2 then a1 else a2

-- makes it ok to assign a var to itself in the repl
-- by replacing the reference with its value at that point
-- TODO forbid this in scripts though
removeSelfReferences :: Script -> Assign -> Assign
removeSelfReferences s a@(Assign {aVar=v, aExpr=e}) = if not (v `elem` depsOf e) then a else a {aExpr=rmRef s v e}

-- does the actual work of removing self-references
rmRef :: Script -> Var -> Expr -> Expr
rmRef scr var e@(Ref _ _ _ v2)
  | var == v2 = justOrDie "failed to rmRef variable!" $ lookupVar var (sAssigns scr)
  | otherwise = e
rmRef _   _   e@(Lit _ _) = e
rmRef scr var (Bop  t ms vs s e1 e2) = Bop t ms (delete var vs) s (rmRef scr var e1) (rmRef scr var e2)
rmRef scr var (Fun  t ms vs s es   ) = Fun t ms (delete var vs) s (map (rmRef scr var) es)
rmRef scr var (Lst t vs       es   ) = Lst t    (delete var vs)   (map (rmRef scr var) es)
rmRef _   _   (Com _) = error "implement this! or rethink?"
