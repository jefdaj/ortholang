module OrthoLang.Interpreter.Repl.Info
  (

  -- * Repl commands
    cmdHelp
  , cmdShow
  , cmdType
  , cmdNeededBy
  , cmdNeededFor

  -- * Implementation details
  , myComplete
  , nakedCompletions
  , promptArrow
  , quotedCompletions
  , shortPrompt
  , showAssignType
  , showExprType
  , welcome

  )
  where

import Prelude hiding (print)
import OrthoLang.Types
import OrthoLang.Interpreter.Config (configFields) -- TODO move back to Interpreter
import System.Console.Haskeline
import qualified Data.Map.Strict as M

import OrthoLang.Interpreter.Parse     (parseExpr)
-- import OrthoLang.Interpreter.Pretty    (pPrint, render, pPrintHdl)
import OrthoLang.Interpreter.Repl.Help (help, renderTypeSig)
import OrthoLang.Util           (stripWhiteSpace, headOrDie)

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.State.Strict (get)
import Data.IORef                 (readIORef)
import Data.List                  (isPrefixOf, isSuffixOf, filter)
import System.FilePath            (takeFileName)
import System.IO                  (Handle, hPutStrLn)

welcome :: Handle -> IO ()
welcome hdl = hPutStrLn hdl
  "Welcome to the OrthoLang interpreter!\n\
  \Type :help for a list of the available commands.\n"

promptArrow :: String
promptArrow = " —▶ "

shortPrompt :: Config -> String
shortPrompt cfg = name ++ promptArrow
  where
    name = case script cfg of
      Nothing -> "ortholang"
      Just s  -> takeFileName s

-- TODO load this from a file?
-- TODO update to include :config getting + setting
-- TODO if possible, make this open in `less`?
-- TODO why does this one have a weird path before the :help text?
-- TODO bop help by mapping to the prefixOf version
cmdHelp :: ReplInfo
cmdHelp ms st@(_, cfg, _, _, _) hdl line = do
  doc <- help cfg ms line
  hPutStrLn hdl doc

cmdType :: ReplCmd
cmdType mods st@(Script as, _, _, _, _) hdl s = hPutStrLn hdl typeInfo >> return st
  where
    typeInfo = case stripWhiteSpace s of
      "" -> allTypes
      s' -> oneType s'
    oneType e = case findFunction mods e of
      Just f  -> renderTypeSig f
      Nothing -> showExprType mods st e -- TODO also show the expr itself?
    allTypes = stripWhiteSpace $ unlines $ map showAssignType as

-- TODO insert id?
showExprType :: [Module] -> GlobalEnv -> String -> String
showExprType ms (s, c, _, _, _) e = case parseExpr ms c s e of
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
cmdShow :: ReplInfo
cmdShow ms st@(_, c, _, _, _) hdl s | showvartypes c = cmdType ms st hdl s
cmdShow _ st@(Script as, c, _, _, _) hdl [] = mapM_ (pPrintHdl c hdl) as >> return st
cmdShow _ st@(Script as, cfg, _, _, _) hdl var = do
  case lookup (Var (RepID Nothing) var) as of
    Nothing -> hPutStrLn hdl $ "Var \"" ++ var ++ "' not found"
    Just e  -> pPrintHdl cfg hdl e -- >> hPutStrLn hdl ""

-- TODO factor out the variable lookup stuff
-- TODO except, this should work with expressions too!
cmdNeededBy :: ReplInfo
cmdNeededBy _ st@(scr, cfg, _, _, _) hdl var = do
  case lookup (Var (RepID Nothing) var) scr of
    Nothing -> hPutStrLn hdl $ "Var \"" ++ var ++ "' not found"
    Just e  -> pPrintHdl cfg hdl $ filter (\(v,_) -> elem v $ (Var (RepID Nothing) var):depsOf e) scr

cmdNeededFor :: ReplInfo
cmdNeededFor _ st@(scr, cfg, _, _, _) hdl var = do
  let var' = Var (RepID Nothing) var
  case lookup var' scr of
    Nothing -> hPutStrLn hdl $ "Var \"" ++ var ++ "' not found"
    Just e  -> pPrintHdl cfg hdl $ filter (\(v,_) -> elem v $ (Var (RepID Nothing) var):depsOf e) scr


--------------------
-- tab completion --
--------------------

-- this is mostly lifted from Haskeline's completeFile
-- TODO clean up cfg, cmds stuff
myComplete :: [Module] -> [String] -> CompletionFunc ReplM
myComplete mods cmdNames
  = completeQuotedWord   escChars quotes quotedCompletions
  $ completeWordWithPrev escChars (quotes ++ filenameWordBreakChars)
  $ nakedCompletions mods cmdNames
  where
    escChars = Just '\\'
    quotes = "\"\""

-- complete things in quotes: filenames, seqids
quotedCompletions :: String -> ReplM [Completion]
quotedCompletions wordSoFar = do
  (_, _, _, idRef, _) <- get
  files  <- listFiles wordSoFar
  seqIDs <- fmap (map $ headOrDie "quotedCompletions failed" . words) $
            fmap M.elems $
            fmap (M.unions . M.elems . hSeqIDs) $
            liftIO $ readIORef idRef
  let seqIDs' = map simpleCompletion $ filter (wordSoFar `isPrefixOf`) seqIDs
  return $ files ++ seqIDs'

-- complete everything else: fn names, var names, :commands, types
-- these can be filenames too, but only if the line starts with a :command
-- nakedCompletions :: String -> String -> ReplM [Completion]
nakedCompletions :: [Module] -> [(String, ReplCmd)] -> String -> String -> ReplM [Completion]
nakedCompletions mods cmds lineReveresed wordSoFar = do
  (Script as, _, _, _, _) <- get
  let wordSoFarList = fnNames ++ varNames ++ cmdNames ++ typeExts ++ cfgFields
      fnNames  = concatMap (map fName . mFunctions) mods
      varNames = map ((\(Var _ v) -> v) . fst) as
      cmdNames = map ((':':) . fst) cmds
      typeExts = map ext $ concatMap mTypes mods
      cfgFields = map fst configFields
  files <- if ":" `isSuffixOf` lineReveresed then listFiles wordSoFar else return []
  return $ files ++ (map simpleCompletion $ filter (wordSoFar `isPrefixOf`) wordSoFarList)
