module OrthoLang.Interpreter.Repl.Messages
  (

  -- * Repl commands
    cmdHelp
  , cmdShow
  , cmdType

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
import System.Console.Haskeline
import qualified Data.Map.Strict as M

import OrthoLang.Interpreter.Parse     (parseExpr)
import OrthoLang.Interpreter.Pretty    (pPrint, render, pPrintHdl)
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
  \Type :help for a list of the available commands."

promptArrow :: String
promptArrow = " —▶ "

shortPrompt :: Config -> String
shortPrompt cfg = "\n" ++ name ++ promptArrow -- TODO no newline if last command didn't print anything
  where
    name = case cfgScript cfg of
      Nothing -> "ortholang"
      Just s  -> takeFileName s

-- TODO load this from a file?
-- TODO update to include :config getting + setting
-- TODO if possible, make this open in `less`?
-- TODO why does this one have a weird path before the :help text?
-- TODO bop help by mapping to the prefixOf version
cmdHelp :: GlobalEnv -> Handle -> String -> IO GlobalEnv
cmdHelp st@(_, cfg, _, _, _) hdl line = do
  doc <- help cfg line
  hPutStrLn hdl doc >> return st

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

--------------------
-- tab completion --
--------------------

-- this is mostly lifted from Haskeline's completeFile
-- TODO clean up cfg, cmds stuff
myComplete :: [(String, ReplCmd)] -> CompletionFunc ReplM
myComplete cmds
  = completeQuotedWord   escChars quotes quotedCompletions
  $ completeWordWithPrev escChars (quotes ++ filenameWordBreakChars)
  $ nakedCompletions cmds
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
nakedCompletions :: [(String, ReplCmd)] -> String -> String -> ReplM [Completion]
nakedCompletions cmds lineReveresed wordSoFar = do
  (scr, cfg, _, _, _) <- get
  let wordSoFarList = fnNames ++ varNames ++ cmdNames ++ typeExts
      fnNames  = concatMap (map fName . mFunctions) (cfgModules cfg)
      varNames = map ((\(Var _ v) -> v) . fst) scr
      cmdNames = map ((':':) . fst) cmds
      typeExts = map tExtOf $ concatMap mTypes $ cfgModules cfg
  files <- if ":" `isSuffixOf` lineReveresed then listFiles wordSoFar else return []
  return $ files ++ (map simpleCompletion $ filter (wordSoFar `isPrefixOf`) wordSoFarList)
