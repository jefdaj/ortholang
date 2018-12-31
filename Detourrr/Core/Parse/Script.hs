module Detourrr.Core.Parse.Script where

import Detourrr.Core.Util (readFileStrict)
import Detourrr.Core.Types
import Detourrr.Core.Config (debug)
import Detourrr.Core.Parse.Basic
import Detourrr.Core.Parse.Expr

import Control.Applicative    ((<|>), many)
-- import Control.Monad          (void)
-- import Detourrr.Core.Debug    (debugParser)
import Text.Parsec            (try, getState, putState)
import Text.Parsec.Char       (spaces, newline)
import Text.Parsec.Combinator (optional)
import Text.Parsec            (ParseError)
-- import Detourrr.Core.Debug    (debug)
-- import Data.IORef             (IORef)
import System.FilePath ((</>), takeDirectory)
-- import Data.Map               (empty)

-------------------
-- preprocessing --
-------------------

{- New overall script parse idea:
 -
 - 0. take an initial script, because may be parsing an import
 - 1. strip comments
 - 2. recursively parse imported scripts
 - 3. pass the rest of the string to the existing parser
 -    (which can be simplified by removing comment code)
 -}

-- untested, but should strip all comments from a script leaving whitespace
-- TODO could this be lobbing off the first char?
stripComments :: String -> String
stripComments = unlines . map stripComment . lines
  where
    stripComment = takeWhile (/= '#')

-- TODO make this parser nicer like the others? or leave simple like other languages?
readScriptWithIncludes :: Locks -> FilePath -> IO String
readScriptWithIncludes ref path = do
  txt <- readFileStrict ref path
  fmap unlines $ mapM processInclude $ lines txt
  where
    processInclude :: String -> IO String
    processInclude line = case words (stripComments line) of
                           ("include":relpath:_) ->
                             readScriptWithIncludes ref $ takeDirectory path </> (stripQuotes relpath)
                           _ -> return line

stripQuotes :: String -> String
stripQuotes s = dropWhile (== '\"') $ reverse $ dropWhile (== '\"') $ reverse s

----------------
-- statements --
----------------

-- TODO combine pVar and pVarEq somehow to reduce try issues?

-- TODO message in case it doesn't parse?
pAssign :: ParseM CutAssign
pAssign = debugParser "pAssign" $ do
  (scr, cfg, ref, ids) <- getState
  -- optional newline
  -- void $ lookAhead $ debugParser "first pVarEq" pVarEq
  v <- debugParseM "second pVarEq" >> (try (optional newline *> pVarEq)) -- TODO use lookAhead here to decide whether to commit to it
  e <- debugParseM "first pExpr" >> (lexeme pExpr)
  putState (scr ++ [(v,e)], cfg, ref, ids)
  debugParseM $ "assigned var " ++ show v
  let res  = (v,e)
      -- res' = debugParser cfg "pAssign" res
  return res

-- Handles the special case of a naked top-level expression, which is treated
-- as being assigned to "result". This parses the same in a script or the repl,
-- but doing it more than once in a script will cause an error later.
-- TODO prevent assignments that include the variable being assigned to
--      (later when working on statement issues)
-- TODO if the statement is literally `result`, what do we do?
--      maybe we need a separate type of assignment statement for this?
pResult :: ParseM CutAssign
pResult = debugParser "pResult" $ do
  -- (_, cfg, _) <- getState
  e <- pExpr
  -- let e' = debugParser cfg "pResult" e
  return (CutVar "result", e)

pStatement :: ParseM CutAssign
pStatement = debugParser "pStatement" (try pAssign <|> pResult)
  -- (_, cfg) <- getState
  -- res <- pAssign <|> pResult
  -- let res' = debugParser cfg "pStatement" res
  -- return res

-------------
-- scripts --
-------------

-- TODO add a preprocessing step that strips comments + recurses on imports?

-- TODO message in case it doesn't parse?
-- TODO should it get automatically `put` here, or manually in the repl?
pScript :: ParseM CutScript
pScript = debugParser "pScript" $ do
  (_, cfg, ref, ids) <- getState
  optional spaces
  scr <- many pStatement
  putState (scr, cfg, ref, ids)
  return scr

-- TODO need CutState here? or just CutConfig?
parseStatement :: CutState -> String -> Either ParseError CutAssign
parseStatement = runParseM pStatement

-- The name doesn't do a good job of explaining this, but it's expected to be
-- parsing an entire script from a string (no previous state).
-- TODO clarify that
parseString :: CutConfig -> Locks -> HashedSeqIDsRef -> String
            -> Either ParseError CutScript
parseString c r ids = runParseM pScript ([], c, r, ids)

-- TODO could generalize to other parsers/checkers like above for testing
-- TODO is it OK that all the others take an initial script but not this?
-- TODO should we really care what the current script is when loading a new one?
parseFile :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath
          -> IO (Either ParseError CutScript)
parseFile cfg ref ids path = do
  txt <- readScriptWithIncludes ref path'
  return $ (parseString cfg ref ids . stripComments) txt
  where
    path' = debug cfg ("parseFile '" ++ path ++ "'") path

-- TODO move to a separate "files/io" module along with some debug fns?
parseFileIO :: CutConfig -> Locks -> HashedSeqIDsRef -> FilePath -> IO CutScript
parseFileIO cfg ref ids scr = do
  mscr1 <- parseFile cfg ref ids scr
  case mscr1 of
    Left  e -> fail $ show e
    Right s -> return s
