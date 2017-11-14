module ShortCut.Core.Parse.Script where

import ShortCut.Core.Types
import ShortCut.Core.Parse.Basic
import ShortCut.Core.Parse.Expr

import Control.Applicative    ((<|>), many)
import Control.Monad          (void)
import ShortCut.Core.Debug    (debugParser)
import Text.Parsec            (try, getState, putState)
import Text.Parsec.Char       (spaces, newline)
import Text.Parsec.Combinator (optional, lookAhead)
import Text.Parsec            (ParseError)
import ShortCut.Core.Debug    (debug)

{- New overall script parse idea:
 -
 - 0. take an initial script, because may be parsing an import
 - 1. strip comments
 - 2. recursively parse imported scripts
 - 3. pass the rest of the string to the existing parser
 -    (which can be simplified by removing comment code)
 -}

-- untested, but should strip all comments from a script leaving whitespace
stripComments :: String -> String
stripComments = unlines . map stripComment . lines
  where
    stripComment = takeWhile (/= '#')

----------------
-- statements --
----------------

-- TODO combine pVar and pVarEq somehow to reduce try issues?

-- TODO message in case it doesn't parse?
pAssign :: ParseM CutAssign
pAssign = do
  (scr, cfg) <- getState
  optional newline
  void $ lookAhead $ try pVarEq
  v <- pVarEq -- TODO use lookAhead here to decide whether to commit to it
  e <- lexeme pExpr
  putState (scr ++ [(v,e)], cfg)
  let res  = (v,e)
      res' = debugParser cfg "pAssign" res
  return res'

-- Handles the special case of a naked top-level expression, which is treated
-- as being assigned to "result". This parses the same in a script or the repl,
-- but doing it more than once in a script will cause an error later.
-- TODO prevent assignments that include the variable being assigned to
--      (later when working on statement issues)
-- TODO if the statement is literally `result`, what do we do?
--      maybe we need a separate type of assignment statement for this?
pResult :: ParseM CutAssign
pResult = do
  (_, cfg) <- getState
  e <- pExpr
  let e' = debugParser cfg "pResult" e
  return (CutVar "result", e')

pStatement :: ParseM CutAssign
pStatement = pAssign <|> pResult
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
pScript = do
  (_, cfg) <- getState
  optional spaces
  scr <- many pStatement
  putState (scr, cfg)
  return scr

-- TODO need CutState here? or just CutConfig?
parseStatement :: CutState -> String -> Either ParseError CutAssign
parseStatement = runParseM pStatement

-- The name doesn't do a good job of explaining this, but it's expected to be
-- parsing an entire script from a string (no previous state).
-- TODO clarify that
parseString :: CutConfig -> String -> Either ParseError CutScript
parseString c = runParseM pScript ([], c)

-- TODO could generalize to other parsers/checkers like above for testing
-- TODO is it OK that all the others take an initial script but not this?
-- TODO should we really care what the current script is when loading a new one?
parseFile :: CutConfig -> FilePath -> IO (Either ParseError CutScript)
parseFile cfg path = readFile path' >>= return . parseString cfg . stripComments
  where
    path' = debug cfg ("parseFile '" ++ path ++ "'") path

-- TODO move to a separate "files/io" module along with some debug fns?
parseFileIO :: CutConfig -> FilePath -> IO CutScript
parseFileIO cfg scr = do
  mscr1 <- parseFile cfg scr
  case mscr1 of
    Left  e -> fail $ show e
    Right s -> return s
