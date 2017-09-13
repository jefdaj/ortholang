-- TODO why export all these?
-- TODO make sure expressions consume the whole string
--      for example right now "\"this\" 2" parses as a str

module ShortCut.Core.Parse
  -- parsec stuff
  ( ParseError
  , parseWithEof
  , parseWithLeftOver
  , parseAndShow
  -- functions used elsewhere in core
  , isExpr
  , parseExpr
  , parseStatement
  , parseFile
  , parseFileIO
  -- functiosn only used for testing
  , escapeChars
  -- , fnNames -- TODO load these from modules
  , literalChars
  , pComment
  , pNum
  , pBop
  , pTerm
  , pExpr
  , pStatement
  , pFun
  , pQuoted
  , pSym
  , pSet
  , pVar
  , pVarEq
  , spaceChars
  )
  where

-- TODO stop accidentally interpreting args in the wrong order as one big variable
-- TODO fix bug where a non-function with args parses to varname with args dropped
--       (example: 'this = load_that cool')

import ShortCut.Core.Types
import ShortCut.Core.Parse.Basic
import ShortCut.Core.Parse.Expr
import ShortCut.Core.Parse.Script

import Control.Monad          (fail)
import Data.Either            (isRight)
import Text.Parsec            (ParseError)
import Text.Parsec.Combinator (manyTill, eof, anyToken)

--------------------------
-- functions for export --
--------------------------

isExpr :: CutState -> String -> Bool
isExpr state line = isRight $ runParseM pExpr state line

-- TODO make this return the "result" assignment directly?
parseExpr :: CutState -> String -> Either ParseError CutExpr
parseExpr = runParseM pExpr

--------------------------------
-- helpers to simplify parsec --
--------------------------------

-- TODO make an empty CutState so you can run these in ghci again

-- Some are from the Parsec tutorial here:
-- https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing

parseWithEof :: ParseM a -> CutState -> String -> Either ParseError a
parseWithEof p s = runParseM (p <* eof) s

parseAndShow :: (Show a) => ParseM a -> CutState -> String -> String
parseAndShow p s str' = case runParseM p s str' of
  Left err -> show err
  Right s2 -> show s2

parseWithLeftOver :: ParseM a -> CutState -> String -> Either ParseError (a,String)
parseWithLeftOver p s = runParseM ((,) <$> p <*> leftOver) s
  where
    leftOver = manyTill anyToken eof
