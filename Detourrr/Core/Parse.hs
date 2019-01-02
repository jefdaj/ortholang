-- TODO why export all these?
-- TODO make sure expressions consume the whole string
--      for example right now "\"this\" 2" parses as a str

module Detourrr.Core.Parse
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
  , pNum
  , pBop
  , pTerm
  , pExpr
  , pStatement
  , pFun
  , pFunName
  , pQuoted
  , pSym
  , pList
  , pVar
  , pVarEq
  , spaceChars
  )
  where

-- TODO stop accidentally interpreting args in the wrong order as one big variable
-- TODO fix bug where a non-function with args parses to varname with args dropped
--       (example: 'this = load_that cool')

import Detourrr.Core.Types
import Detourrr.Core.Parse.Basic
import Detourrr.Core.Parse.Expr
import Detourrr.Core.Parse.Script

-- import Control.Monad          (fail)
import Data.Either            (isRight)
import Text.Parsec            (ParseError)
import Text.Parsec.Combinator (manyTill, eof, anyToken)

--------------------------
-- functions for export --
--------------------------

isExpr :: DtrState -> String -> Bool
isExpr state line = isRight $ runParseM pExpr state line

-- TODO make this return the "result" assignment directly?
parseExpr :: DtrState -> String -> Either ParseError DtrExpr
parseExpr = runParseM pExpr

--------------------------------
-- helpers to simplify parsec --
--------------------------------

-- TODO make an empty DtrState so you can run these in ghci again

-- Some are from the Parsec tutorial here:
-- https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing

parseWithEof :: ParseM a -> DtrState -> String -> Either ParseError a
parseWithEof p s = runParseM (p <* eof) s

parseAndShow :: (Show a) => ParseM a -> DtrState -> String -> String
parseAndShow p s str' = case runParseM p s str' of
  Left err -> show err
  Right s2 -> show s2

parseWithLeftOver :: ParseM a -> DtrState -> String -> Either ParseError (a,String)
parseWithLeftOver p s = runParseM ((,) <$> p <*> leftOver) s
  where
    leftOver = manyTill anyToken eof
