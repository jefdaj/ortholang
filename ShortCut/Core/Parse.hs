-- TODO why export all these?
-- TODO make sure expressions consume the whole string
--      for example right now "\"this\" 2" parses as a str

module ShortCut.Core.Parse
  -- parsec stuff
  ( ParseError
  , parseWithEof
  , parseWithLeftOver
  , parseAndShow -- TODO remove for production?
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

import ShortCut.Core.Parse.Util
import ShortCut.Core.Parse.Basic
import ShortCut.Core.Parse.Expr
import ShortCut.Core.Parse.Script

import Text.Parsec (ParseError)
