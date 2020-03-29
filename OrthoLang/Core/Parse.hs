-- TODO why export all these?
-- TODO make sure expressions consume the whole string
--      for example right now "\"this\" 2" parses as a str

-- TODO stop accidentally interpreting args in the wrong order as one big variable
-- TODO fix bug where a non-function with args parses to varname with args dropped
--       (example: 'this = load_that cool')

module OrthoLang.Core.Parse
  (
  -- * Parsec stuff
    ParseError
  , parseWithEof
  , parseWithLeftOver
  , parseAndShow -- TODO remove for production?

  -- * Functions used elsewhere in core
  , isExpr
  , parseExpr
  , parseStatement
  , parseFile
  , parseFileIO

  -- * Functions only used for testing
  , escapeChars
  , literalChars
  -- , fnNames -- TODO load these from modules

  -- * Parsers
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

import OrthoLang.Core.Parse.Basic
import OrthoLang.Core.Parse.Expr
import OrthoLang.Core.Parse.Script
import OrthoLang.Core.Parse.Util

import Text.Parsec (ParseError)
