-- This module does the initial parsing of text into an abstract syntax tree.
-- That tree will then be checked for errors (Check.hs) and compiled into Shake
-- build rules (Shake.hs).

-- TODO: stop accidentally interpreting args in the wrong order as one big variable
-- TODO: start adding nice error messages to each parser
-- TODO: fix bug where a non-function with args parses to varname with args dropped
--       (example: 'this = load_that cool')

module ShortCut.Interpret.Parse where

import ShortCut.Types

import Control.Applicative    ((<|>), many)
import Control.Monad          (void)
import Control.Monad.Identity (Identity)
import Data.Char              (isPrint)
import Text.Parsec            (parse, try)
import Text.Parsec.Char       (char, digit ,letter, spaces, anyChar
                              ,newline, string, oneOf)
import Text.Parsec.Combinator (optional, many1, manyTill, eof
                              ,lookAhead, between, choice, anyToken)
import Text.Parsec.Expr       (buildExpressionParser, Assoc(..)
                              ,Operator(..))
import Text.Parsec.Prim       (Parsec)

------------------------------------------
-- aliases + helpers to simplify parsec --
------------------------------------------

-- Some are from the Parsec tutorial here:
-- https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing

type Parser  = Parsec String ()
type OpTable = [[Operator String () Identity ParsedExpr]]

parseWithError :: Parser a -> String -> Either ShortCutError a
parseWithError psr str = case parse psr "" str of
  Left  err -> Left $ InvalidSyntax err
  Right res -> Right res

regularParse :: Parser a -> String -> Either ShortCutError a
regularParse p = parseWithError p

parseWithEof :: Parser a -> String -> Either ShortCutError a
parseWithEof p = parseWithError (p <* eof)

parseWithLeftOver :: Parser a -> String -> Either ShortCutError (a,String)
parseWithLeftOver p = parseWithError ((,) <$> p <*> leftOver)
  where
    leftOver = manyTill anyToken eof

parseWithWSEof :: Parser a -> String -> Either ShortCutError a
parseWithWSEof p = parseWithEof (whiteSpace *> p)
  where
    whiteSpace = void $ many $ oneOf " \n\t"

parseAndShow :: (Show a) => Parser a -> String -> String
parseAndShow p s = case regularParse p s of
  Left err -> show err
  Right s2 -> show s2

-- There's a convention in parsers that each one should consume whitespace
-- after itself (handled by this function), and you only skip leading
-- whitespace at the top level. That way all the whitespace gets skipped
-- exactly once.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

whitespaceChars :: [Char]
whitespaceChars = " \t\n"

-- this is like spaces, except it requires at least one
-- TODO: can this be replaced with something from Text.Parsec.Token?
spaces1 :: Parser ()
spaces1 = void $ many1 $ oneOf whitespaceChars

pSym :: Char -> Parser ()
pSym c = void $ lexeme $ char c

-----------------
-- identifiers --
-----------------

pVarName :: Parser VarName
pVarName = lexeme (iden <$> first <*> many rest)
  where
    iden c cs = VarName (c:cs)
    first = letter <|> char '_'
    rest  = digit  <|> first

-- TODO: should this include the Var itself?
pRef :: Parser ParsedExpr
pRef = Ref <$> pVarName

-------------
-- numbers --
-------------

-- TODO: allow negative numbers? (would currently conflict with minus op)
pNum :: Parser ParsedExpr
pNum = do
  n  <- digit
  ns <- lexeme $ many (digit <|> oneOf ".e-")
  return $ Num $ read (n:ns)

---------------------
-- string literals --
---------------------

-- list of chars which can be escaped in ShortCut
-- (they're also escaped in Haskell, so need extra backslashes here)
-- TODO: any others needed? check what people use in Windows filenames
escapeChars :: [Char]
escapeChars = "\\\""

literalChars :: [Char]
literalChars = filter valid $ map toEnum [0..127]
  where
    valid c = isPrint c && not (elem c escapeChars)

-- Tricky bit: the first quote char needs to NOT be a lexeme,
-- because that would consume spaces inside the string literal.
-- see stackoverflow.com/questions/24106314
-- TODO: can the between part be replaced with something from Text.Parsec.Token?
pQuoted :: Parser String
pQuoted = lexeme $ between (char '"') (char '"') $ many (lit <|> esc)
  where
    lit = oneOf literalChars
    esc = char '\\' *> oneOf escapeChars

pFil :: Parser ParsedExpr
pFil = Fil <$> pQuoted

-----------------------------
-- (assignment) statements --
-----------------------------

pVarEq :: Parser ParsedVar
pVarEq = pVarName <* (pSym '=')

pAssign :: Parser ParsedAssign
pAssign = lexeme ((,) <$> pVarEq <*> pExpr <* optional newline)

--------------
-- commands --
--------------

-- This is a kludge to make my "interesting" preference for spaces as function
-- application work right. It's used to test whether we've reached the end of a
-- list of arguments for the function currently being parsed.
pEnd :: Parser ()
pEnd = lookAhead $ void $ choice
  [ eof, pComment, pSym ')'
  , void $ try $ choice $ map pSym operatorChars
  , void $ try pVarEq
  ]

-- TODO: load names from somewhere else?
fnNames :: [String]
fnNames =
  [ "load_aa_seqs"
  , "load_na_seqs"
  , "load_genes"
  , "load_genomes"
  , "filter_genomes"
  , "filter_genes"
  , "worst_best_evalue"
  ]

-- TODO: put this in terms of "keyword" or something?
pName :: Parser String
pName = choice $ map (try . str) fnNames
  where
    str s = string s <* (void spaces1 <|> eof)

pCmd :: Parser ParsedExpr
pCmd = Cmd <$> pName <*> manyTill pTerm pEnd

-----------------
-- expressions --
-----------------

pParens :: Parser ParsedExpr
pParens = between (pSym '(') (pSym ')') pExpr

pTerm :: Parser ParsedExpr
pTerm = pParens <|> pCmd <|> try pNum <|> pFil <|> pRef

operatorChars :: [Char]
operatorChars = "+-*/&"

-- for now, I think all binary operators at the same precedence should work.
-- but it gets more complicated I'll write out an actual table here as
-- expected, with a prefix function too etc. see the jake wheat tutorial
operatorTable :: OpTable
operatorTable = [map binary operatorChars]
  where
    binary c = Infix (Bop c <$ pSym c) AssocLeft

-- This function automates building complicated nested grammars that parse
-- operators correctly. I'm using it in a very basic way though: one short list
-- of three operators with the same properties. I think it's good to have in
-- here though, because theoretically I could add fancier things like boolean
-- expressions by just changing the operatorTable.
-- see:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: Parser ParsedExpr
pExpr = buildExpressionParser operatorTable pTerm

--------------
-- comments --
--------------

-- Tricky bit: # should NOT be a lexeme
pComment :: Parser ()
pComment = lexeme $ void $ char '#' >> restOfLine
  where
    restOfLine = manyTill anyChar (void newline <|> eof)

-------------
-- scripts --
-------------

pScript :: Parser ParsedScript
pScript = optional spaces *> many pComment *> many (pAssign <* many pComment)
