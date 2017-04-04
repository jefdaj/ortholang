module ShortCut.Core.Parse where

-- TODO stop accidentally interpreting args in the wrong order as one big variable
-- TODO fix bug where a non-function with args parses to varname with args dropped
--       (example: 'this = load_that cool')

import ShortCut.Core.Types

import Control.Applicative    ((<|>), many)
import Control.Monad          (void)
import Control.Monad.Identity (Identity)
import Data.Char              (isPrint)
import Text.Parsec            (try, ParseError, getState, putState, (<?>))
import Text.Parsec.Char       (char, digit ,letter, spaces, anyChar,
                               newline, string, oneOf)
import Text.Parsec.Combinator (optional, many1, manyTill, eof
                              ,lookAhead, between, choice, anyToken)
import Text.Parsec.Expr       (buildExpressionParser, Assoc(..), Operator(..))

--------------------------------
-- helpers to simplify parsec --
--------------------------------

-- Some are from the Parsec tutorial here:
-- https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing

parseWithEof :: ParseM a -> CutScript -> String -> Either ParseError a
parseWithEof p s = runParseM (p <* eof) s

parseAndShow :: (Show a) => ParseM a -> CutScript -> String -> String
parseAndShow p s str' = case runParseM p s str' of
  Left err -> show err
  Right s2 -> show s2

parseWithLeftOver :: ParseM a -> CutScript -> String -> Either ParseError (a,String)
parseWithLeftOver p s = runParseM ((,) <$> p <*> leftOver) s
  where
    leftOver = manyTill anyToken eof

-- There's a convention in parsers that each one should consume whitespace
-- after itself (handled by this function), and you only skip leading
-- whitespace at the top level. That way all the whitespace gets skipped
-- exactly once. (I guess it would work the other way around too)
lexeme :: ParseM a -> ParseM a
lexeme p = p <* spaces

spaceChars :: [Char]
spaceChars = " \t\n"

-- this is like spaces, except it requires at least one
-- TODO can this be replaced with something from Text.Parsec.Token?
spaces1 :: ParseM ()
spaces1 = void $ many1 $ oneOf spaceChars

pSym :: Char -> ParseM ()
pSym c = void $ lexeme $ char c

--------------
-- comments --
--------------

-- Tricky bit: # should NOT be a lexeme
pComment :: ParseM ()
pComment = (lexeme $ void $ char '#' >> restOfLine) <?> "comment"
  where
    restOfLine = manyTill anyChar (void newline <|> eof)

-----------------
-- identifiers --
-----------------

pVar :: ParseM CutVar
pVar = lexeme (iden <$> first <*> many rest) <?> "variable"
  where
    iden c cs = CutVar (c:cs)
    -- TODO allow variable names that start with numbers too?
    first = letter <|> char '_'
    rest  = digit  <|> first

-- A reference is just a variable name, but that variable has to be in the script.
-- TODO why does it fail after this, but only sometimes??
pRef :: ParseM CutExpr
pRef = do
  v@(CutVar var) <- pVar
  scr <- getState
  case lookup v scr of 
    Nothing -> fail $ "no such variable '" ++ var ++ "'" ++ "\n" ++ show scr
    Just e -> return $ CutRef (typeOf e) v

--------------
-- literals --
--------------

pNum :: ParseM CutExpr
pNum = do
  -- TODO optional minus sign here? see it doesn't conflict with subtraction
  n  <- digit
  ns <- lexeme $ many (digit <|> oneOf ".e-")
  return $ CutLit num (n:ns)

-- list of chars which can be escaped in ShortCut
-- (they're also escaped in Haskell, so need extra backslashes here)
-- TODO any others needed? check what people use in Windows filenames
escapeChars :: [Char]
escapeChars = "\\\""

literalChars :: [Char]
literalChars = filter valid $ map toEnum [0..127]
  where
    valid c = isPrint c && not (elem c escapeChars)

-- Tricky bit: the first quote char needs to NOT be a lexeme,
-- because that would consume spaces inside the string literal.
-- see stackoverflow.com/questions/24106314
-- TODO can the between part be replaced with something from Text.Parsec.Token?
pQuoted :: ParseM String
pQuoted = (lexeme $ between (char '"') (char '"') $ many (lit <|> esc)) <?> "quoted"
  where
    lit = oneOf literalChars
    esc = char '\\' *> oneOf escapeChars

pStr :: ParseM CutExpr
pStr = CutLit str <$> pQuoted <?> "string"

---------------
-- operators --
---------------

operatorChars :: [Char]
operatorChars = "+-*/&|~"

-- for now, I think all binary operators at the same precedence should work.
-- but it gets more complicated I'll write out an actual table here with a
-- prefix function too etc. see the jake wheat tutorial
operatorTable :: [[Operator String CutScript Identity CutExpr]]
operatorTable = [map binary operatorChars]
  where
    binary c = Infix (pBop c) AssocLeft

-- Tricky bit: needs to take two already-parsed expressions
-- TODO verify they have the correct types
pBop :: Char -> ParseM (CutExpr -> CutExpr -> CutExpr)
pBop o = pSym o *> (return $ \e1 e2 -> CutBop (typeOf e1) [o] e1 e2)

---------------
-- functions --
---------------

-- This is a kludge to make my "interesting" preference for spaces as function
-- application work right. It's used to test whether we've reached the end of a
-- list of arguments for the function currently being parsed.
pEnd :: ParseM ()
pEnd = lookAhead $ void $ choice
  [ eof, pComment, pSym ')'
  , void $ try $ choice $ map pSym operatorChars
  , void $ try pVarEq
  ]

-- TODO load names from modules, of course
-- TODO put this in terms of "keyword" or something?
pName :: ParseM String
pName = (choice $ map (try . str') fnNames) <?> "fn name"
  where
    str' s = string s <* (void spaces1 <|> eof)

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

pFun :: ParseM CutExpr
pFun = do
  name <- pName
  args <- manyTill pTerm pEnd
  let (rtype, _) = case name of
                    "load_aa_seqs"      -> (faa, [str])
                    "load_na_seqs"      -> (fna, [str])
                    "load_genes"        -> (gen, [str])
                    "load_genomes"      -> (gom, [str])
                    "filter_genomes"    -> (gom, [SetOf gom, SetOf gen, num])
                    "filter_genes"      -> (gen, [SetOf gen, SetOf gom, num])
                    "worst_best_evalue" -> (num, [SetOf gen, SetOf gom]) 
                    x -> error $ "bad argument to pFun: '" ++ show x ++ "'"
  return $ CutFun rtype name args

-----------------
-- expressions --
-----------------

pParens :: ParseM CutExpr
pParens = between (pSym '(') (pSym ')') pExpr <?> "parens"

pTerm :: ParseM CutExpr
pTerm = pParens <|> pFun <|> try pNum <|> pStr <|> pRef <?> "term"

-- This function automates building complicated nested grammars that parse
-- operators correctly. It's kind of annoying, but I haven't figured out how
-- to do without it. Also it seems like it will get more useful if I want to
-- add non-assignment statements like assertions. See:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: ParseM CutExpr
pExpr = buildExpressionParser operatorTable pTerm <?> "expression"

----------------
-- statements --
----------------

pVarEq :: ParseM CutVar
pVarEq = pVar <* (pSym '=') <?> "vareq"

-- TODO message in case it doesn't parse?
pAssign :: ParseM CutAssign
pAssign = do
  scr <- getState
  optional newline
  v <- pVarEq
  e <- lexeme pExpr
  putState $ scr ++ [(v,e)]
  return (v,e)

-- Handles the special case of a naked top-level expression, which is treated
-- as being assigned to "result". This parses the same in a script or the repl,
-- but doing it more than once in a script will cause an error later.
-- TODO prevent assignments that include the variable being assigned to
--      (later when working on statement issues)
pResult :: ParseM CutAssign
pResult = pExpr >>= \e -> return (CutVar "result", e)

pStatement :: ParseM CutAssign
pStatement = pAssign <|> pResult

-------------
-- scripts --
-------------

-- TODO message in case it doesn't parse?
-- TODO should it get automatically `put` here, or manually in the repl?
pScript :: ParseM CutScript
pScript = do
  optional spaces
  void $ many pComment
  scr <- many (pStatement <* many pComment)
  putState scr
  return scr
