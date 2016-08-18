-- TODO: stop accidentally interpreting args in the wrong order as one big variable
-- TODO: start adding nice error messages to each parser
-- TODO: fix bug where a non-function with args parses to varname with args dropped
--       (example: 'this = load_that cool')

module ShortCut.Interpret.Parse where

import ShortCut.Types

import Control.Applicative    ((<|>), many)
import Control.Monad          (void, foldM)
import Control.Monad.Identity (Identity)
import Data.Char              (isPrint)
import Text.Parsec            (parse, try)
import Text.Parsec.Char       (char, digit ,letter, spaces, anyChar
                              ,newline, string, oneOf)
import Text.Parsec.Combinator (optional, many1, manyTill, eof
                              ,lookAhead, between, choice, anyToken)
import Text.Parsec.Expr       (buildExpressionParser, Assoc(..), Operator(..))
import Text.Parsec.Prim       (Parsec)
import Control.Monad.Except   (throwError)
import Control.Monad.RWS.Lazy (get)
import Control.Monad.Reader   (ask)

--------------------------------
-- helpers to simplify parsec --
--------------------------------

-- Some are from the Parsec tutorial here:
-- https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing

type Parser  = Parsec String ()
type OpTable = [[Operator String () Identity ParsedExpr]]

parseWithError :: Parser a -> String -> Either CutError a
parseWithError psr str = case parse psr "" str of
  Left  err -> Left $ InvalidSyntax err
  Right res -> Right res

regularParse :: Parser a -> String -> Either CutError a
regularParse p = parseWithError p

parseWithEof :: Parser a -> String -> Either CutError a
parseWithEof p = parseWithError (p <* eof)

parseAndShow :: (Show a) => Parser a -> String -> String
parseAndShow p s = case regularParse p s of
  Left err -> show err
  Right s2 -> show s2

parseWithLeftOver :: Parser a -> String -> Either CutError (a,String)
parseWithLeftOver p = parseWithError ((,) <$> p <*> leftOver)
  where
    leftOver = manyTill anyToken eof

-- this works fine, but isn't used so far:
-- parseWithWSEof :: Parser a -> String -> Either CutError a
-- parseWithWSEof p = parseWithEof (whiteSpace *> p)
--   where
--     whiteSpace = void $ many $ oneOf " \n\t"

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
operatorChars = "+-*/&|~"

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

-----------------------------------------
-- typechecking (not nearly done yet!) --
-----------------------------------------

tExpr :: ParsedExpr -> CutM TypedExpr
tExpr   (Fil s)     = return $ TStr s
tExpr   (Num n)     = return $ TNum n
tExpr r@(Ref _)     = tRef r
tExpr c@(Cmd _ _  ) = tCmd c
tExpr b@(Bop _ _ _) = tBop b

tAssign :: ParsedAssign -> CutM TypedAssign
tAssign (var, expr) = do
  cexpr <- tExpr expr
  return (var, cexpr) -- TODO is the var always going to be OK?

-- I'm not sure what this is supposed to mean design-wise,
-- but it has the type required by foldM for use in tScript
foldAssign :: TypedScript -> ParsedAssign -> CutM TypedScript
foldAssign script assign = do
  cfg <- ask
  let (cassign, _, _) = runCutM (tAssign assign) cfg script -- TODO pass it the cfg
  case cassign of
    Left err -> throwError err
    Right c  -> return $ script ++ [c]

tScript :: ParsedScript -> CutM TypedScript
tScript = foldM foldAssign []

tBop :: ParsedExpr -> CutM TypedExpr
tBop (Bop o a1 a2) = do
  a1' <- tExpr a1
  a2' <- tExpr a2
  -- TODO assert o `elem` "+-*/&|~"
  -- TODO assert (getExt a1 == getExt a2)
  -- TODO if o `elem` "+-*/" assert (getExt a1) == "num"
  -- TODO if o `elem` "&|~"  assert (getExt a1) == "set of something"
  return (TBop (getExt a1') [o] a1' a2')
tBop x = error $ "bad argument to tBop: '" ++ show x ++ "'"

tCmd :: ParsedExpr -> CutM TypedExpr
tCmd (Cmd c as) = do
  -- TODO check return types
  as' <- mapM tExpr as
  return $ TCmd (Ext rtype) c as'
  where
    (rtype, _) = case c of
      "load_aa_seqs"      -> ("faa"    ,["str"])
      "load_na_seqs"      -> ("fna"    ,["str"])
      "load_genes"        -> ("genes"  ,["str"])
      "load_genomes"      -> ("genomes",["str"])
      "filter_genomes"    -> ("genomes",["genomes", "genes"  , "num"])
      "filter_genes"      -> ("genes"  ,["genes"  , "genomes", "num"])
      "worst_best_evalue" -> ("num"    ,["genes"  , "genomes"])
      x -> error $ "bad argument to tCmd: '" ++ show x ++ "'"
tCmd x = error $ "bad argument to tCmd: '" ++ show x ++ "'"

tRef :: ParsedExpr -> CutM TypedExpr
tRef (Ref v@(VarName var)) = do
  s <- get
  case lookup v s of
    Nothing -> throwError $ NoSuchVariable var
    Just e -> return $ TRef (getExt e) v
tRef x = error $ "bad argument to tRef: '" ++ show x ++ "'"
