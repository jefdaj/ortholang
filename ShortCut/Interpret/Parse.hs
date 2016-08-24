-- TODO stop accidentally interpreting args in the wrong order as one big variable
-- TODO start adding nice error messages to each parser
-- TODO fix bug where a non-function with args parses to varname with args dropped
--       (example: 'this = load_that cool')

-- TODO maybe instead of making it all one step, the simple thing is to 
--      have a second step for just *tagging* everything with the right
--      extensions? and since there's only one constructor for lit now,
--      the tags can just be added around the expressions themselves

-- TODO and if we're going this direction, is there any need for a Ref
--      to have a Var inside it rather than just a string? Maybe it makes sense

-- TODO add more useful error messages with <?>

module ShortCut.Interpret.Parse where

import ShortCut.Types

import Control.Applicative    ((<|>), many)
-- import Control.Monad          (void, foldM, fail)
import Control.Monad          (void)
import Control.Monad.Identity (Identity)
import Data.Char              (isPrint)
import Text.Parsec            (try, ParseError, getState)
import Text.Parsec.Char       (char, digit ,letter, spaces, anyChar
                              ,newline, string, oneOf)
import Text.Parsec.Combinator (optional, many1, manyTill, eof
                              ,lookAhead, between, choice, anyToken)
import Text.Parsec.Expr       (buildExpressionParser, Assoc(..), Operator(..))
-- import Data.Scientific (Scientific)

--------------------------------
-- helpers to simplify parsec --
--------------------------------

-- Some are from the Parsec tutorial here:
-- https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing

-- parseWithError :: ParseM a -> CutState -> String -> Either ParseError a
-- parseWithError parser state str' = case runParseM parser state str' of
--   -- Left  err -> Left $ InvalidSyntax err
--   Left  err -> fail $ "invalid syntax: " ++ show err
--   Right res -> Right res

-- parseWithError = runParseM

-- TODO remove? is it exactly parseWithError?
-- regularParse :: ParseM a -> CutState -> String -> Either CutError a
-- regularParse p s = parseWithError p s

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

-- this works fine, but isn't used so far:
-- parseWithWSEof :: ParseM a -> String -> Either CutError a
-- parseWithWSEof p = parseWithEof (whiteSpace *> p)
--   where
--     whiteSpace = void $ many $ oneOf " \n\t"

-- There's a convention in parsers that each one should consume whitespace
-- after itself (handled by this function), and you only skip leading
-- whitespace at the top level. That way all the whitespace gets skipped
-- exactly once.
lexeme :: ParseM a -> ParseM a
lexeme p = p <* spaces

whitespaceChars :: [Char]
whitespaceChars = " \t\n"

-- this is like spaces, except it requires at least one
-- TODO can this be replaced with something from Text.Parsec.Token?
spaces1 :: ParseM ()
spaces1 = void $ many1 $ oneOf whitespaceChars

pSym :: Char -> ParseM ()
pSym c = void $ lexeme $ char c

-----------------
-- identifiers --
-----------------

pVar :: ParseM CutVar
pVar = lexeme (iden <$> first <*> many rest)
  where
    iden c cs = CutVar (c:cs)
    first = letter <|> char '_'
    rest  = digit  <|> first

-- TODO should this include the Var itself?
-- TODO how to look up the variable? guess this requires the monad?
pRef :: ParseM CutExpr
-- pRef = CutRef <$> pVar
pRef = do
  v@(CutVar var) <- pVar
  -- scr <- getScript
  (scr, _) <- getState
  case lookup v scr of 
    -- TODO replace CutError with regular Parsec errors? Needed for anything else?
    -- Nothing -> throwError $ NoSuchVariable var
    Nothing -> fail $ "no such variable '" ++ var ++ "'"
    Just e -> return $ CutRef (typeOf e) v

-- tRef :: CutExpr -> ParseM CutExpr
-- tRef (Ref v@(CutVar var)) = do
--   (s, _) <- get
--   case lookup v s of
--     Nothing -> throwError $ NoSuchVariable var
--     Just e -> return $ CutRef (typeOf e) v
-- tRef x = error $ "bad argument to tRef: '" ++ show x ++ "'"

-------------
-- numbers --
-------------

-- TODO allow negative numbers? (would currently conflict with minus op)
pNum :: ParseM CutExpr
pNum = do
  -- TODO optional minus sign here? see it doesn't conflict with subtraction
  n  <- digit
  ns <- lexeme $ many (digit <|> oneOf ".e-")
  return $ CutLit num (n:ns)

---------------------
-- string literals --
---------------------

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
pQuoted = lexeme $ between (char '"') (char '"') $ many (lit <|> esc)
  where
    lit = oneOf literalChars
    esc = char '\\' *> oneOf escapeChars

-- this is just any literal that doens't parse as a number with pNum
pStr :: ParseM CutExpr
pStr = CutLit str <$> pQuoted

----------------
-- statements --
----------------

pVarEq :: ParseM CutVar
pVarEq = pVar <* (pSym '=')

pAssign :: ParseM CutAssign
pAssign = lexeme ((,) <$> pVarEq <*> pExpr <* optional newline)

-- tAssign :: CutAssign -> CutM CutAssign
-- tAssign (var, expr) = do
--   cexpr <- tExpr expr
--   return (var, cexpr) -- TODO is the var always going to be OK?

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

-- TODO put this in terms of "keyword" or something?
pName :: ParseM String
pName = choice $ map (try . str') fnNames
  where
    str' s = string s <* (void spaces1 <|> eof)

-- pFun :: ParseM CutExpr
-- pFun = Fun <$> pName <*> manyTill pTerm pEnd

-- tFun :: CutExpr -> CutM CutExpr
-- tFun (Fun c as) = do
--   -- TODO check return types
--   as' <- mapM tExpr as
--   return $ CutFun rtype c as'
--   where
--     (rtype, _) = case c of
--       "load_aa_seqs"      -> (faa, ["str"])
--       "load_na_seqs"      -> (fna, ["str"])
--       "load_genes"        -> (gen, ["str"])
--       "load_genomes"      -> (gom, ["str"])
--       "filter_genomes"    -> (gom, ["genomes", "genes"  , "num"])
--       "filter_genes"      -> (gen, ["genes"  , "genomes", "num"])
--       "worst_best_evalue" -> (num, ["genes"  , "genomes"])
--       x -> error $ "bad argument to tFun: '" ++ show x ++ "'"
-- tFun x = error $ "bad argument to tFun: '" ++ show x ++ "'"

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
pParens = between (pSym '(') (pSym ')') pExpr

pTerm :: ParseM CutExpr
pTerm = pParens <|> pFun <|> try pNum <|> pStr <|> pRef

operatorChars :: [Char]
operatorChars = "+-*/&|~"

-- This function automates building complicated nested grammars that parse
-- operators correctly. It's kind of annoying, but I haven't figured out how
-- to do without it. Also it seems like it will get more useful if I want to
-- add non-assignment statements like assertions. See:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: ParseM CutExpr
pExpr = buildExpressionParser operatorTable pTerm

-- TODO move to Types.hs?
type OpTable = [[Operator String CutState Identity CutExpr]]

-- for now, I think all binary operators at the same precedence should work.
-- but it gets more complicated I'll write out an actual table here as
-- expected, with a prefix function too etc. see the jake wheat tutorial
operatorTable :: OpTable
operatorTable = [map binary operatorChars]
  where
    binary c = Infix (pSym c *> pBop c) AssocLeft

-- this is weird, but has the type needed to go in an OpTable...
-- err, i guess it makes sense because the terms have to be parsed separately
pBop :: Char -> ParseM (CutExpr -> CutExpr -> CutExpr)
pBop o = return $ \e1 e2 -> CutBop (typeOf e1) [o] e1 e2

--------------
-- comments --
--------------

-- Tricky bit: # should NOT be a lexeme
pComment :: ParseM ()
pComment = lexeme $ void $ char '#' >> restOfLine
  where
    restOfLine = manyTill anyChar (void newline <|> eof)

-------------
-- scripts --
-------------

pScript :: ParseM CutScript
pScript = optional spaces *> many pComment *> many (pAssign <* many pComment)
