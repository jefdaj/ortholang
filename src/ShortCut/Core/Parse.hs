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
  -- functiosn only used for testing
  , escapeChars
  -- , fnNames -- TODO load these from modules
  , literalChars
  , pComment
  , pNum
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

import Debug.Trace

import ShortCut.Core.Types

import ShortCut.Core.Debug    (debug)
import Control.Applicative    ((<|>), many)
import Control.Monad          (void, fail, when)
import Control.Monad.Identity (Identity)
import Data.Char              (isPrint)
import Data.List              (find, union)
import Data.Either            (isRight)
import Text.Parsec            (try, ParseError, getState, putState, (<?>))
import Text.Parsec.Char       (char, digit ,letter, spaces, anyChar,
                               newline, string, oneOf)
import Text.Parsec.Combinator (optional, many1, manyTill, eof
                              ,lookAhead, between, choice, anyToken, sepBy)
-- import Text.Parsec.Expr       (buildExpressionParser)
import qualified Text.Parsec.Expr as E

--------------------------
-- functions for export --
--------------------------

isExpr :: CutState -> String -> Bool
isExpr state line = isRight $ runParseM pExpr state line

-- TODO make this return the "result" assignment directly?
parseExpr :: CutState -> String -> Either ParseError CutExpr
parseExpr = runParseM pExpr

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
parseFile cfg path = readFile path' >>= return . parseString cfg
  where
    path' = debug cfg ("parse: '" ++ path ++ "'") path

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
  (scr, _) <- getState
  case lookup v scr of 
    Nothing -> fail $ "no such variable '" ++ var ++ "'" ++ "\n" -- ++ show scr
    Just e -> return $ CutRef (typeOf e) 0 (depsOf e) v

--------------
-- literals --
--------------

pNum :: ParseM CutExpr
pNum = do
  -- TODO optional minus sign here? see it doesn't conflict with subtraction
  -- TODO try this for negative numbers: https://stackoverflow.com/a/39050006
  n  <- digit
  ns <- lexeme $ many (digit <|> oneOf ".e-")
  return $ CutLit num 0 (n:ns)

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
pStr = CutLit str 0 <$> pQuoted <?> "string"

-- TODO remove the empty list case? can't imagine when it would be used
-- TODO how hard would it be to get Haskell's sequence notation? would it be useful?
-- TODO actually check that elements are the same type
pList :: ParseM CutExpr
pList = do
  terms <- between (pSym '[') (pSym ']') (sepBy pTerm $ pSym ',')
  let deps = foldr1 union $ map depsOf terms -- TODO what happens if []?
      rtn  = if null terms
               then EmptyList
               else typeOf $ head terms
  -- TODO assert that the rest of the terms match the first one here!
  when
    (not $ all (== rtn) (map typeOf terms))
    (error "all elements of a list must have the same type")
  return $ CutList rtn 0 deps terms

---------------
-- operators --
---------------

-- TODO load from modules somehow... or do individual fn parsers make it obsolete?
operatorChars :: [Char]
operatorChars = "+-*/&|~"

-- for now, I think all binary operators at the same precedence should work.
-- but it gets more complicated I'll write out an actual table here with a
-- prefix function too etc. see the jake wheat tutorial
-- TODO remove this once sure the version 2 below works
-- operatorTable :: [[E.Operator String CutState Identity CutExpr]]
-- operatorTable = [map binary operatorChars]
--   where
--     binary c = E.Infix (pBop c) E.AssocLeft

-- TODO once sure this works, remove the one above
operatorTable2 :: CutConfig -> [[E.Operator String CutState Identity CutExpr]]
operatorTable2 cfg = [map binary bops]
  where
    -- TODO extract these to utility functions in Types or somewhere?
    binary f = E.Infix (pBop2 $ fName f) E.AssocLeft
    -- TODO shit, seems the extra type arg might have been a bad idea
    --      could put (f undefined) here, but that smells bad!
    bops = filter (\f -> fFixity f == Infix) (concat $ map mFunctions mods)
    mods = cfgModules cfg

-- Tricky bit: needs to take two already-parsed expressions
-- TODO verify they have the correct types
-- TODO is this obsolete now that there's pBop2?
-- pBop :: Char -> ParseM (CutExpr -> CutExpr -> CutExpr)
-- pBop o = pSym o *> (return $ \e1 e2 ->
--   let deps = union (depsOf e1) (depsOf e2)
--   in CutBop (typeOf e1) deps [o] e1 e2)

-- TODO is there a better way than only taking one-char strings?
pBop2 :: String -> ParseM (CutExpr -> CutExpr -> CutExpr)
pBop2 [o] = pSym o *> (return $ \e1 e2 ->
  let deps = union (depsOf e1) (depsOf e2)
  in CutBop (typeOf e1) 0 deps [o] e1 e2)
pBop2  s  = fail $ "invalid binary op name '" ++ s ++ "'"

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
-- TODO get function names from modules
pName :: ParseM String
pName = do
  (_, cfg) <- getState
  (choice $ map (try . str') $ fnNames cfg) <?> "fn name"
  where
    str' s = string s <* (void spaces1 <|> eof)

-- TODO move to types? utils?
fnNames :: CutConfig -> [String]
fnNames cfg = map fName $ concat $ map mFunctions $ cfgModules cfg

-- TODO any way to do this last so "function not found" error comes through??
pFun :: ParseM CutExpr
pFun = do
  (_, cfg) <- getState
  -- find the function by name
  name <- pName
  void $ optional pComment
  args <- manyTill (pTerm <* optional pComment) pEnd
  let fns  = concat $ map mFunctions $ cfgModules cfg
      fn   = find (\f -> fName f == name) fns
      deps = foldr1 union $ map depsOf args
  case fn of
    -- TODO does this happen, or does it just move on to variables?
    --      it should commit to being a fn as soon as there are args!
    -- TODO use an error call here to definitively fail until you figure out Parsec
    Nothing -> (trace (name ++ " " ++ show args) (fail name))
    -- once found, have the function typecheck its own arguments
    Just f  -> case (fTypeCheck f) (map typeOf args) of
      Left  err -> error err
      Right rtn -> let rtn' = debug cfg ("parse: " ++ fName f ++ " " ++ show args) rtn
                   in return $ CutFun rtn' 0 deps (fName f) args

-----------------
-- expressions --
-----------------

pParens :: ParseM CutExpr
pParens = between (pSym '(') (pSym ')') pExpr <?> "parens"

-- TODO need to commit to separate branches so Parsec doesn't try to parse
--      other stuff after substitute_each fails. the rule should be:
--      once a fn/macro name is parsed it commits to that branch
--      if none of them work it moves on to others
--      without that we get silly errors like "no such variable" for any of them!
pTerm :: ParseM CutExpr
pTerm = (pList <|> pParens <|> pFun <|> pNum <|> pStr <|> pRef <?> "term") <* optional pComment

-- This function automates building complicated nested grammars that parse
-- operators correctly. It's kind of annoying, but I haven't figured out how
-- to do without it. Also it seems like it will get more useful if I want to
-- add non-assignment statements like assertions. See:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: ParseM CutExpr
-- pExpr = E.buildExpressionParser operatorTable pTerm <?> "expression"
pExpr = do
  (_, cfg) <- getState
  E.buildExpressionParser (operatorTable2 cfg) pTerm <?> "expression"

----------------
-- statements --
----------------

pVarEq :: ParseM CutVar
pVarEq = pVar <* (optional pComment) <* (pSym '=') <* (optional pComment) <?> "vareq"

-- TODO message in case it doesn't parse?
pAssign :: ParseM CutAssign
pAssign = do
  (scr, cfg) <- getState
  optional newline
  v <- pVarEq
  e <- lexeme pExpr
  putState (scr ++ [(v,e)], cfg)
  return (v,e)

-- Handles the special case of a naked top-level expression, which is treated
-- as being assigned to "result". This parses the same in a script or the repl,
-- but doing it more than once in a script will cause an error later.
-- TODO prevent assignments that include the variable being assigned to
--      (later when working on statement issues)
-- TODO if the statement is literally `result`, what do we do?
--      maybe we need a separate type of assignment statement for this?
pResult :: ParseM CutAssign
pResult = pExpr >>= \e -> return (CutVar "result", e)

pStatement :: ParseM CutAssign
pStatement = try pAssign <|> pResult

-------------
-- scripts --
-------------

-- TODO message in case it doesn't parse?
-- TODO should it get automatically `put` here, or manually in the repl?
pScript :: ParseM CutScript
pScript = do
  (_, cfg) <- getState
  optional spaces
  void $ many pComment
  scr <- many (pStatement <* many pComment)
  putState (scr, cfg)
  return scr