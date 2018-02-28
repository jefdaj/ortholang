module ShortCut.Core.Parse.Basic where

import ShortCut.Core.Types

import Control.Applicative    ((<|>), many)
import Control.Monad          (void, fail)
import Data.Char              (isPrint)
import Data.Scientific        (Scientific())
import Text.Parsec            (getState, (<?>))
import Text.Parsec.Char       (char, digit ,letter, spaces, oneOf)
import Text.Parsec.Combinator (many1, between, notFollowedBy)

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

pEq :: ParseM ()
pEq = void $ spaces <* pSym '='

pVarEq :: ParseM CutVar
pVarEq = pVar <* pEq <?> "vareq"

-- A reference is just a variable name, but that variable has to be in the script.
-- TODO why does it fail after this, but only sometimes??
pRef :: ParseM CutExpr
pRef = do
  v@(CutVar var) <- pVar <* notFollowedBy pEq
  (scr, _, _) <- getState
  case lookup v scr of
    Nothing -> fail $ "no such variable '" ++ var ++ "'" ++ "\n" -- ++ show scr
    Just e -> return $ CutRef (typeOf e) 0 (depsOf e) v

--------------
-- literals --
--------------

-- TODO hey Scientific has its own parser, would it work to add?
pNum :: ParseM CutExpr
pNum = do
  -- TODO optional minus sign here? see it doesn't conflict with subtraction
  -- TODO try this for negative numbers: https://stackoverflow.com/a/39050006
  n  <- digit
  ns <- many (digit <|> oneOf ".e-")
  spaces
  -- read + show puts it in "canonical" form to avoid duplicate tmpfiles
  let lit = show (read (n:ns) :: Scientific)
  return $ CutLit num 0 lit

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
