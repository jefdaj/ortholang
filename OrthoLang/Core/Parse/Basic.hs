module OrthoLang.Core.Parse.Basic
  (
  -- * Utilities
    putAssign

  -- * Whitespace and endings
  , lexeme
  , spaceChars
  , spaces1
  , pEnd

  -- * Misc utilities
  , escapeChars
  , literalChars
  , pSym

  -- * Variable assignment
  , pIden
  , pVar
  , pVarOnly
  , pEq
  , pVarEq

  -- * Literals
  , pNum
  , pQuoted
  , pStr
  )
  where

-- TODO hold up, is Logging missing a bunch of NOINLINE statements?

import OrthoLang.Core.Types
import qualified Data.Map.Strict as M

import Control.Applicative    ((<|>), many)
import Control.Monad          (void, fail)
import Data.Char              (isPrint)
import Data.Scientific        (Scientific())
import Text.Parsec            (getState, putState, (<?>), try)
import Text.Parsec.Char       (char, digit ,letter, spaces, oneOf)
import Text.Parsec.Combinator (many1, between, notFollowedBy, choice, lookAhead, eof, optionMaybe, anyToken)
import Text.Parsec.Prim       (ParsecT, Stream)
-- import OrthoLang.Core.Paths   (exprDigests)
import OrthoLang.Core.Parse.Util (ParseM, debugParser)
import OrthoLang.Util    (trace)
import Control.Monad.Reader   (ask)

--------------
-- utilites --
--------------

-- TODO remove if this is only used once in pScript
putAssign :: String -> Assign -> ParseM ()
putAssign name a = do
  s <- getState
  let s' = trace name ("adding assignment: " ++ show a) $ s ++ [a]
  putState $ s'

{-|
There's a convention in parsers that each one should consume whitespace after
itself (handled by this function), and you only skip leading whitespace at the
top level. That way all the whitespace gets skipped exactly once. (I guess it
would work the other way around too)
-}
lexeme :: ParseM a -> ParseM a
lexeme p = p <* spaces

spaceChars :: [Char]
spaceChars = " \t\n"

{-|
This is like spaces, except it requires at least one.

TODO can this be replaced with something from Text.Parsec.Token?
-}
spaces1 :: ParseM ()
spaces1 = debugParser "spaces1" ((void $ many1 $ oneOf spaceChars) <?> "whitespace and/or newline")

pSym :: Char -> ParseM ()
pSym c = debugParser ("pSym " ++ [c]) (void $ lexeme $ char c) <?> "symbol \"" ++ [c] ++ "\""

-----------------
-- identifiers --
-----------------

{-|
This is a kludge to make my preference for Haskell-style function application
with spaces work. It's used to test whether we've reached the end of a list of
arguments for the function currently being parsed. Probably very slow.

TODO can factor the try out to be by void right?

TODO error in here when it somehow succeeds on full7942?

TODO this must be succeding on 'loaner... right?
-}
pEnd :: ParseM ()
pEnd = debugParser "pEnd" $ do
  -- (cfg, _) <- getState
  cfg <- ask
  try $ lookAhead $ choice
    [ eof
    , void $ choice $ map pSym $ operatorChars cfg ++ ")],"
    , try $ void $ pVarEq
    ]

pIden :: ParseM String
pIden = debugParser "pIden" $ lexeme $ do
  c  <- first
  cs <- many rest <* try (notFollowedBy rest)
  return (c:cs)
  where
    -- TODO allow variable names that start with numbers too? capital letters?
    first = letter
    rest  = letter <|> digit <|> oneOf "-_"

pVar :: ParseM Var
pVar = debugParser "pVar" (Var (RepID Nothing) <$> pIden)

-- TODO is the error in here?? maybe it consumes a space and therefore doesn't fail?
pEq :: ParseM ()
-- pEq = debugParser "pEq" ((void $ spaces <* pSym '=') <?> "equals sign")
pEq = debugParser "pEq" $ pSym '='

pVarEq :: ParseM Var
pVarEq = debugParser "pVarEq" ((pVar <* pEq) <?> "variable assignment")

pVarOnly :: ParseM Var
pVarOnly = debugParser "pVarOnly " (pVar <* notFollowedBy pEq)

--------------
-- literals --
--------------

-- TODO hey Scientific has its own parser, would it work to add?
pNum :: ParseM Expr
pNum = debugParser "pNum" $ do
  -- TODO optional minus sign here? see it doesn't conflict with subtraction
  -- TODO try this for negative numbers: https://stackoverflow.com/a/39050006
  neg <- try $ optionMaybe $ char '-'
  n  <- digit
  ns <- many (digit <|> oneOf ".e-")
  spaces
  -- read + show puts it in "canonical" form to avoid duplicate tmpfiles
  let sign = case neg of { Just x -> x; _ -> ' ' }
      lit  = show (read (sign:n:ns) :: Scientific)
      expr = Lit num (Salt 0) lit 
  -- putDigests "pNum" [expr]
  return expr

-- list of chars which can be escaped in OrthoLang
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
pQuoted = debugParser "pQuoted" ((lexeme $ between (char '\"') (char '\"') $ many (lit <|> esc)) <?> "quoted")
  where
    lit = oneOf literalChars
    esc = char '\\' *> oneOf escapeChars

pStr :: ParseM Expr
pStr = debugParser "pStr" $ do
  expr <- Lit str (Salt 0) <$> pQuoted <?> "string literal"
  -- putDigests "pStr" [expr]
  return expr
