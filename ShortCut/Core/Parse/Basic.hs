module ShortCut.Core.Parse.Basic where

-- TODO hold up, is Logging missing a bunch of NOINLINE statements?

import ShortCut.Core.Types
import ShortCut.Core.Util (trace)
-- import ShortCut.Core.Pretty (Pretty, pPrint, render)

import Control.Applicative    ((<|>), many)
import Control.Monad          (void, fail)
import Data.Char              (isPrint)
import Data.Scientific        (Scientific())
import Text.Parsec            (getState, (<?>), try)
import Text.Parsec.Char       (char, digit ,letter, spaces, oneOf)
import Text.Parsec.Combinator (many1, between, notFollowedBy, choice, lookAhead, eof, optionMaybe, anyToken)
import Text.Parsec.Prim (ParsecT, Stream)

-- based on Text.Parsec.Combinator.parserTrace, but:
--   uses the logging module
--   shortens long strings to fit on one line
parserTrace' :: (Show t, Stream s m t) => String -> ParsecT s u m ()
parserTrace' s = pt <|> return ()
    where
        n = 30
        pt = try $ do
           x <- try $ many1 anyToken
           let x' = let sx = show x in if length sx > n then take n sx ++ "\"..." else sx
           -- TODO why does this output to stderr no matter what??
           --      the extra messages don't obey the filter either
           trace "core.parser" (s ++ ": " ++ x') $ try $ eof
           fail x'

-- based on Text.Parsec.Combinator.parserTraced, but:
--   uses the logging module
--   shortens long strings to fit on one line
parserTraced' :: (Stream s m t, Show t) => String -> ParsecT s u m b -> ParsecT s u m b
parserTraced' s p = do
  parserTrace' s
  -- TODO why does this output to stderr no matter what??
  --      the extra messages don't obey the filter either
  p <|> trace "core.parser" (s ++ " backtracked") (fail s)
  -- p

-- trace for a parser
-- TODO go back to removing it when not in debug mode for speed, even though order might change?
debugParser :: Show a => String -> ParseM a -> ParseM a
debugParser name pFn = do
  parserTraced' name pFn

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
spaces1 = debugParser "spaces1" ((void $ many1 $ oneOf spaceChars) <?> "whitespace and/or newline")

pSym :: Char -> ParseM ()
pSym c = debugParser ("pSym " ++ [c]) (void $ lexeme $ char c) <?> "symbol '" ++ [c] ++ "'"

-----------------
-- identifiers --
-----------------

-- Tests for the end of a single argument: space, comma, close bracket of some kind, or eof.
-- Doesn't consume any input or return a value.
-- TODO use this in pEnd? and rename that pEndArgs
-- TODO is there any reason not to use pEnd itself for this?
-- pEndArg :: ParseM ()
-- pEndArg = do
--   (_, cfg, _, _) <- getState
--   lookAhead $ void $ choice $ map (try . pSym) $ operatorChars cfg ++ ")],"

-- This is a kludge to make my "interesting" preference for spaces as function
-- application work right. It's used to test whether we've reached the end of a
-- list of arguments for the function currently being parsed.
-- TODO can factor the try out to be by void right?
-- TODO error in here when it somehow succeeds on full7942?
-- TODO this must be succeding on 'loaner... right?
pEnd :: ParseM ()
pEnd = debugParser "pEnd" $ do
  (_, cfg, _, _) <- getState
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

pVar :: ParseM CutVar
pVar = debugParser "pVar" (CutVar (ReplaceID Nothing) <$> pIden)

-- TODO is the error in here?? maybe it consumes a space and therefore doesn't fail?
pEq :: ParseM ()
-- pEq = debugParser "pEq" ((void $ spaces <* pSym '=') <?> "equals sign")
pEq = debugParser "pEq" $ pSym '='

pVarEq :: ParseM CutVar
pVarEq = debugParser "pVarEq" ((pVar <* pEq) <?> "variable assignment")

pVarOnly :: ParseM CutVar
pVarOnly = debugParser "pVarOnly " (pVar <* notFollowedBy pEq)

--------------
-- literals --
--------------

-- TODO hey Scientific has its own parser, would it work to add?
pNum :: ParseM CutExpr
pNum = debugParser "pNum" $ do
  -- TODO optional minus sign here? see it doesn't conflict with subtraction
  -- TODO try this for negative numbers: https://stackoverflow.com/a/39050006
  neg <- try $ optionMaybe $ char '-'
  n  <- digit
  ns <- many (digit <|> oneOf ".e-")
  spaces
  -- read + show puts it in "canonical" form to avoid duplicate tmpfiles
  let sign = case neg of
               Just s -> s
               _ -> ' '
      lit = show (read (sign:n:ns) :: Scientific)
  return $ CutLit num (RepeatSalt 0) lit

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
pQuoted = debugParser "pQuoted" ((lexeme $ between (char '"') (char '"') $ many (lit <|> esc)) <?> "quoted")
  where
    lit = oneOf literalChars
    esc = char '\\' *> oneOf escapeChars

pStr :: ParseM CutExpr
pStr = debugParser "pStr" (CutLit str (RepeatSalt 0) <$> pQuoted <?> "string literal")
