module ShortCut.Core.Parse.Basic where

import ShortCut.Core.Types
-- import ShortCut.Core.Pretty (Pretty, pPrint, render)

import Control.Applicative    ((<|>), many)
import Control.Monad          (void, fail)
import Data.Char              (isPrint)
import Data.Scientific        (Scientific())
import Text.Parsec            (getState, (<?>), getParserState, stateInput, try)
import Text.Parsec.Char       (char, digit ,letter, spaces, oneOf)
import Text.Parsec.Combinator (many1, between, notFollowedBy)
import Debug.Trace       (trace, traceM)

debugParser :: Show a => String -> ParseM a -> ParseM a
debugParser name pFn = do
  (_, cfg, _) <- getState
  if cfgDebug cfg
    then do
      inp <- fmap stateInput getParserState
      let nxt = take 10 inp ++ if length inp > 10 then "..." else ""
      res  <- pFn
      inp' <- fmap stateInput getParserState
      let nxt' = take 10 inp' ++ if length inp' > 10 then "..." else ""
          msg  = name ++ " " ++ show nxt ++ " -> (" ++ show res ++ ", " ++ show nxt' ++ ")"
      return $ trace msg res
    else pFn

debugParseM :: String -> ParseM ()
debugParseM msg = do
  (_, cfg, _) <- getState
  if cfgDebug cfg
    then traceM msg
    else return ()

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

pIden :: ParseM String
pIden = debugParser "pIden" (lexeme ((:) <$> first <*> many rest) <?> "variable name")
  where
    -- iden c cs = CutVar (c:cs)
    -- TODO allow variable names that start with numbers too?
    first = letter <|> char '_'
    rest  = digit  <|> first

pVar :: ParseM CutVar
pVar = debugParser "pVar" (CutVar <$> pIden)

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
pQuoted = debugParser "pQuoted" ((lexeme $ between (char '"') (char '"') $ many (lit <|> esc)) <?> "quoted")
  where
    lit = oneOf literalChars
    esc = char '\\' *> oneOf escapeChars

pStr :: ParseM CutExpr
pStr = debugParser "pStr" (CutLit str 0 <$> pQuoted <?> "string literal")
