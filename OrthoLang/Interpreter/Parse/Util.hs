{-|
Parse utilities. Some are from the Parsec tutorial here:

<https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing>

Most aren't needed in production, but they help with debugging.
-}

module OrthoLang.Interpreter.Parse.Util
  (
  -- * Parse utilities
    parseAndShow
  , parseWithLeftOver
  , parseWithEof
  , parserTrace'
  , parserTraced'
  , debugParser
  -- , runParseM

  -- * New parse utilities with errors
  -- , ParseM
  -- , runParseM
  -- , parseFail
  )
  where

import OrthoLang.Debug
import OrthoLang.Types
import Text.Parsec.Combinator     (manyTill, eof, anyToken)
import Text.Parsec hiding (Empty)
import Control.Monad.Trans.Except
import Control.Monad.Trans
import Control.Monad.Reader (runReaderT)

-- TODO make an empty GlobalEnv so you can run these in ghci again

-- TODO is this ever needed in production? probably not
parseAndShow :: (Show a) => [Module] -> ParseM a -> Config -> Script -> String -> String
parseAndShow ms p c s str' = case runParseM ms p c s str' of
  Left err -> show err
  Right s2 -> show s2

parseWithLeftOver :: [Module] -> ParseM a -> Config -> Script -> String -> Either String (a,String)
parseWithLeftOver ms p c s = runParseM ms ((,) <$> p <*> leftOver) c s
  where
    leftOver = manyTill anyToken eof

parseWithEof :: [Module] -> ParseM a -> Config -> Script -> String -> Either String a
parseWithEof ms p c s = runParseM ms (p <* eof) c s

{-|
Based on Text.Parsec.Combinator.parserTrace, but:

* uses the logging module
* shortens long strings to fit on one line
-}
parserTrace' :: (Show t, Stream s m t) => String -> ParsecT s u m ()
parserTrace' s = pt <|> return ()
    where
        n = 30
        pt = try $ do
           x <- try $ many1 anyToken
           let x' = let sx = show x in if length sx > n then take n sx ++ "\"..." else sx
           trace ("core.parser." ++ s) (x') $ try $ eof
           fail x'

{-|
Based on Text.Parsec.Combinator.parserTraced, but:

* uses the logging module
* shortens long strings to fit on one line
-}
parserTraced' :: (Stream s m t, Show t) => String -> ParsecT s u m b -> ParsecT s u m b
parserTraced' s p = do
  parserTrace' s
  p <|> trace ("core.parser." ++ s) "backtracked" (fail s)

{-|
Trace for a parser

TODO go back to removing it when not in debug mode for speed, even though order might change?
-}
debugParser :: Show a => String -> ParseM a -> ParseM a
debugParser name pFn = parserTraced' name pFn
