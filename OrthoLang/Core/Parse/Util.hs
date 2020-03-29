{-|
Parse utilities. Some are from the Parsec tutorial here:

<https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing>

Most aren't needed in production, but they help with debugging.
-}

module OrthoLang.Core.Parse.Util
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
  , ParseM
  , runParseM
  , parseFail
  )
  where

import OrthoLang.Core.Types
import qualified Text.Parsec as P

import Development.Shake.FilePath (makeRelative)
-- import OrthoLang.Core.Util        (trace)
import Text.Parsec.Combinator     (manyTill, eof, anyToken)
import OrthoLang.Core.Util    (trace)

-- for ParseM (new ParseM with errors)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Text.Parsec hiding (Empty)
-- import Text.Parsec.Combinator
-- import Text.Parsec.Char

-- TODO make an empty GlobalEnv so you can run these in ghci again

-- TODO is this ever needed in production? probably not
parseAndShow :: (Show a) => ParseM a -> ParseEnv -> String -> String
parseAndShow p s str' = case runParseM p s str' of
  Left err -> show err
  Right s2 -> show s2

-- TODO adjust this to fail when there's extra text off the end of the line!
-- runParseM :: ParseM a -> ParseEnv -> String -> Either ParseError a
-- runParseM p s@(cfg, _) = P.runParser p s desc
--   where
--     desc = case cfgScript cfg of
--              Nothing -> "repl"
--              Just f  -> makeRelative (cfgWorkDir cfg) f

parseWithLeftOver :: ParseM a -> ParseEnv -> String -> Either String (a,String)
parseWithLeftOver p s = runParseM ((,) <$> p <*> leftOver) s
  where
    leftOver = manyTill anyToken eof

parseWithEof :: ParseM a -> ParseEnv -> String -> Either String a
parseWithEof p s = runParseM (p <* eof) s

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

---------------------------------
-- attempt to add nicer errors --
---------------------------------

type ParseM a = ParsecT String ParseEnv (Except String) a

-- based on https://stackoverflow.com/a/54089987/429898
runParseM :: ParseM a -> ParseEnv -> String -> Either String a
runParseM op e@(cfg, _) input = case runExcept (runPT op e sn input) of
  Left s          -> Left s        -- parseFail; return the String
  Right (Left  e) -> Left (show e) -- Parsec error; convert to String
  Right (Right r) -> Right r
  where
    sn = case cfgScript cfg of
           Nothing -> "repl"
           Just f  -> makeRelative (cfgWorkDir cfg) f

parseFail :: String -> ParseM a
parseFail = lift . throwE
