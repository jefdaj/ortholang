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
  , runParseM
  )
  where

import OrthoLang.Core.Types
import qualified Text.Parsec as P

import Development.Shake.FilePath (makeRelative)
import OrthoLang.Core.Util        (trace)
import Text.Parsec                (ParseError)
import Text.Parsec.Combinator     (manyTill, eof, anyToken)

-- TODO make an empty GlobalEnv so you can run these in ghci again

-- TODO is this ever needed in production? probably not
parseAndShow :: (Show a) => ParseM a -> ParseEnv -> String -> String
parseAndShow p s str' = case runParseM p s str' of
  Left err -> show err
  Right s2 -> show s2

-- TODO adjust this to fail when there's extra text off the end of the line!
runParseM :: ParseM a -> ParseEnv -> String -> Either ParseError a
runParseM p s@(cfg, _) = P.runParser p s desc
  where
    desc = case cfgScript cfg of
             Nothing -> "repl"
             Just f  -> makeRelative (cfgWorkDir cfg) f

parseWithLeftOver :: ParseM a -> ParseEnv -> String -> Either ParseError (a,String)
parseWithLeftOver p s = runParseM ((,) <$> p <*> leftOver) s
  where
    leftOver = manyTill anyToken eof

parseWithEof :: ParseM a -> ParseEnv -> String -> Either ParseError a
parseWithEof p s = runParseM (p <* eof) s
