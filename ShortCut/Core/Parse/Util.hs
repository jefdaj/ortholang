module ShortCut.Core.Parse.Util where

import ShortCut.Core.Types
import ShortCut.Core.Util (trace)

import qualified Text.Parsec as P
import Text.Parsec                (ParseError)
import Text.Parsec.Combinator     (manyTill, eof, anyToken)
import Development.Shake.FilePath (makeRelative)

-- TODO make an empty CutState so you can run these in ghci again

-- Some are from the Parsec tutorial here:
-- https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing

-- TODO is this ever needed in production? probably not
parseAndShow :: (Show a) => ParseM a -> CutState -> String -> String
parseAndShow p s str' = case runParseM p s str' of
  Left err -> show err
  Right s2 -> show s2

-- TODO adjust this to fail when there's extra text off the end of the line!
runParseM :: ParseM a -> CutState -> String -> Either ParseError a
runParseM p s@(_, cfg, _, _) = P.runParser p s desc
  where
    desc = case cfgScript cfg of
             Nothing -> "repl"
             Just f  -> makeRelative (cfgWorkDir cfg) f

parseWithLeftOver :: ParseM a -> CutState -> String -> Either ParseError (a,String)
parseWithLeftOver p s = runParseM ((,) <$> p <*> leftOver) s
  where
    leftOver = manyTill anyToken eof

parseWithEof :: ParseM a -> CutState -> String -> Either ParseError a
parseWithEof p s = runParseM (p <* eof) s
