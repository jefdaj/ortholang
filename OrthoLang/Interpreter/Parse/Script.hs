{-|
Overall script parsing strategy:

0. Take an initial script, because we may be parsing an import

1. Strip comments

2. Recursively parse imported scripts

3. Pass the rest of the string to the existing parser
   (which can be simplified by removing comment code)
-}

module OrthoLang.Interpreter.Parse.Script
  (
  -- * Preprocessing
    stripComments
  , readScriptWithIncludes
  , stripQuotes

  -- * Statements
  , stripResult
  , pAssign
  , pResult
  , pStatement

  -- * Scripts
  , parseFileIO
  , parseStatement
  , parseString
  , lastResultOnly
  , pScript
  , parseFile
  )
  where

import OrthoLang.Debug
import OrthoLang.Interpreter.Parse.Basic
import OrthoLang.Interpreter.Parse.Expr
import OrthoLang.Interpreter.Parse.Util (debugParser, parseWithEof)
import OrthoLang.Types
-- import OrthoLang.Interpreter.Paths (scriptDigests)

import Control.Applicative    ((<|>), many)
import Control.Monad          (when)
import Data.List              (partition)
import Data.List.Utils        (hasKeyAL)
import OrthoLang.Util    (readFileStrict)
import System.FilePath        ((</>), takeDirectory)
import Text.Parsec            (try, getState, putState)
import Text.Parsec.Char       (newline, spaces)
import Text.Parsec.Combinator (optional)
-- import Control.Monad.Reader   (ask)

-------------------
-- preprocessing --
-------------------

{-|
Strips all comments from a script, leaving whitespace.
-}
stripComments :: String -> String
stripComments = unlines . map stripComment . lines
  where
    stripComment = takeWhile (/= '#')

-- TODO make this parser nicer like the others? or leave simple like other languages?
readScriptWithIncludes :: LocksRef -> FilePath -> IO String
readScriptWithIncludes ref path = do
  txt <- readFileStrict ref path
  fmap unlines $ mapM processInclude $ lines txt
  where
    processInclude :: String -> IO String
    processInclude line = case words (stripComments line) of
                           ("include":relpath:_) ->
                             readScriptWithIncludes ref $ takeDirectory path </> (stripQuotes relpath)
                           _ -> return line

stripQuotes :: String -> String
stripQuotes s = dropWhile (== '\"') $ reverse $ dropWhile (== '\"') $ reverse s

----------------
-- statements --
----------------

stripResult :: Script -> Script
stripResult = filter notRes
  where
    notRes ((Var _ "result"), _) = False
    notRes _ = True

-- TODO combine pVar and pVarEq somehow to reduce try issues?

{-|
TODO message in case it doesn't parse?

TODO should reject duplicate variables! except replace result
-}
pAssign :: ParseM Assign
pAssign = debugParser "pAssign" $ do
  -- optional newline
  -- void $ lookAhead $ debugParser "first pVarEq" pVarEq
  -- TODO use lookAhead here to decide whether to commit to it?
  scr <- getState
  v@(Var _ vName) <- (try (optional newline *> pVarEq))
  -- "result" can be silently overwritten, but assigning another variable twice is an error
  -- TODO is that the best way to do it?
  when (hasKeyAL v scr && vName /= "result") $ do
    fail $ "duplicate variable \"" ++ vName ++ "\""
  e <- lexeme pExpr

  -- TODO actually, is *this* the only place it's needed rather than in pScript?
  putAssign  "pAssign" (v, e)
  -- putDigests "pAssign" [e]

  return (v, e)

{-|
Handles the special case of a naked top-level expression, which is treated as
being assigned to "result". This parses the same in a script or the repl.

TODO Prevent assignments that include the variable being assigned to
     (later when working on statement issues).

TODO If the statement is literally `result`, what do we do?
     Maybe we need a separate type of assignment statement for this?
-}
pResult :: ParseM Assign
pResult = debugParser "pResult" $ do
  e <- pExpr
  let res = (Var (RepID Nothing) "result", e)
  return res -- TODO is there any new result digest needed?

pStatement :: ParseM Assign
pStatement = debugParser "pStatement" (try pAssign <|> pResult)

-------------
-- scripts --
-------------

-- TODO move to a separate "files/io" module along with some debug fns?
parseFileIO :: [Module] -> GlobalEnv -> FilePath -> IO Script
parseFileIO mods st scr = do
  mscr1 <- parseFile mods st scr
  case mscr1 of
    Left  e -> fail $ show e
    Right s -> return s

-- TODO need GlobalEnv here? or just Config?
parseStatement :: [Module] -> Config -> Script -> String -> Either String Assign
parseStatement mods = parseWithEof mods pStatement

{-|
The name doesn't do a good job of explaining this, but it's expected to be
parsing an entire script from a string (no previous state).

TODO clarify that

TODO error if it has leftover?

-}
parseString :: [Module] -> Config -> String -> Either String Script
parseString ms c s = parseWithEof ms pScript c emptyScript s -- config not needed? pass modules here instead?
  -- where
  --   addDigests :: Script -> Script
  --   addDigests scr = scr {sDigests = scriptDigests c scr}

{-|
Not sure why this should be necessary, but it was easier than fixing the parser
to reject multiple results.

TODO one result *per repeat ID*, not total!
-}
lastResultOnly :: Script -> Script
lastResultOnly scr = otherVars ++ [lastRes]
  where
    (resVars, otherVars) = partition (\(v, _) -> v == Var (RepID Nothing) "result") scr
    -- lastRes = trace ("resVars: " ++ show resVars) $ last resVars -- should be safe because we check for no result separately?
    lastRes = last resVars -- should be safe because we check for no result separately?

{-|
This one is special because if it parses it replaces the whole script in
(Config, Script), and if not the original script should remain unaltered.

TODO message in case it doesn't parse?

TODO should it get automatically `put` here, or manually in the repl?
-}
pScript :: ParseM Script
pScript = debugParser "pScript" $ do
  optional spaces
  scr <- many pStatement
  -- cfg <- ask -- not used at all
  -- let scr  = emptyScript {sAssigns = as}
  let scr' = lastResultOnly scr
      -- scr'' = scr' {sDigests = trace ("pScript ds: " ++ show ds) ds}
  -- putState scr'
  -- putDigests "pScript" $ map snd as -- TODO is this the only place it needs to be done?
  putState scr'
  return scr'
  -- return $ trace (unlines $ map show $ M.toList ds') scr' -- TODO remove

-- TODO could generalize to other parsers/checkers like above for testing
-- TODO is it OK that all the others take an initial script but not this?
-- TODO should we really care what the current script is when loading a new one?
parseFile :: [Module] -> GlobalEnv -> FilePath -> IO (Either String Script)
parseFile mods (_, cfg, ref, _, _) path = do -- only passed on, so modules could be passed here instead?
  debug "core.parse.script.parseFile" $ "parseFile \"" ++ path ++ "\""
  txt <- readScriptWithIncludes ref path
  return $ (parseString mods cfg . stripComments) txt
