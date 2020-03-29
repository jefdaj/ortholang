{-|
Overall script parsing strategy:

0. Take an initial script, because we may be parsing an import

1. Strip comments

2. Recursively parse imported scripts

3. Pass the rest of the string to the existing parser
   (which can be simplified by removing comment code)
-}

module OrthoLang.Core.Parse.Script
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

import Debug.Trace

import OrthoLang.Core.Parse.Basic
import OrthoLang.Core.Parse.Expr
import OrthoLang.Core.Parse.Util
import OrthoLang.Core.Types
import OrthoLang.Core.Paths (scriptDigests)

import Control.Applicative    ((<|>), many)
import Control.Monad          (when)
import Data.List              (partition)
import Data.List.Utils        (hasKeyAL)
import OrthoLang.Core.Util    (readFileStrict, debug)
import System.FilePath        ((</>), takeDirectory)
import Text.Parsec            (try)
import Text.Parsec.Char       (newline, spaces)
import Text.Parsec.Combinator (optional)

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
stripResult scr = scr {sAssigns = filter notRes $ sAssigns scr}
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
  cfg <- getConfig
  scr <- getScript
  v@(Var _ vName) <- (try (optional newline *> pVarEq))
  when ((not $ cfgInteractive cfg) && (hasKeyAL v $ sAssigns scr) && (vName /= "result")) $ do
    fail $ "duplicate variable '" ++ vName ++ "'"
  e <- lexeme pExpr
  -- putAssign  "pAssign" (v, e) -- TODO is this covered by returning it?
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
parseFileIO :: GlobalEnv -> FilePath -> IO Script
parseFileIO st scr = do
  mscr1 <- parseFile st scr
  case mscr1 of
    Left  e -> fail $ show e
    Right s -> return s

-- TODO need GlobalEnv here? or just Config?
parseStatement :: ParseEnv -> String -> Either String Assign
parseStatement = parseWithEof pStatement

{-|
The name doesn't do a good job of explaining this, but it's expected to be
parsing an entire script from a string (no previous state).

TODO clarify that

TODO error if it has leftover?
-}
parseString :: Config -> String -> Either String Script
parseString c = parseWithEof pScript (c, emptyScript)

{-|
Not sure why this should be necessary, but it was easier than fixing the parser
to reject multiple results.

TODO one result *per repeat ID*, not total!
-}
lastResultOnly :: Script -> Script
lastResultOnly scr@(Script {sAssigns = as}) = scr {sAssigns = otherVars ++ [lastRes]}
  where
    (resVars, otherVars) = partition (\(v, _) -> v == Var (RepID Nothing) "result") as
    -- lastRes = trace ("resVars: " ++ show resVars) $ last resVars -- should be safe because we check for no result separately?
    lastRes = last resVars -- should be safe because we check for no result separately?

{-|
This one is special because if it parses it replaces the whole script in
ParseEnv, and if not the original script should remain unaltered.

TODO message in case it doesn't parse?

TODO should it get automatically `put` here, or manually in the repl?
-}
pScript :: ParseM Script
pScript = debugParser "pScript" $ do
  optional spaces
  as <- many pStatement
  -- (cfg, scr) <- getState
  -- scr <- getScript
  -- let (as, ds) = unzip ads 
  -- let ds'  = M.union (sDigests scr) $ exprDigests cfg scr $ map snd as
      -- scr  = emptyScript {sAssigns = as, sDigests = ds'}
  cfg <- getConfig
  let scr  = emptyScript {sAssigns = as}
      scr' = lastResultOnly scr
      ds   = scriptDigests cfg scr'
      scr'' = scr' {sDigests = trace ("pScript ds: " ++ show ds) ds}
  -- putScript scr'
  -- putDigests "pScript" $ map snd as -- TODO is this the only place it needs to be done?
  putScript scr''
  return scr''
  -- return $ trace (unlines $ map show $ M.toList ds') scr' -- TODO remove

-- TODO could generalize to other parsers/checkers like above for testing
-- TODO is it OK that all the others take an initial script but not this?
-- TODO should we really care what the current script is when loading a new one?
parseFile :: GlobalEnv -> FilePath -> IO (Either String Script)
parseFile (_, cfg, ref, _) path = do
  debug "core.parse.script.parseFile" $ "parseFile '" ++ path ++ "'"
  txt <- readScriptWithIncludes ref path
  return $ (parseString cfg . stripComments) txt
