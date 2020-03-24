module OrthoLang.Core.Parse.Script where

import OrthoLang.Core.Util (readFileStrict, debug)
import OrthoLang.Core.Types
import OrthoLang.Core.Parse.Util
import OrthoLang.Core.Parse.Basic
import OrthoLang.Core.Parse.Expr

import Control.Applicative    ((<|>), many)
import System.FilePath        ((</>), takeDirectory)
import Text.Parsec            (ParseError, try, getState, putState)
import Text.Parsec.Char       (newline, spaces)
import Text.Parsec.Combinator (optional)
import Control.Monad          (when)
import Data.List              (partition)
import Data.List.Utils        (hasKeyAL)

-------------------
-- preprocessing --
-------------------

{- New overall script parse idea:
 -
 - 0. take an initial script, because may be parsing an import
 - 1. strip comments
 - 2. recursively parse imported scripts
 - 3. pass the rest of the string to the existing parser
 -    (which can be simplified by removing comment code)
 -}

-- untested, but should strip all comments from a script leaving whitespace
-- TODO could this be lobbing off the first char?
stripComments :: String -> String
stripComments = unlines . map stripComment . lines
  where
    stripComment = takeWhile (/= '#')

-- TODO make this parser nicer like the others? or leave simple like other languages?
readScriptWithIncludes :: Locks -> FilePath -> IO String
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

stripResult :: OrthoLangScript -> OrthoLangScript
stripResult scr = filter notRes scr
  where
    notRes ((OrthoLangVar _ "result"), _) = False
    notRes _ = True

-- TODO combine pVar and pVarEq somehow to reduce try issues?

-- TODO message in case it doesn't parse?
-- TODO should reject duplicate variables! except replace result
pAssign :: ParseM OrthoLangAssign
pAssign = debugParser "pAssign" $ do
  (scr, cfg, ref, ids) <- getState
  -- optional newline
  -- void $ lookAhead $ debugParser "first pVarEq" pVarEq
  v@(OrthoLangVar _ vName) <- (try (optional newline *> pVarEq)) -- TODO use lookAhead here to decide whether to commit to it
  when ((not $ cfgInteractive cfg) && (hasKeyAL v scr) && (vName /= "result")) $ fail $ "duplicate variable '" ++ vName ++ "'"
  e <- (lexeme pExpr)
  -- let scr' = case vName of
               -- -- "result" -> trace "stripping old result" $ stripResult scr ++ [(v, e)]
               -- "result" -> stripResult scr ++ [(v, e)]
               -- -- _ -> trace "this is not a result assignment" $ scr ++ [(v,e)]
               -- _ -> scr ++ [(v,e)]
  let scr' = scr ++ [(v, e)]
  -- trace ("got past scr' with scr: " ++ show scr ++ " -> " ++ show scr') $ putState (scr', cfg, ref, ids)
  putState (scr', cfg, ref, ids)
  -- debugParseM $ "assigned var " ++ show v
  let res  = (v,e)
      -- res' = debugParser cfg "pAssign" res
  -- return $ traceShow scr' res
  return res

-- Handles the special case of a naked top-level expression, which is treated
-- as being assigned to "result". This parses the same in a script or the repl.
-- TODO prevent assignments that include the variable being assigned to
--      (later when working on statement issues)
-- TODO if the statement is literally `result`, what do we do?
--      maybe we need a separate type of assignment statement for this?
pResult :: ParseM OrthoLangAssign
pResult = debugParser "pResult" $ do
  e <- pExpr
  let res = (OrthoLangVar (ReplaceID Nothing) "result", e)
  return res

pStatement :: ParseM OrthoLangAssign
pStatement = debugParser "pStatement" (try pAssign <|> pResult)
  -- (_, cfg) <- getState
  -- res <- pAssign <|> pResult
  -- let res' = debugParser cfg "pStatement" res
  -- return res
  --

-------------
-- scripts --
-------------

-- TODO move to a separate "files/io" module along with some debug fns?
parseFileIO :: OrthoLangState -> FilePath -> IO OrthoLangScript
parseFileIO st scr = do
  mscr1 <- parseFile st scr
  case mscr1 of
    Left  e -> fail $ show e
    Right s -> return s

-- TODO need OrthoLangState here? or just OrthoLangConfig?
parseStatement :: OrthoLangState -> String -> Either ParseError OrthoLangAssign
parseStatement = parseWithEof pStatement

-- The name doesn't do a good job of explaining this, but it's expected to be
-- parsing an entire script from a string (no previous state).
-- TODO clarify that
-- TODO error if it has leftover?
parseString :: OrthoLangConfig -> Locks -> HashedIDsRef -> String
            -> Either ParseError OrthoLangScript
parseString c r ids = parseWithEof pScript ([], c, r, ids)

-- TODO add a preprocessing step that strips comments + recurses on imports?

-- Not sure why this should be necessary, but it was easier than fixing the parser to reject multiple results.
-- TODO one result *per repeat ID*, not total!
lastResultOnly :: OrthoLangScript -> OrthoLangScript
lastResultOnly scr = otherVars ++ [lastRes]
  where
    (resVars, otherVars) = partition (\(v, _) -> v == OrthoLangVar (ReplaceID Nothing) "result") scr
    -- lastRes = trace ("resVars: " ++ show resVars) $ last resVars -- should be safe because we check for no result separately?
    lastRes = last resVars -- should be safe because we check for no result separately?

-- TODO message in case it doesn't parse?
-- TODO should it get automatically `put` here, or manually in the repl?
pScript :: ParseM OrthoLangScript
pScript = debugParser "pScript" $ do
  (_, cfg, ref, ids) <- getState
  optional spaces
  scr <- many pStatement
  let scr' = lastResultOnly scr
  putState (scr', cfg, ref, ids)
  return scr'

-- TODO could generalize to other parsers/checkers like above for testing
-- TODO is it OK that all the others take an initial script but not this?
-- TODO should we really care what the current script is when loading a new one?
parseFile :: OrthoLangState -> FilePath -> IO (Either ParseError OrthoLangScript)
parseFile st@(_, cfg, ref, ids) path = do
  debug "core.parse.script.parseFile" $ "parseFile '" ++ path ++ "'"
  txt <- readScriptWithIncludes ref path
  return $ (parseString cfg ref ids . stripComments) txt
