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
    readScriptWithIncludes
  , stripQuotes

  -- * Statements
  -- , stripNaked
  , pAssign
  , pNaked
  , pStatement

  -- * Scripts
  , parseFileIO
  , parseStatement
  , parseString
  -- , lastResultOnly
  , pScript
  , parseFile
  )
  where

import OrthoLang.Debug
import OrthoLang.Interpreter.Parse.Basic
import OrthoLang.Interpreter.Parse.Expr
import OrthoLang.Interpreter.Parse.Util (debugParser, parseWithEof, stripComments)
import OrthoLang.Types
import OrthoLang.Script (appendStatementRepl, appendStatementFile)
-- import OrthoLang.Interpreter.Paths (scriptDigests)

import Control.Applicative    ((<|>), many)
import Control.Monad          (when)
-- import Data.List              (partition)
-- import Data.List.Utils        (hasKeyAL)
import OrthoLang.Util    (readFileStrict)
import System.FilePath        ((</>), takeDirectory)
import Text.Parsec            (try, getState, putState)
import Text.Parsec.Char       (newline, spaces)
import Text.Parsec.Combinator (optional)
-- import Control.Monad.Reader   (ask)

-------------------
-- preprocessing --
-------------------

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

-- stripNaked :: Script -> Script
-- stripNaked scr = scr {sAssigns = filter notRes $ sAssigns scr}
  -- where
    -- notRes (Assign {aVar = Var _ "result"}) = False
    -- notRes _ = True

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

  -- I can't think of any obvious reason a user would need to define the same
  -- variable twice in a written script, so it's prohibited here. But that's
  -- just erring on the safe/simple side so if you have an idea for a design
  -- pattern that requires it, reach out by email!
  --
  -- REPL sessions, on the other hand, might include lots of redefinition. We
  -- can handle that by asking the user to manually confirm that they're OK
  -- dropping anything that depends on the old definition when a variable
  -- changes type.
  --
  -- TODO in either case, prevent recursive self-references!
  cfg <- askConfig
  when (hasVar v (sAssigns scr) && vName /= "result") $ do
    when (not $ interactive cfg) $ fail $ "duplicate variable \"" ++ vName ++ "\"" -- TODO word this better

  e <- lexeme pExpr

  -- let asn = Assign {aVar = v, aExpr = e}
  -- putAssign "pAssign" asn
  -- putDigests "pAssign" [e]
  let assign = Assign {aVar = v, aExpr = e}
      scr' = if interactive cfg
               then appendStatementRepl scr $ Right assign
               else appendStatementFile scr assign
  putState scr'
  return assign

{-|
Handles the special case of a naked top-level expression, which is treated as
being assigned to "result". This parses the same in a script or the repl.

TODO Prevent assignments that include the variable being assigned to
     (later when working on statement issues).

TODO If the statement is literally `result`, what do we do?
     Maybe we need a separate type of assignment statement for this?
-}
pNaked :: ParseM Assign
pNaked = debugParser "pNaked" $ do
  e   <- pExpr
  -- let rv = Var (RepID Nothing) "result" -- TODO is it always Nothing?
  --     ra = Assign {aVar = rv, aExpr = e}
  -- putAssign "pNaked" ra
      -- scr' = scr {sAssigns = delVar (sAssigns scr) rv, sResult = Just e}
  cfg <- askConfig
  when (not $ interactive cfg) $ fail "naked expression in script" -- TODO better error here
  scr <- getState
  let scr' = appendStatementRepl scr (Left e) -- Left is not an error here
      ra   = Assign {aVar = Var (RepID Nothing) "result", aExpr = e}
  putState scr'
  return ra

pStatement :: ParseM Assign
pStatement = debugParser "pStatement" (try pAssign <|> pNaked)

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
-- lastResultOnly :: Script -> Script
-- lastResultOnly s = s {sAssigns = otherVars ++ [lastRes]}
--   where
--     (resVars, otherVars) = partition (\a -> aVar a == Var (RepID Nothing) "result") (sAssigns s)
--     -- lastRes = trace ("resVars: " ++ show resVars) $ last resVars -- should be safe because we check for no result separately?
--     lastRes = last resVars -- should be safe because we check for no result separately?

{-|
This one is special because if it parses it replaces the whole script in
(Config, Script), and if not the original script should remain unaltered.

TODO message in case it doesn't parse?

TODO should it get automatically `put` here, or manually in the repl?
-}
pScript :: ParseM Script
pScript = debugParser "pScript" $ do
  optional spaces
  _ <- many pStatement
  scr <- getState
  -- let scr' = scr {sAssigns = as} -- TODO is this redundant?
  -- putState scr'
  return scr

-- TODO could generalize to other parsers/checkers like above for testing
-- TODO is it OK that all the others take an initial script but not this?
-- TODO should we really care what the current script is when loading a new one?
parseFile :: [Module] -> GlobalEnv -> FilePath -> IO (Either String Script)
parseFile mods (_, cfg, ref, _, _) path = do -- only passed on, so modules could be passed here instead?
  debug "core.parse.script.parseFile" $ "parseFile \"" ++ path ++ "\""
  txt <- readScriptWithIncludes ref path
  return $ (parseString mods cfg . stripComments) txt
