module OrthoLang.Interpreter.Parse.Basic
  (
  -- * Utilities
    putAssign
  , assign

  -- * Whitespace and endings
  , lexeme
  , spaceChars
  , spaces1
  , pEnd

  -- * Misc utilities
  , escapeChars
  , literalChars
  , pSym

  -- * Variable assignment
  , pIden
  , pVar
  , pVarOnly
  , pEq
  , pVarEq

  -- * Literals
  , pNum
  , pQuoted
  , pStr
  )
  where

-- TODO hold up, is Logging missing a bunch of NOINLINE statements?

import OrthoLang.Debug (trace)
import OrthoLang.Types
import qualified Data.Map.Strict as M

import Control.Applicative    ((<|>), many)
import Control.Monad          (void, fail)
import Data.Char              (isPrint)
import Data.Scientific        (Scientific())
import Text.Parsec            (getState, putState, (<?>), try)
import Text.Parsec.Char       (char, digit ,letter, spaces, oneOf)
import Text.Parsec.Combinator (many1, between, notFollowedBy, choice, lookAhead, eof, optionMaybe, anyToken)
import Text.Parsec.Prim       (ParsecT, Stream)
import OrthoLang.Interpreter.Parse.Util (debugParser)
import Control.Monad.Reader   (ask)

import Text.PrettyPrint               (Doc, (<>), (<+>), render)
import Data.Scientific                (Scientific(), toBoundedInteger)

--------------
-- utilites --
--------------

-- TODO remove the first string (parser name)? or use it to debug
putAssign :: String -> Assign -> ParseM ()
putAssign _ a = do
  scr <- getState
  -- let as' = (if null (sResult scr) then (sAssigns scr) else delVar (sAssigns scr) "result") ++ [a]
  --     re' =  if null (sResult scr) || vName == "result"
  --              then Just (aExpr a)
  --              else (sResult scr)
  --     scr' = scr {sAssigns = as', sResult = re'}
  let scr' = assign scr $ trace "interpreter.parse.basic.putAssign" ("a: " ++ show a) a
  putState scr'

-- TODO move, but where?
-- Ref Type (Maybe Salt) [Var] Var -- do refs need a salt? yes! (i think?)
-- TODO salt is Nothing rather than the expr's salt, right?
-- TODO var is added to deps, right?
assign :: Script -> Assign -> Script
assign scr a@(Assign var@(Var _ vName) expr) =

  -- let rv  = Var (RepID Nothing) "result"
      -- rr  = Ref (typeOf expr) Nothing (depsOf expr) var 
      -- ra  = Assign rv rr
  -- let as' = delVar (sAssigns scr) vName ++ [a]
  --     r'  = if null (sResult scr) || vName == "result"
  --             then Just expr 
  --             else sResult scr
  --     scr' = Script {sAssigns = as', sResult = r'}

  -- OK, so by example the behavior we actually wanted instead of this:
  --
  -- result = load_fna "https://molb7621.github.io/workshop/_downloads/sample.fa"
  -- samplefa = result
  --
  -- was:
  -- 1) substitute the result ref for its value in the old script
  -- 2) strip the "result" assignment from the old script
  -- 3) add the new assignment statement samplefa = load_fna ...
  -- 4) assigned a new default result = samplefa
  --
  -- How do we know that?
  --
  -- we should always do (1)
  -- if interactive we should also always do (2), but respect the earlier "result = " in written scripts?
  -- always do (3)
  -- and always do (4) unless the current assignment was explicitly "result = " already

  -- the next bug:
  --
  -- ortholang —▶ 1 # comments after naked expressions should be ignored
  -- 1
  -- 
  -- ortholang —▶ :show
  -- result = 1
  -- 
  -- ortholang —▶ test = 1 # same with comments after assignment statements
  -- ortholang —▶ :show
  -- result = 1
  -- test = 1
  -- 
  -- ## only problem is it should remove "result = 1" and insert "result = test" at the end
  -- TODO try stripping old result and see if that fixes it
  -- TODO if not, try also inserting the new one at the end

  -- Seems like the issue here is implicit vs explicit result? When assigned
  -- explicitly in a script, result should persist after more assignments. But
  -- when only assigned implicitly it should be overwritten, and when in the
  -- REPL it should always be overwritten. When saving a specific REPL var it
  -- should be set added to the end of the script file.
  --
  -- The simplest way to capture this in types, though maybe not the cleanest,
  -- seems to be to prioritize a "result" var in sAssigns over sResult, and
  -- have sResult be optional. Then parsing behavior should vary between file and repl:
  -- in repl, assign each naked expression to sResult. but in a file, naked expressions should be errors
  -- in a file, never remove previous result vars, except during includes
  -- in a file, sResult isn't set at all until it holds a ref to the result var in sAssigns
  --
  -- does only the repl version need the fancy remove-self-references logic?

  let scr'  = scr {sAssigns = delVar (sAssigns scr) "result"}
      scr'' = updateVars scr' a
  in trace "interpreter.parse.basic.assign"
           ("old scr:\n" ++ render (pPrint scr) ++ "\nnew scr'':\n" ++ render (pPrint scr''))
           scr''

{-|
There's a convention in parsers that each one should consume whitespace after
itself (handled by this function), and you only skip leading whitespace at the
top level. That way all the whitespace gets skipped exactly once. (I guess it
would work the other way around too)
-}
lexeme :: ParseM a -> ParseM a
lexeme p = p <* spaces

spaceChars :: [Char]
spaceChars = " \t\n"

{-|
This is like spaces, except it requires at least one.

TODO can this be replaced with something from Text.Parsec.Token?
-}
spaces1 :: ParseM ()
spaces1 = debugParser "spaces1" ((void $ many1 $ oneOf spaceChars) <?> "whitespace and/or newline")

pSym :: Char -> ParseM ()
pSym c = debugParser ("pSym " ++ [c]) (void $ lexeme $ char c) <?> "symbol \"" ++ [c] ++ "\""

-----------------
-- identifiers --
-----------------

{-|
This is a kludge to make my preference for Haskell-style function application
with spaces work. It's used to test whether we've reached the end of a list of
arguments for the function currently being parsed. Probably very slow.

TODO can factor the try out to be by void right?

TODO error in here when it somehow succeeds on full7942?

TODO this must be succeding on 'loaner... right?
-}
pEnd :: ParseM ()
pEnd = debugParser "pEnd" $ do
  mods <- askModules
  try $ lookAhead $ choice
    [ eof
    , void $ choice $ map pSym $ operatorChars mods ++ ")],"
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

pVar :: ParseM Var
pVar = debugParser "pVar" (Var (RepID Nothing) <$> pIden)

-- TODO is the error in here?? maybe it consumes a space and therefore doesn't fail?
pEq :: ParseM ()
-- pEq = debugParser "pEq" ((void $ spaces <* pSym '=') <?> "equals sign")
pEq = debugParser "pEq" $ pSym '='

pVarEq :: ParseM Var
pVarEq = debugParser "pVarEq" ((pVar <* pEq) <?> "variable assignment")

pVarOnly :: ParseM Var
pVarOnly = debugParser "pVarOnly " (pVar <* notFollowedBy pEq)

--------------
-- literals --
--------------

-- TODO hey Scientific has its own parser, would it work to add?
pNum :: ParseM Expr
pNum = debugParser "pNum" $ do
  -- TODO optional minus sign here? see it doesn't conflict with subtraction
  -- TODO try this for negative numbers: https://stackoverflow.com/a/39050006
  neg <- try $ optionMaybe $ char '-'
  n  <- digit
  ns <- many (digit <|> oneOf ".e-")
  spaces
  -- read + show puts it in "canonical" form to avoid duplicate tmpfiles
  let sign = case neg of { Just x -> x; _ -> ' ' }
      l    = show (read (sign:n:ns) :: Scientific)
      expr = Lit num l 
  -- putDigests "pNum" [expr]
  return expr

-- list of chars which can be escaped in OrthoLang
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
pQuoted = debugParser "pQuoted" ((lexeme $ between (char '\"') (char '\"') $ many (l <|> esc)) <?> "quoted")
  where
    l = oneOf literalChars
    esc = char '\\' *> oneOf escapeChars

pStr :: ParseM Expr
pStr = debugParser "pStr" $ do
  expr <- Lit str <$> pQuoted <?> "string literal"
  -- putDigests "pStr" [expr]
  return expr
