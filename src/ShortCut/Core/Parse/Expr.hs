module ShortCut.Core.Parse.Expr where

import ShortCut.Core.Types
import ShortCut.Core.Parse.Basic

import qualified Text.Parsec.Expr as E
import Text.PrettyPrint.HughesPJClass (render, pPrint)

import Control.Applicative    ((<|>))
import Control.Monad          (void, fail, when)
import Control.Monad.Identity (Identity)
import Data.List              (find, union)
import ShortCut.Core.Debug    (debugParser)
import Text.Parsec            (try, getState, (<?>))
import Text.Parsec.Char       (string)
import Text.Parsec.Combinator (optional, manyTill, eof, lookAhead, between,
                               choice, sepBy)

-- TODO how hard would it be to get Haskell's sequence notation? would it be useful?
-- TODO once there's [ we can commit to a set, right? should allow failing for real afterward
pSet :: ParseM CutExpr
pSet = do
  terms <- between (pSym '[') (pSym ']')
                   (sepBy pExpr (pSym ',' <* optional pComment))
  let deps = if null terms
               then []
               else foldr1 union $ map depsOf terms
      rtn = if null terms
               then EmptyList
               else typeOf $ head terms
  when (not $ all (== rtn) (map typeOf terms))
    (fail "all elements of a set must have the same type")
  return $ CutList rtn 0 deps terms

---------------
-- operators --
---------------

-- TODO load from modules somehow... or do individual fn parsers make it obsolete?
-- operatorChars :: [Char]
-- operatorChars = "+-*/&|~"
operatorChars :: CutConfig -> [Char]
operatorChars cfg = concat $ map fName $ filter (\f -> fFixity f == Infix)
                  $ concat $ map mFunctions $ cfgModules cfg

-- for now, I think all binary operators at the same precedence should work.
-- but it gets more complicated I'll write out an actual table here with a
-- prefix function too etc. see the jake wheat tutorial
operatorTable :: CutConfig -> [[E.Operator String CutState Identity CutExpr]]
operatorTable cfg = [map binary bops]
  where
    binary f = E.Infix (pBop $ fName f) E.AssocLeft
    bops = filter (\f -> fFixity f == Infix) (concat $ map mFunctions mods)
    mods = cfgModules cfg

-- Tricky bit: needs to take two already-parsed expressions
-- TODO verify they have the correct types
-- TODO is this obsolete now that there's pBop?
-- pBop :: Char -> ParseM (CutExpr -> CutExpr -> CutExpr)
-- pBop o = pSym o *> (return $ \e1 e2 ->
--   let deps = union (depsOf e1) (depsOf e2)
--   in CutBop (typeOf e1) deps [o] e1 e2)

-- TODO is there a better way than only taking one-char strings?
-- TODO check that typeOf e1 == typeOf e2 (error or skip on failure?)
pBop :: String -> ParseM (CutExpr -> CutExpr -> CutExpr)
pBop [o] = pSym o *> (return $ \e1 e2 ->
  let deps = union (depsOf e1) (depsOf e2)
      (t1, t2) = case (typeOf e1, typeOf e2) of
                   (EmptyList, b) -> (b, b)
                   (a, EmptyList) -> (a, a)
                   (a, b) -> (a, b)
      rtn  = if t1 == t2 then t1 else error $
               o:" operator requires two arguments of the same type, but got:\n"
                ++ "\t'" ++ render (pPrint e1) ++ "' (" ++ show t1 ++ ")\n"
                ++ "\t'" ++ render (pPrint e2) ++ "' (" ++ show t2 ++ ")"
  in CutBop rtn 0 deps [o] e1 e2)
pBop  s  = fail $ "invalid binary op name '" ++ s ++ "'"

---------------
-- functions --
---------------

-- This is a kludge to make my "interesting" preference for spaces as function
-- application work right. It's used to test whether we've reached the end of a
-- list of arguments for the function currently being parsed.
-- TODO can factor the try out to be by void right?
pEnd :: ParseM ()
pEnd = do
  (_, cfg) <- getState
  res <- lookAhead $ void $ choice
    [ eof
    , pComment -- TODO shouldn't this not end it?
    , void $ try $ choice $ map pSym $ operatorChars cfg ++ ")],"
    , void $ try pVarEq
    ]
  let res' = debugParser cfg "pEnd" res
  return res'

-- TODO load names from modules, of course
-- TODO put this in terms of "keyword" or something?
-- TODO get function names from modules
pName :: ParseM String
pName = do
  (_, cfg) <- getState
  (choice $ map (try . str') $ fnNames cfg) <?> "fn name"
  where
    str' s = string s <* (void spaces1 <|> eof)

-- TODO move to types? utils?
fnNames :: CutConfig -> [String]
fnNames cfg = map fName $ concat $ map mFunctions $ cfgModules cfg

-- TODO any way to do this last so "function not found" error comes through??
-- TODO should be able to commit after parsing the fn name, which would allow real failure
pFun :: ParseM CutExpr
pFun = do
  (_, cfg) <- getState
  name <- try $ pName
  void $ optional pComment
  args <- manyTill pTerm pEnd
  let fns  = concat $ map mFunctions $ cfgModules cfg
      fn   = find (\f -> fName f == name) fns
      deps = foldr1 union $ map depsOf args
  case fn of
    Nothing -> fail name
    Just f  -> case (fTypeCheck f) (map typeOf args) of
      Left  err -> fail err
      Right rtn -> let res  = CutFun rtn 0 deps (fName f) args
                       res' = debugParser cfg "pFun" res
                   in return res'

-----------------
-- expressions --
-----------------

pParens :: ParseM CutExpr
pParens = between (pSym '(') (pSym ')') pExpr <?> "parens"

-- TODO need to commit to separate branches so Parsec doesn't try to parse
--      other stuff after substitute_each fails. the rule should be:
--      once a fn/macro name is parsed it commits to that branch
--      if none of them work it moves on to others
--      without that we get silly errors like "no such variable" for any of them!
pTerm :: ParseM CutExpr
pTerm = choice [pSet, pParens, pNum, pStr, pFun, pRef] <* optional pComment

-- This function automates building complicated nested grammars that parse
-- operators correctly. It's kind of annoying, but I haven't figured out how
-- to do without it. Also it seems like it will get more useful if I want to
-- add non-assignment statements like assertions. See:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: ParseM CutExpr
pExpr = do
  (_, cfg) <- getState
  optional pComment
  E.buildExpressionParser (operatorTable cfg) pTerm <?> "expression"