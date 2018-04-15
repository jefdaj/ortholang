module ShortCut.Core.Parse.Expr where

import ShortCut.Core.Types
import ShortCut.Core.Pretty()
import ShortCut.Core.Parse.Basic

import qualified Text.Parsec.Expr as E
-- import Text.PrettyPrint.HughesPJClass (render, pPrint)

import Control.Applicative    ((<|>))
import Control.Monad          (void)
import Control.Monad.Identity (Identity)
import Data.List              (find, union)
-- import ShortCut.Core.Debug    (debugParser)
-- import ShortCut.Core.Util     (nonEmptyType)
import Text.Parsec            (try, getState, (<?>))
import Text.Parsec.Char       (string)
import Text.Parsec.Combinator (manyTill, eof, lookAhead, between, choice, sepBy)
-- import Data.Either (either)

import Debug.Trace

-- TODO how hard would it be to get Haskell's sequence notation? would it be useful?
-- TODO once there's [ we can commit to a list, right? should allow failing for real afterward
pList :: ParseM CutExpr
pList = do
  (_, cfg, _) <- getState
  terms <- between (pSym '[') (pSym ']')
                   (sepBy pExpr (pSym ','))
  let eType = nonEmptyType $ map typeOf terms
      deps  = if null terms then [] else foldr1 union $ map depsOf terms
  res <- case eType of
    Left err -> fail err
    Right t  -> return $ CutList t 0 deps terms
  let res' = debugParser cfg "pList" res
  return res'

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
    binary f = E.Infix (pBop f) E.AssocLeft
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
-- TODO how to fail gracefully (with fail, not error) here??
pBop :: CutFunction -> ParseM (CutExpr -> CutExpr -> CutExpr)
pBop bop
  | fFixity bop == Infix = pSym (head $ fName bop) *> (return $ \e1 e2 ->
    let deps  = union (depsOf e1) (depsOf e2)
    in case (fTypeCheck bop) [typeOf e1, typeOf e2] of
      Left  msg -> error msg -- TODO can't `fail` because not in monad here?
      Right rtn -> CutBop rtn 0 deps (fName bop) e1 e2)
pBop _ = error "pBop only works with infix functions"

---------------
-- functions --
---------------

-- This is a kludge to make my "interesting" preference for spaces as function
-- application work right. It's used to test whether we've reached the end of a
-- list of arguments for the function currently being parsed.
-- TODO can factor the try out to be by void right?
pEnd :: ParseM ()
pEnd = do
  (_, cfg, _) <- getState
  res <- lookAhead $ void $ choice
    [ eof
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
  (_, cfg, _) <- getState
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
  (_, cfg, _) <- getState
  name <- try $ pName
  args <- manyTill pTerm pEnd
  let fns  = concat $ map mFunctions $ cfgModules cfg
      fn   = find (\f -> fName f == name) fns
      deps = foldr1 union $ map depsOf args
  case fn of
    Nothing -> fail $ trace ("invalid fn name: " ++ name) name
    -- Just f  -> let check = (fTypeCheck $ trace ("found fn: " ++ fName f) f) (map typeOf args)
    --            in case debugParser cfg "pFun typechecker" check of
    -- Just f  -> case (fTypeCheck f) (map typeOf args) of
    Just f  -> case typeCheckArgs f args of
                 Left  err -> fail err
                 Right rtn -> let res  = CutFun rtn 0 deps (fName f) args
                                  res' = debugParser cfg "pFun" res
                              in return res'

typeCheckArgs :: CutFunction -> [CutExpr] -> Either String CutType
typeCheckArgs fn args = trace ("return type: " ++ show rtn) rtn
  where
    fn'    = trace ("typechecking fn: " ++ fName fn) fn
    args'  = trace ("args: " ++ unwords (map show args)) args
    types  = map typeOf args'
    types' = trace ("types: " ++ show types) types
    rtn    = fTypeCheck fn' types'

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
pTerm = choice [pList, pParens, pNum, pStr, pFun, pRef]

-- This function automates building complicated nested grammars that parse
-- operators correctly. It's kind of annoying, but I haven't figured out how
-- to do without it. Also it seems like it will get more useful if I want to
-- add non-assignment statements like assertions. See:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: ParseM CutExpr
pExpr = do
  (_, cfg, _) <- getState
  E.buildExpressionParser (operatorTable cfg) pTerm <?> "expression"
