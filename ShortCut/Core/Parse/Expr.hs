module ShortCut.Core.Parse.Expr where

import ShortCut.Core.Types
import ShortCut.Core.Pretty()
import ShortCut.Core.Parse.Basic


import qualified Text.Parsec.Expr as E
-- import Text.PrettyPrint.HughesPJClass (render, pPrint)

import Control.Applicative    ((<|>))
import Control.Monad          (void)
import Control.Monad.Identity (Identity)
import Data.List              (union)
-- import ShortCut.Core.Debug    (debugParser)
-- import ShortCut.Core.Util     (nonEmptyType)
import Text.Parsec            (try, getState, (<?>))
import Text.Parsec.Char       (string)
import Text.Parsec.Combinator (manyTill, eof, between, choice, sepBy)
-- import Data.Either (either)

-- import Debug.Trace (traceM)

-- TODO how hard would it be to get Haskell's sequence notation? would it be useful?
-- TODO once there's [ we can commit to a list, right? should allow failing for real afterward
pList :: ParseM CutExpr
pList = debugParser "pList" $ do
  terms <- between (pSym '[') (pSym ']')
                   (sepBy pExpr (pSym ','))
  let eType = nonEmptyType $ map typeOf terms
      deps  = if null terms then [] else foldr1 union $ map depsOf terms
  case eType of
    Left err -> fail err
    Right t  -> return $ CutList t initialRandomSeed deps terms

---------------
-- operators --
---------------

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
  | fFixity bop == Infix = (debugParser ("pBop " ++ fName bop) (pSym (head $ fName bop))) *> return (pBop' bop)
pBop _ = fail "pBop only works with infix functions"

pBop' :: CutFunction -> (CutExpr -> CutExpr -> CutExpr)
pBop' bop e1 e2 = case (fTypeCheck bop) [typeOf e1, typeOf e2] of
  Left  msg -> error msg -- TODO can't `fail` because not in monad here?
  Right rtn -> CutBop rtn initialRandomSeed (union (depsOf e1) (depsOf e2)) (fName bop) e1 e2

---------------
-- functions --
---------------

-- TODO load names from modules, of course
-- TODO put this in terms of "keyword" or something?
-- TODO get function names from modules
pFunName :: ParseM String
pFunName = do
  (_, cfg, _, _) <- getState
  (choice $ map (try . str') $ listFunctionNames cfg) <?> "fn name"
  where
    str' s = string s <* (void spaces1 <|> eof)

-- TODO any way to do this last so "function not found" error comes through??
-- TODO should be able to commit after parsing the fn name, which would allow real failure
pFun :: ParseM CutExpr
pFun = do
  name <- try pFunName -- TODO try?
  debugParseM $ "pFun committed to parsing " ++ name
  args <- pArgs
  debugParseM $ "pFun " ++ name ++ " args: " ++ show args
  pFunArgs name args

-- TODO main parse error is in here, when pTerm fails on an arg??
-- TODO so is pTerm failing on it, or pEnd succeeding?
pArgs :: ParseM [CutExpr]
pArgs = debugParser "pArgs" $ manyTill (try pTerm) pEnd

-- This function uses error rather than fail to prevent parsec from trying anything more
-- (TODO is there a better way?)
pFunArgs :: String -> [CutExpr] -> ParseM CutExpr
pFunArgs name args = debugParser "pFun" $ do
  (_, cfg, _, _) <- getState
  -- name <- try pFunName -- after this, we can commit to the fn and error on bad args
  -- args <- pArgs
  -- args <- manyTill pTerm pEnd
  let fns  = concat $ map mFunctions $ cfgModules cfg
      fns' = filter (\f -> fName f == name) fns
  case fns' of
    []      -> fail $ "no function found with name '" ++ name ++ "'"
    (fn:[]) -> typecheckArgs fn args -- TODO why no full7942??
    _       -> fail $ "function name collision! multiple fns match '" ++ name ++ "'"

-- A reference is just a variable name, but that variable has to be in the script.
-- TODO why does it fail after this, but only sometimes??
-- TODO main error is in here when if fails on a ref that clearly exists?
-- pRef :: String -> ParseM CutExpr
-- pRef var = debugParser "pRef" $ do
--
-- Since this is the last term parser, it can actually error instead of failing
pRef :: ParseM CutExpr
pRef = debugParser "pRef" $ do
  -- v@(CutVar var) <- pVarOnly
  v@(CutVar var) <- pVar
  -- let v = CutVar var
  (scr, _, _, _) <- getState
  debugParseM $ "scr before lookup of '" ++ var ++ "': " ++ show scr
  case lookup v scr of
    Nothing -> fail $ "no such variable '" ++ var ++ "'" ++ "\n" -- ++ show scr
    Just e -> return $ CutRef (typeOf e) initialRandomSeed (depsOf e) v

-- pFunOrRef :: ParseM CutExpr
-- pFunOrRef = debugParser "pFunOrRef" $ do
--   name <- try pIden -- TODO when it gets here, loader is cut off to oader
--   traceM $ "parsed name: " ++ name
--   (_, cfg, _, _) <- getState
--   if name `elem` fnNames cfg
--     then pFunArgs name
--     else pRef name

typecheckArgs :: CutFunction -> [CutExpr] -> ParseM CutExpr
typecheckArgs fn args = case (fTypeCheck fn) (map typeOf args) of
  Left  msg -> fail msg
  Right rtn -> let deps = foldr1 union $ map depsOf args
               in return $ CutFun rtn initialRandomSeed deps (fName fn) args

-----------------
-- expressions --
-----------------

pParens :: ParseM CutExpr
pParens = debugParser "pParens" (between (pSym '(') (pSym ')') pExpr <?> "parens")

-- TODO need to commit to separate branches so Parsec doesn't try to parse
--      other stuff after substitute_each fails. the rule should be:
--      once a fn/macro name is parsed it commits to that branch
--      if none of them work it moves on to others
--      without that we get silly errors like "no such variable" for any of them!
--
-- TODO main error is in here when pRef fails?

-- TODO hey should I actually just name this pArg?

pTerm :: ParseM CutExpr
-- pTerm = debugParser "pTerm" $ choice [pList, pParens, pNum, pStr, pFunOrRef]
pTerm = debugParser "pTerm" $ choice [pList, pParens, pNum, pStr, pFun, pRef]
-- pTerm = debugParser "pTerm" $ choice [pList, pParens, pNum, pStr, pRef]

-- This function automates building complicated nested grammars that parse
-- operators correctly. It's kind of annoying, but I haven't figured out how
-- to do without it. Also it seems like it will get more useful if I want to
-- add non-assignment statements like assertions. See:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: ParseM CutExpr
pExpr = debugParser "pExpr" $ do
  (_, cfg, _, _) <- getState
  -- debugParseM "expr"
  res <- E.buildExpressionParser (operatorTable cfg) pTerm <?> "expression"
  -- res <- E.buildExpressionParser (operatorTable cfg) pTerm <?> "expression"
  -- let res' = debugParser cfg "pExpr" res
  return res
