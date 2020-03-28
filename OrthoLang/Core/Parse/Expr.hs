module OrthoLang.Core.Parse.Expr where

import OrthoLang.Core.Types
import OrthoLang.Core.Pretty()
import OrthoLang.Core.Parse.Util
import OrthoLang.Core.Parse.Basic
import OrthoLang.Core.Util (headOrDie)
import OrthoLang.Core.Paths (exprPath, exprPathDigest)

import qualified Text.Parsec.Expr as E
-- import Text.PrettyPrint.HughesPJClass (render, pPrint)

import Control.Applicative    ((<|>))
import Control.Monad          (void)
import Control.Monad.Identity (Identity)
import Data.List              (union)
import Data.Maybe (isJust, fromJust)
import Text.Parsec            (try, getState, (<?>))
import Text.Parsec.Char       (string)
import Text.Parsec.Combinator (manyTill, eof, between, choice, sepBy)

import Data.Either            (isRight)
import Text.Parsec            (ParseError)

import qualified Data.Map.Strict as M

-- TODO how hard would it be to get Haskell's sequence notation? would it be useful?
-- TODO once there's [ we can commit to a list, right? should allow failing for real afterward
pList :: ParseM (Expr, DigestMap)
pList = debugParser "pList" $ do
  tds <- between (pSym '[') (pSym ']')
                 (sepBy pExpr (pSym ','))
  st <- getState
  let (terms, ds) = unzip tds
      eType = nonEmptyType $ map typeOf terms
      deps  = if null terms then [] else foldr1 union $ map depsOf terms
  case eType of
    Left err -> fail err
    Right t  -> do
      let expr  = Lst t (Salt 0) deps terms
          p    = exprPath st expr
          dKey = exprPathDigest p
          dVal = (typeOf expr, p)
          dMap = M.unions $ (M.singleton dKey dVal) : ds
      return (expr, dMap)

---------------
-- operators --
---------------

-- for now, I think all binary operators at the same precedence should work.
-- but it gets more complicated I'll write out an actual table here with a
-- prefix function too etc. see the jake wheat tutorial
operatorTable :: Config -> [[E.Operator String GlobalEnv Identity (Expr, DigestMap)]]
operatorTable cfg = [map binary bops]
  where
    binary f = E.Infix (pBop f) E.AssocLeft
    bops = filter (isJust . fOpChar) (concat $ map mFunctions mods)
    mods = cfgModules cfg

-- Tricky bit: needs to take two already-parsed expressions
-- TODO verify they have the correct types
-- TODO is this obsolete now that there's pBop?
-- pBop :: Char -> ParseM (Expr -> Expr -> Expr)
-- pBop o = pSym o *> (return $ \e1 e2 ->
--   let deps = union (depsOf e1) (depsOf e2)
--   in Bop (typeOf e1) deps [o] e1 e2)

-- TODO is there a better way than only taking one-char strings?
-- TODO how to fail gracefully (with fail, not error) here??
pBop :: Function -> ParseM ((Expr, DigestMap) -> (Expr, DigestMap) -> (Expr, DigestMap))
pBop bop
  | isJust (fOpChar bop) = (debugParser ("pBop " ++ fName bop)
                           (pSym (headOrDie "failed to read fName in pBop" $ [fromJust $ fOpChar bop])))
                           *> do { st <- getState; return (mkBop st bop) }
pBop _ = fail "pBop only works with infix functions"

mkBop :: GlobalEnv -> Function -> ((Expr, DigestMap) -> (Expr, DigestMap) -> (Expr, DigestMap))
mkBop st bop (e1, ds1) (e2, ds2) = case bopTypeCheck (typeOf e1) (typeOf e2) of
  Left  msg -> error msg -- TODO can't `fail` because not in monad here?
  Right rtn -> let expr = Bop rtn (Salt 0) (union (depsOf e1) (depsOf e2)) [fromJust $ fOpChar bop] e1 e2
                   p    = exprPath st expr
                   dKey = exprPathDigest p
                   dVal = (typeOf expr, p)
                   dMap = M.unions [M.singleton dKey dVal, ds1, ds2]
               in (expr, dMap)
  where
    -- TODO does typesMatch already cover (ListOf Empty) comparisons?
    bopTypeCheck (ListOf Empty) (ListOf Empty) = Right $ ListOf Empty
    bopTypeCheck (ListOf x    ) (ListOf Empty) = Right $ ListOf x
    bopTypeCheck (ListOf Empty) (ListOf x    ) = Right $ ListOf x
    bopTypeCheck t1 t2 = case (fTypeCheck bop) [ListOf t1] of
      Left  e -> Left e
      Right r -> if typesMatch [t1] [t2]
                   then Right r
                   else Left $ "mismatched bop types: " ++ show t1 ++ " and " ++ show t2

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
pFun :: ParseM (Expr, DigestMap)
pFun = do
  name <- try pFunName -- TODO try?
  -- debugParseM $ "pFun committed to parsing " ++ name
  args <- pArgs
  -- debugParseM $ "pFun " ++ name ++ " args: " ++ show args
  pFunArgs name args

-- TODO main parse error is in here, when pTerm fails on an arg??
-- TODO so is pTerm failing on it, or pEnd succeeding?
pArgs :: ParseM ([Expr], DigestMap)
pArgs = debugParser "pArgs" $ do
  eds <- manyTill (try pTerm) pEnd
  let (es, ds) = unzip eds
      dMap = M.unions ds
  return (es, dMap)

-- This function uses error rather than fail to prevent parsec from trying anything more
-- (TODO is there a better way?)
pFunArgs :: String -> ([Expr], DigestMap) -> ParseM (Expr, DigestMap)
pFunArgs name (args, ds) = debugParser "pFun" $ do
  (_, cfg, _, _) <- getState
  -- name <- try pFunName -- after this, we can commit to the fn and error on bad args
  -- args <- pArgs
  -- args <- manyTill pTerm pEnd
  let fns  = concat $ map mFunctions $ cfgModules cfg
      fns' = filter (\f -> fName f == name) fns
  case fns' of
    []      -> fail $ "no function found with name '" ++ name ++ "'"
    (fn:[]) -> typecheckArgs fn (args, ds) -- TODO why no full7942??
    _       -> fail $ "function name collision! multiple fns match '" ++ name ++ "'"

-- A reference is just a variable name, but that variable has to be in the script.
-- TODO why does it fail after this, but only sometimes??
-- TODO main error is in here when if fails on a ref that clearly exists?
-- pRef :: String -> ParseM (Expr, DigestMap)
-- pRef var = debugParser "pRef" $ do
--
-- Since this is the last term parser, it can actually error instead of failing
pRef :: ParseM (Expr, DigestMap)
pRef = debugParser "pRef" $ do
  -- v@(Var var) <- pVarOnly
  v@(Var _ var) <- pVar
  -- let v = Var var
  (scr, _, _, _) <- getState
  -- debugParseM $ "scr before lookup of '" ++ var ++ "': " ++ show scr
  case lookup v (sAssigns scr) of
    Nothing -> fail $ "no such variable '" ++ var ++ "'" ++ "\n" -- ++ show scr
    Just e -> return (Ref (typeOf e) (Salt 0) (depsOf e) v, M.empty) -- TODO is it always empty?

-- pFunOrRef :: ParseM (Expr, DigestMap)
-- pFunOrRef = debugParser "pFunOrRef" $ do
--   name <- try pIden -- TODO when it gets here, loader is cut off to oader
--   traceM $ "parsed name: " ++ name
--   (_, cfg, _, _) <- getState
--   if name `elem` fnNames cfg
--     then pFunArgs name
--     else pRef name

typecheckArgs :: Function -> ([Expr], DigestMap) -> ParseM (Expr, DigestMap)
typecheckArgs fn (args, ds) = case (fTypeCheck fn) (map typeOf args) of
  Left  msg -> fail msg
  Right rtn -> do
    st <- getState
    let expr = Fun rtn (Salt 0) deps (fName fn) args
        deps = foldr1 union $ map depsOf args
        dKey = exprPathDigest p
        p    = exprPath st expr
        dVal = (typeOf expr, p)
        dMap = M.insert dKey dVal ds
    return (expr, dMap)

-----------------
-- expressions --
-----------------

pParens :: ParseM (Expr, DigestMap)
pParens = debugParser "pParens" (between (pSym '(') (pSym ')') pExpr <?> "parens")

-- TODO need to commit to separate branches so Parsec doesn't try to parse
--      other stuff after substitute_each fails. the rule should be:
--      once a fn/macro name is parsed it commits to that branch
--      if none of them work it moves on to others
--      without that we get silly errors like "no such variable" for any of them!
--
-- TODO main error is in here when pRef fails?

-- TODO hey should I actually just name this pArg?

pTerm :: ParseM (Expr, DigestMap)
-- pTerm = debugParser "pTerm" $ choice [pList, pParens, pNum, pStr, pFunOrRef]
pTerm = debugParser "pTerm" $ choice [pList, pParens, pNum, pStr, pFun, pRef]
-- pTerm = debugParser "pTerm" $ choice [pList, pParens, pNum, pStr, pRef]

-- This function automates building complicated nested grammars that parse
-- operators correctly. It's kind of annoying, but I haven't figured out how
-- to do without it. Also it seems like it will get more useful if I want to
-- add non-assignment statements like assertions. See:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: ParseM (Expr, DigestMap)
pExpr = debugParser "pExpr" $ do
  st@(_, cfg, _, _) <- getState
  -- debugParseM "expr"
  (e, ds) <- E.buildExpressionParser (operatorTable cfg) pTerm <?> "expression"
  -- return $ unsafePerformIO (insertNewRulesDigest st res >> return res) -- TODO move to compiler
  -- let res' = debugParser cfg "pExpr" res
  let p    = exprPath st e
      dKey = exprPathDigest p
      dVal = (typeOf e, p)
      dMap = M.insert dKey dVal ds
  return (e, dMap)

-- TODO is this incorrectly counting assignment statements of 'result = ...'?
--      (maybe because it only parses the varname and returns?)
isExpr :: GlobalEnv -> String -> Bool
isExpr state line = isRight $ parseWithEof pExpr state line

-- TODO make this return the "result" assignment directly?
parseExpr :: GlobalEnv -> String -> Either ParseError (Expr, DigestMap)
parseExpr = runParseM pExpr
