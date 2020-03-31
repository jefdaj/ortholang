module OrthoLang.Core.Parse.Expr
  (
  -- * Lists
    pList

  -- * Binary Operators
  , operatorTable
  , BopExprsParser
  , pBop
  , pBopOp
  , mkBop

  -- * Functions
  , pFunName
  , pFun
  , pArgs
  , pFunArgs
  , typecheckArgs

  -- * References
  , pRef

  -- * Expressions
  , pParens
  , pTerm
  , pExpr
  , isExpr
  , parseExpr
  )
  where

import OrthoLang.Core.Types
import OrthoLang.Core.Pretty ()
import OrthoLang.Core.Parse.Basic
import OrthoLang.Core.Parse.Util

import OrthoLang.Util (trace)

import qualified Text.Parsec.Expr as E
import Control.Monad.Trans.Except
-- import qualified Data.Map.Strict  as M

import Control.Applicative    ((<|>))
import Control.Monad          (void)
import Data.Either            (isRight)
import Data.List              (union)
import Data.Maybe             (isJust, fromJust)
import Text.Parsec            (try, (<?>))
import Text.Parsec.Char       (string)
import Text.Parsec.Combinator (manyTill, eof, between, choice, sepBy)
import Control.Monad.Reader   (ReaderT, ask)
import Text.Parsec            (getState)


{-
TODO how hard would it be to get Haskell's sequence notation? would it be useful?

TODO once there's [ we can commit to a list, right? should allow failing for real afterward
-}
pList :: ParseM Expr
pList = debugParser "pList" $ do
  terms <- between (pSym '[') (pSym ']') (sepBy pExpr (pSym ','))
  let eType = nonEmptyType $ map typeOf terms
  case eType of
    Left err -> parseFail err
    Right t  -> do
      let deps  = if null terms then [] else foldr1 union $ map depsOf terms
          expr  = Lst t (Salt 0) deps terms
      -- putDigests "pList" (expr:terms)
      return expr

---------------
-- operators --
---------------

{-|
For now, I think all binary operators at the same precedence should work.
but it gets more complicated I'll write out an actual table here with a
prefix function too etc. See the jake wheat tutorial.
-}
operatorTable :: Config -> [[E.Operator String Script (ReaderT Config (Except String)) Expr]]
operatorTable cfg = [map binary bops]
  where
    binary f = E.Infix (pBop f) E.AssocLeft
    bops = filter (isJust . fOpChar) (concat $ map mFunctions mods)
    mods = cfgModules cfg

{-|
This is an annoying extra type, but I can't figure out how to implement pBop without it.

TODO how to putState in here?? that's probably the multiply bug
-}
type BopExprsParser = ParseM (Expr -> Expr -> Expr)

{-
TODO is there a better way than only taking one-char strings?

TODO how to fail gracefully (with fail, not error) here??

TODO is the error that putState is never called in here?
-}
pBop :: Function -> BopExprsParser
pBop bop | isJust (fOpChar bop) = pBopOp (fName bop) (fromJust $ fOpChar bop) *> mkBop bop
pBop _ = parseFail "pBop only works with infix functions"

pBopOp :: String -> Char -> ParseM ()
pBopOp name c = debugParser ("pBopOp " ++ name) (pSym c)

mkBop :: Function -> BopExprsParser
mkBop bop = return $ \e1 e2 -> do
  case bopTypeCheck (fTypeCheck bop) (typeOf e1) (typeOf e2) of
    Left  msg -> error msg -- TODO can't `fail` because not in monad here?
    Right rtn -> Bop rtn (Salt 0) (union (depsOf e1) (depsOf e2)) [fromJust $ fOpChar bop] e1 e2

        -- TODO is naming it after the opchar wrong now?
-- TODO how to putState with these? is it needed at all?
--         let expr = Bop rtn (Salt 0) (union (depsOf e1) (depsOf e2)) [fromJust $ fOpChar bop] e1 e2
--             p    = exprPath cfg scr expr
--             dKey = exprPathDigest p
--             dVal = (typeOf expr, p)
--             dMap' = M.unions
--                       [ M.insert dKey dVal (sDigests scr)
--                       , trace ("mkBop ds1: " ++ show ds1) ds1
--                       , trace ("mkBop ds2: " ++ show ds2) ds2
--                       ]
        -- in (expr, trace ("mkbop dMap': " ++ show dMap') dMap')

-- TODO does typesMatch already cover (ListOf Empty) comparisons?
bopTypeCheck :: TypeChecker -> Type -> Type -> Either String Type
bopTypeCheck _ (ListOf Empty) (ListOf Empty) = Right $ ListOf Empty
bopTypeCheck _ (ListOf x    ) (ListOf Empty) = Right $ ListOf x
bopTypeCheck _ (ListOf Empty) (ListOf x    ) = Right $ ListOf x
bopTypeCheck tFn t1 t2 = case tFn [ListOf t1] of
  Left  e -> Left e
  Right r -> if typesMatch [t1] [t2]
               then Right r
               else Left $ "mismatched bop types: " ++ show t1 ++ " and " ++ show t2

---------------
-- functions --
---------------

{-|
TODO load names from modules, of course

TODO put this in terms of "keyword" or something?

TODO get function names from modules
-}
pFunName :: ParseM String
pFunName = do
  cfg <- ask
  (choice $ map (try . str') $ listFunctionNames cfg) <?> "fn name"
  where
    str' s = string s <* (void spaces1 <|> eof)

{-|
TODO any way to do this last so "function not found" error comes through??

TODO should be able to commit after parsing the fn name, which would allow real failure
-}
pFun :: ParseM Expr
pFun = do
  name <- try pFunName
  args <- pArgs
  -- putDigests "pFun" args
  pFunArgs name args

{-|
TODO main parse error is in here, when pTerm fails on an arg??

TODO so is pTerm failing on it, or pEnd succeeding?

TODO is this where we forget to add the list digest?
-}
pArgs :: ParseM [Expr]
pArgs = debugParser "pArgs" $ do
  es <- manyTill (try pTerm) pEnd
  -- putDigests "pArgs" es
  return es

{-|
This function uses error rather than fail to prevent parsec from trying anything more

TODO is there a better way?

TODO hey is this where it's missing the dmap?
-}
pFunArgs :: String -> [Expr] -> ParseM Expr
pFunArgs name args = debugParser "pFun" $ do
  cfg <- ask
  -- name <- try pFunName -- after this, we can commit to the fn and error on bad args
  -- args <- pArgs
  -- args <- manyTill pTerm pEnd
  let fns  = concat $ map mFunctions $ cfgModules cfg
      fns' = filter (\f -> fName f == name) fns
  case fns' of
    []      -> parseFail $ "no function found with name \"" ++ name ++ "\""
    (fn:[]) -> typecheckArgs fn args -- TODO why no full7942??
    _       -> parseFail $ "function name collision! multiple fns match \"" ++ name ++ "\""

typecheckArgs :: Function -> [Expr] -> ParseM Expr
typecheckArgs fn args = case (fTypeCheck fn) (map typeOf args) of
  Left  msg -> parseFail msg
  Right rtn -> do
    let expr = Fun rtn (Salt 0) deps (fName fn) args
        deps = foldr1 union $ map depsOf args
        -- TODO hey should ParseM be ReaderT Config, StateT Script ... instead of StateT both?
    -- putDigests "typecheckArgs" (expr:args)
    return expr

{-|
A reference is just a variable name, but that variable has to be in the script.
Since this is the last term parser, it can actually error instead of failing.

TODO why does it fail after this, but only sometimes??

TODO main error is in here when if fails on a ref that clearly exists?

TODO any need for digests here?
-}
pRef :: ParseM Expr
pRef = debugParser "pRef" $ do
  v@(Var _ var) <- pVar
  scr <- getState
  case lookup v scr of
    Nothing -> trace "pRef" ("scr before lookup of \"" ++ var ++ "': " ++ show scr) $
                 parseFail $ "no such variable \"" ++ var ++ "\"" ++ "\n" -- ++ show scr
    Just e -> return $ Ref (typeOf e) (Salt 0) (depsOf e) v

-- debugParseM :: String -> String -> 
-- debugParseM name msg = 

-----------------
-- expressions --
-----------------

pParens :: ParseM Expr
pParens = debugParser "pParens" (between (pSym '(') (pSym ')') pExpr <?> "parens")

-- TODO need to commit to separate branches so Parsec doesn't try to parse
--      other stuff after substitute_each fails. the rule should be:
--      once a fn/macro name is parsed it commits to that branch
--      if none of them work it moves on to others
--      without that we get silly errors like "no such variable" for any of them!
--
-- TODO main error is in here when pRef fails?

-- TODO hey should I actually just name this pArg?

pTerm :: ParseM Expr
-- pTerm = debugParser "pTerm" $ choice [pList, pParens, pNum, pStr, pFunOrRef]
pTerm = debugParser "pTerm" $ choice [pList, pParens, pNum, pStr, pFun, pRef]
-- pTerm = debugParser "pTerm" $ choice [pList, pParens, pNum, pStr, pRef]

-- This function automates building complicated nested grammars that parse
-- operators correctly. It's kind of annoying, but I haven't figured out how
-- to do without it. Also it seems like it will get more useful if I want to
-- add non-assignment statements like assertions. See:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: ParseM Expr
pExpr = debugParser "pExpr" $ do
  -- debugParseM "expr"
  cfg <- ask
  E.buildExpressionParser (operatorTable cfg) pTerm <?> "expression"
  -- return $ unsafePerformIO (insertNewRulesDigest st res >> return res) -- TODO move to compiler
  -- putDigests "pExpr" [e]
  -- return e

-- TODO is this incorrectly counting assignment statements of 'result = ...'?
--      (maybe because it only parses the varname and returns?)
isExpr :: (Config, Script) -> String -> Bool
isExpr state line = isRight $ parseWithEof pExpr state line

-- TODO make this return the "result" assignment directly?
parseExpr :: (Config, Script) -> String -> Either String Expr
parseExpr = runParseM pExpr
