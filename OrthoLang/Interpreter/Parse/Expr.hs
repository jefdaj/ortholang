module OrthoLang.Interpreter.Parse.Expr
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

    -- * Misc
  , typecheckFn
  )
  where

import Prelude hiding (error)
import OrthoLang.Debug
import OrthoLang.Types
import OrthoLang.Interpreter.Parse.Basic
import OrthoLang.Interpreter.Parse.Util (debugParser, parseWithEof, parseFail, stripComments)
import OrthoLang.Util (headOrDie)

import qualified Text.Parsec.Expr as E
import Control.Monad.Trans.Except
-- import qualified Data.Map.Strict  as M

import Control.Applicative    ((<|>))
import Control.Monad          (void)
import Data.Either            (isRight)
import Data.List              (union, find, intercalate)
import Data.Maybe             (isJust, fromJust, catMaybes)
import Text.Parsec            (try, (<?>))
import Text.Parsec.Char       (string)
import Text.Parsec.Combinator (manyTill, eof, between, choice, sepBy)
import Control.Monad.Reader   (ReaderT)
import Text.Parsec            (getState)


{-
TODO how hard would it be to get Haskell's sequence notation? would it be useful?

TODO once there's [ we can commit to a list, right? should allow failing for real afterward
-}
pList :: ParseM Expr
pList = debugParser "pList" $ do
  terms <- between (pSym '[') (pSym ']') (sepBy pExpr (pSym ','))
  let eType = listElemType $ map typeOf terms
  case eType of
    Left err -> parseFail err
    Right t  -> do
      let deps  = if null terms then [] else foldr1 union $ map depsOf terms
          expr  = Lst t Nothing deps terms -- TODO is seed always Nothing here?
      -- putDigests "pList" (expr:terms)
      return expr

{-|
Searches for the first non-empty type to determine the type of a list.
-}
listElemType :: [Type] -> Either String Type
listElemType ts = if typesOK then Right elemType else Left errorMsg
  where
    nonEmpty = filter isNonEmpty ts
    elemType = if      null ts       then Empty
               else if null nonEmpty then headOrDie "listElemType failed" ts -- for example (ListOf Empty)
               else    headOrDie "listElemType failed" nonEmpty
    typesOK  = all (== elemType) nonEmpty -- TODO is this where we could allow heterogenous lists?
    errorMsg = "list elements have different types: " ++ show ts

isNonEmpty :: Type -> Bool
isNonEmpty Empty      = False
isNonEmpty (ListOf t) = isNonEmpty t
isNonEmpty _          = True

---------------
-- operators --
---------------

{-|
For now, I think all binary operators at the same precedence should work.
but it gets more complicated I'll write out an actual table here with a
prefix function too etc. See the jake wheat tutorial.
-}
operatorTable :: [Module] -> [[E.Operator String Script (ReaderT ParseEnv (Except String)) Expr]]
operatorTable mods = [map binary bops] -- modules would work
  where
    binary f = E.Infix (pBop f) E.AssocLeft
    bops = filter (isJust . fOpChar) (concat $ map mFunctions mods)

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
mkBop bop = return $ \e1 e2 ->
  let eType = listElemType $ [typeOf e1, typeOf e2]
  in case eType of
       Left  e  -> error "mkBop" e -- TODO is a better error type possible here?
       Right r1 ->
         case typecheckFn [fromJust $ fOpChar bop] (fOutput bop) (bopInputs $ fInputs bop) [typeOf e1, typeOf e2] of
           Left  msg -> error "mkBop" msg -- TODO can't `fail` because not in monad here?
           -- TODO is the seed thing right here?
           Right _ -> let seed = if usesSeed bop then (Just $ Seed 0) else Nothing
                      in Bop r1 seed (union (depsOf e1) (depsOf e2)) [fromJust $ fOpChar bop] e1 e2

        -- TODO is naming it after the opchar wrong now?
-- TODO how to putState with these? is it needed at all?
--         let expr = Bop rtn (Seed 0) (union (depsOf e1) (depsOf e2)) [fromJust $ fOpChar bop] e1 e2
--             p    = exprPath cfg dRef scr expr
--             dKey = pathDigest p
--             dVal = (typeOf expr, p)
--             dMap' = M.unions
--                       [ M.insert dKey dVal (sDigests scr)
--                       , trace ("mkBop ds1: " ++ show ds1) ds1
--                       , trace ("mkBop ds2: " ++ show ds2) ds2
--                       ]
        -- in (expr, trace ("mkbop dMap': " ++ show dMap') dMap')

-- TODO does typeSigsMatch already cover (ListOf Empty) comparisons?
-- bopTypeCheck :: TypeChecker -> Type -> Type -> Either String Type
-- bopTypeCheck _ (ListOf Empty) (ListOf Empty) = Right $ ListOf Empty
-- bopTypeCheck _ (ListOf x    ) (ListOf Empty) = Right $ ListOf x
-- bopTypeCheck _ (ListOf Empty) (ListOf x    ) = Right $ ListOf x
-- bopTypeCheck tFn t1 t2 = case tFn [ListOf t1] of
--   Left  e -> Left e
--   Right r -> if t1 == t2
--                then Right r
--                else Left $ "mismatched bop types: " ++ show t1 ++ " and " ++ show t2

-- a hack to generate bop type signatures from the associate fold functions
-- TODO should it live somewhere else?
bopInputs :: [TypeSig] -> [TypeSig]
bopInputs [Exactly (ListOf t)] = [Exactly t, Exactly t]
bopInputs [ListSigs s] = [s, s]
bopInputs s = error "interpreter.parse.expr.bopInputs" $ "bad argument: " ++ show s

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
  mods <- askModules
  (choice $ map (try . str') $ listFunctionNames mods) <?> "fn name"
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
  mods <- askModules
  case findFun mods name of
    Left err -> parseFail err
    Right fn -> typecheckArgs fn args -- TODO why no full7942??


-- TODO this one should be the parser; write a simpler pure typecheck fn
typecheckArgs :: Function -> [Expr] -> ParseM Expr
typecheckArgs fn args = case typecheckFn (fName fn) (fOutput fn) (fInputs fn) (map typeOf args) of
  Left  msg -> parseFail msg
  Right rtn -> do
    -- TODO is the seed thing right here?
    let seed = if usesSeed fn then Just (Seed 0) else Nothing
        expr = Fun rtn seed deps (fName fn) args
        deps = foldr1 union $ map depsOf args
        -- TODO hey should ParseM be ReaderT Config, StateT Script ... instead of StateT both?
    -- putDigests "typecheckArgs" (expr:args)
    return expr

-- | Determines whether a function's type signature matches a list of concrete types.
--   Returns an error message or the function's final output type.
-- TODO rename typecheckArgs? typecheckFn?
typecheckFn :: String -> TypeSig -> [TypeSig] -> [Type] -> Either String Type
typecheckFn name outSig inSigs inTypes =
  let nExp = length inSigs
      nAct = length inTypes
  in if nExp /= nAct
       then Left $ "expected " ++ show nExp ++ " arguments to " ++ name ++
                   ", but got " ++ show nAct ++ "." ++
                   "\nType signature is: " ++ show inSigs ++
                   "\nInputs are: " ++ show inTypes
       else typecheckFn' name outSig inSigs inTypes

{-|
Folds over a list of expected type signatures + actual types, ...

Rules:
  if output is Some, the same one must appear in the sigs (error otherwise)
  if output sig is Some, give it the same exact type as that index in the types

Cases:
  signature typegroups are wrong (check separately while loading modules?)
-}
typecheckFn' :: String -> TypeSig -> [TypeSig] -> [Type] -> Either String Type
typecheckFn' name outSig inSigs inTypes =
  if not (null errors)
    then Left $ errHeader ++ unlines errors
    else inferOutputType name outSig $ zip inSigs inTypes
  where
    errors = catMaybes $ map sigMatch $ zip3 [1..] inSigs inTypes
    sigMatch (n,s,t) = if typeSigMatches s t then Nothing else Just $ explain (n,s,t)
    errHeader = name ++ " has the type signature " ++ show inSigs
                     ++ ",\nwhich doesn't match its inputs "   ++ show inTypes
                     ++ "\nSpecifically, "
    explain (n,s,t) = "the " ++ show t ++ " in position " ++ show (n :: Int)
                      ++ " doesn't match " ++ show s ++ "."

{-|
If outSig can be converted to an exact type, do that. If it's a typegroup, find the matching
inSig and return that exact type. Return an error if it doesn't match exactly one.

TODO also error if it matches more than one unique type
-}
inferOutputType :: String -> TypeSig -> [(TypeSig, Type)] -> Either String Type
inferOutputType _ (Exactly t) _ = Right t
inferOutputType n (ListSigs     s) sts = fmap  ListOf       $ inferOutputType n s sts
inferOutputType n (ScoresSigs   s) sts = fmap  ScoresOf     $ inferOutputType n s sts
inferOutputType n (EncodedSig e s) sts = fmap (EncodedAs e) $ inferOutputType n s sts
inferOutputType n s@(AnyType _) sts = inferAmbigOutputType n s sts
inferOutputType n s@(Some  _ _) sts = inferAmbigOutputType n s sts

{-|
The rule is that when a function has an ambiguous output type (it includes an
AnyType or Some constructor), that same ambiguous type has to also be one of
the inputs. Then we can assume that the matching actual input 'Type' should
also be the return type. Note that equality includes the 'String' descriptions.
Errors here should be evaluated immediately so they don't confuse users later!

TODO should still work when the matching sig is inside another
-}
inferAmbigOutputType :: String -> TypeSig -> [(TypeSig, Type)] -> Either String Type
inferAmbigOutputType n s sts =
  let sts' = flattenAmbigTypes [] sts
  in case find (\(s2,_) -> s == s2) sts' of
       Nothing -> Left $ "Invalid type signature for " ++ n ++ "!" ++
                         "\nAmbiguous return type " ++  show s ++
                         " doesn't match any of the ambiguous inputs:\n" ++ 
                         intercalate "\n" (map show sts')
       Just (_,t) -> Right t

-- | Accumulates a flat list of ambiguous 'TypeSig's with their matching 'Type's,
--   which is necessary because the one we want might be nested.
flattenAmbigTypes :: [(TypeSig, Type)] -> [(TypeSig, Type)] -> [(TypeSig, Type)]
flattenAmbigTypes acc [] = acc
flattenAmbigTypes acc (x:xs) = case x of
  (ListSigs     s, ListOf      t) -> flattenAmbigTypes acc ((s,t):xs)
  (ListSigs     _, _            ) -> flattenAmbigTypes acc xs
  (ScoresSigs   s, ScoresOf    t) -> flattenAmbigTypes acc ((s,t):xs)
  (ScoresSigs   _, _            ) -> flattenAmbigTypes acc xs
  (EncodedSig _ s, EncodedAs _ t) -> flattenAmbigTypes acc ((s,t):xs)
  (EncodedSig _ _, _            ) -> flattenAmbigTypes acc xs
  (Exactly      _, _            ) -> flattenAmbigTypes acc xs
  ((AnyType _),_) -> flattenAmbigTypes (x:acc) xs
  ((Some  _ _),_) -> flattenAmbigTypes (x:acc) xs

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
  case lookupExpr var (sAssigns scr) of
    Nothing -> trace "pRef" ("scr before lookup of \"" ++ var ++ "\": " ++ show (sAssigns scr)) $
                 parseFail $ "no such variable \"" ++ var ++ "\"" ++ "\n" -- ++ show scr
    Just e -> return $ Ref (typeOf e) (seedOf e) (depsOf e) v

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
  mods <- askModules
  E.buildExpressionParser (operatorTable mods) pTerm <?> "expression"
  -- return $ unsafePerformIO (insertNewRulesDigest st res >> return res) -- TODO move to compiler
  -- putDigests "pExpr" [e]
  -- return e

-- TODO is this incorrectly counting assignment statements of 'result = ...'?
--      (maybe because it only parses the varname and returns?)
isExpr :: [Module] -> Config -> Script -> String -> Bool
isExpr mods cfg scr line = isRight $ parseWithEof mods pExpr cfg scr $ stripComments line

-- TODO make this return the "result" assignment directly?
parseExpr :: [Module] -> Config -> Script -> String -> Either String Expr
parseExpr mods = runParseM mods pExpr
