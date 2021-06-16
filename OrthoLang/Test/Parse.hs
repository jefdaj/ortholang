{-|
TODO add pretty-printing round trips to everything

TODO move some of the test utilties here to Utils.hs?

TODO adjust generators to handle script state!

need to:

* generate number and string statements

* generate references to those

* generate function calls with those

some ideas for alternative/easier formulation:

* given a valid script, a reference to any var is valid

* given a valid script, fn calls made from refs are valid
  (unless they create cycles? see what happens!)

TODO example function calls

TODO example bop expressions

TODO everything with and without parens

TODO test operator precedence
-}

module OrthoLang.Test.Parse where

import OrthoLang.Types
import OrthoLang.Interpreter
import OrthoLang.Test.Parse.Arbitrary
import OrthoLang.Test.Parse.Examples
import OrthoLang.Modules     (modules)
import Test.Tasty.QuickCheck

-- TODO parsing doesn't actually require the hashed seqids ref

import Test.Tasty            (TestTree, testGroup)

-- TODO switch this to use HSpec?
import Test.Tasty.HUnit      ((@=?), testCase)

import Data.Either           (isRight)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import Text.Parsec.Combinator     (manyTill, eof, anyToken)

------------------------
-- utility functions --
-----------------------

-- TODO does this generally need to be recursive, digests of the exprs inside it too?
-- digestExample :: GlobalEnv -> (String, Expr) -> (String, Expr, DigestMap)
-- digestExample st (s, e) = let p = exprPath cfg dRef scr e
--                           in (s, e, M.singleton (pathDigest p) (typeOf e, p))

-- TODO remember to wrap in withFileLogging if running from stack repl
-- digestExamples :: GlobalEnv -> [(String, Expr)] -> IO [(String, Expr, DigestMap)]
-- digestExamples st = mapM (digestExample st)

parseWithLeftOver :: [Module] -> ParseM a -> Config -> Script -> String -> Either String (a,String)
parseWithLeftOver ms p c s = runParseM ms ((,) <$> p <*> leftOver) c s
  where
    leftOver = manyTill anyToken eof

-- TODO take mods as an arg to match the ones in Parse.Util?
regularParse :: ParseM a -> Config -> String -> Either String a
regularParse p cfg = parseWithEof modules p cfg emptyScript

takeVar :: String -> Var
takeVar = Var (RepID Nothing) . takeWhile (flip elem vNonFirstChars)

parsedItAll :: ParseM a -> Config -> String -> Bool
parsedItAll p cfg str' = case parseWithLeftOver modules p cfg emptyScript str' of
  Right (_, "") -> True
  _ -> False

{-|
Parse some cut code, pretty-print it, parse again,
and check that the two parsed ASTs are equal.
-}
roundTrip :: (Eq a, Show a, Pretty a) => Config
          -> ParseM a -> String -> Either (String, String) a
roundTrip cfg psr code = case regularParse psr cfg code of
  Left  l1 -> Left (code, "parse 1 failed: " ++ show l1)
  Right r1 -> case regularParse psr cfg $ prettyShow r1 of
    Left  l2 -> Left (code, "parse 2 failed: " ++ show l2)
    Right r2 -> if r1 == r2
                  then Right r2
                  else Left (code, "equality failed: " ++ show r2 ++ " /= " ++ show r1)

{-|
Test that a list of example strings can be parsed + printed + parsed,
and both parses come out correctly, or return the first error.
-}
tripExamples :: (Eq a, Show a, Pretty a) => Config
             -> ParseM a -> [(String, a)] -> Either (String, String) ()
tripExamples _ _ [] = Right ()
tripExamples cfg p ((a,b):xs) = case roundTrip cfg p a of
  Left  l -> Left (a, "round-trip failed: " ++ show l)
  Right r -> if r == b
    then tripExamples cfg p xs
    else Left (a, "round-trip was wrong: " ++ show r ++ " /= " ++ show b)

-----------
-- tests --
-----------

mkTests :: Config -> LocksRef -> IDsRef -> DigestsRef -> IO TestTree
mkTests cfg ref ids _ = return $ testGroup "test parser"
  [ exTests cfg ref ids
  , wsProps cfg ref ids
  , acProps cfg ref ids
  ]

-- a here can be: Expr, Assign, ...
mkCase :: (Show a, Eq a, Pretty a) => Config -> LocksRef -> IDsRef
       -> String -> ParseM a -> [(String, a)] -> TestTree
mkCase cfg ref ids name parser examples =
  testCase name $ Right () @=? tripExamples cfg parser examples

exTests :: Config -> LocksRef -> IDsRef -> TestTree
exTests cfg ref ids = testGroup "round-trip handwritten cut code"
  [ mkCase cfg ref ids "function calls"   pFun       exFuns
  , mkCase cfg ref ids "terms"            pTerm      exTerms
  , mkCase cfg ref ids "expressions"      pExpr      exExprs
  , mkCase cfg ref ids "statements"       pStatement exStatements
  -- , mkCase cfg "binary operators" pBop       exBops
  ]

wsProps :: Config -> LocksRef -> IDsRef -> TestTree
wsProps cfg ref ids = testGroup "consume randomly generated whitespace"
  [ testProperty "after variables" $
    \(ExVar v@(Var _ s)) (ExSpace w) ->
      parseWithLeftOver modules pVar cfg emptyScript (s ++ w) == Right (v, "")
  , testProperty "after symbols" $
    \(ExSymbol c) (ExSpace w) ->
      parseWithLeftOver modules (pSym c) cfg emptyScript (c:w) == Right ((), "")
  , testProperty "after equals signs in assignment statements" $
    \(ExAssign a) (ExSpace w) ->
      parseWithLeftOver modules pVarEq cfg emptyScript (a ++ w) == Right (takeVar a, "")
  , testProperty "after quoted strings" $
    \(ExQuoted q) (ExSpace w) ->
      parseWithLeftOver modules pQuoted cfg emptyScript (q ++ w) == Right (read q, "")
  , testProperty "after numbers" $
    \(ExNum n) (ExSpace w) -> parsedItAll pNum cfg (n ++ w)
  ]

-- TODO use round-trip here too
acProps :: Config -> LocksRef -> IDsRef -> TestTree
acProps cfg ref ids = testGroup "parse randomly generated cut code"
  [ testProperty "variable names" $
      \(ExVar v@(Var _ s)) -> parseWithLeftOver modules pVar cfg emptyScript s == Right (v, "")
  , testProperty "symbols (reserved characters)" $
      \(ExSymbol c) -> parseWithLeftOver modules (pSym c) cfg emptyScript [c] == Right ((), "")
  , testProperty "variables with equal signs after" $
      \(ExAssign a) ->
        parseWithLeftOver modules pVarEq cfg emptyScript a == Right (takeVar a, "")
  , testProperty "quoted strings" $
      \(ExQuoted q) -> regularParse pQuoted cfg q == Right (read q)
  , testProperty "positive numbers" $
      \(ExNum n) -> isRight $ regularParse pNum cfg n
  -- TODO shouldn't have to parse it all since there's a random iden added too
  -- TODO (but why did I do that? lol)
  , testProperty "function names" $
      \(ExFun f) -> isRight $ regularParse pFunName cfg f
  ]
