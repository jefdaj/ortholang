module OrthoLang.Test.Parse where

import OrthoLang.Core.Types
import OrthoLang.Core.Parse
import OrthoLang.Core.Pretty -- (prettyShow)
import OrthoLang.Test.Parse.Arbitrary
import OrthoLang.Test.Parse.Examples
import Test.Tasty.QuickCheck

-- TODO parsing doesn't actually require the hashed seqids ref

import Test.Tasty            (TestTree, testGroup)

-- TODO switch this to use HSpec?
import Test.Tasty.HUnit      ((@=?), testCase)

import Text.Parsec           (ParseError)
import Data.Either           (isRight)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
-- import Data.Map.Strict              (empty)
-- import Data.IORef            (IORef)

-- import Test.Tasty            (TestTree, testGroup)
-- import Test.Tasty.QuickCheck (testProperty)
-- import Text.Parsec           (ParseError)

-- TODO add pretty-printing round trips to everything
-- TODO move some of the test utilties here to Utils.hs?
-- TODO adjust generators to handle script state!
--      need to:
--      * generate number and string statements
--      * generate references to those
--      * generate function calls with those
--      some ideas for alternative/easier formulation:
--      * given a valid script, a reference to any var is valid
--      * given a valid script, fn calls made from refs are valid
--        (unless they create cycles? see what happens!)

------------------------
-- utility functions --
-----------------------

regularParse :: ParseM a -> Config -> LocksRef -> IDsRef -> String -> Either ParseError a
regularParse p cfg ref ids = parseWithEof p (emptyScript, cfg, ref, ids)

takeVar :: String -> Var
takeVar = Var (RepID Nothing) . takeWhile (flip elem $ vNonFirstChars)

parsedItAll :: ParseM a -> Config -> LocksRef -> IDsRef -> String -> Bool
parsedItAll p cfg ref ids str' = case parseWithLeftOver p (emptyScript, cfg, ref, ids) str' of
  Right (_, "") -> True
  _ -> False

-- parse some cut code, pretty-print it, parse again,
-- and check that the two parsed ASTs are equal
roundTrip :: (Eq a, Show a, Pretty a) => Config -> LocksRef -> IDsRef
          -> ParseM a -> String -> Either (String, String) a
roundTrip cfg ref ids psr code = case regularParse psr cfg ref ids code of
  Left  l1 -> Left (code, show l1)
  Right r1 -> case regularParse psr cfg ref ids $ prettyShow r1 of
    Left  l2 -> Left (code, show l2)
    Right r2 -> if r1 == r2
                  then Right r2
                  else Left (code, show r2)

-- Test that a list of example strings can be parsed + printed + parsed,
-- and both parses come out correctly, or return the first error.
tripExamples :: (Eq a, Show a, Pretty a) => Config -> LocksRef -> IDsRef -> ParseM a
             -> [(String, a)] -> Either (String, String) ()
tripExamples _ _ _ _ [] = Right ()
tripExamples cfg ref ids p ((a,b):xs) = case roundTrip cfg ref ids p a of
  Left  l -> Left (a, show l)
  Right r -> if r == b
    then tripExamples cfg ref ids p xs
    else Left (a, show r)

-----------
-- tests --
-----------

mkTests :: Config -> LocksRef -> IDsRef -> IO TestTree
mkTests cfg ref ids = return $ testGroup "test parser"
                               [exTests cfg ref ids, wsProps cfg ref ids, acProps cfg ref ids]

mkCase :: (Show a, Eq a, Pretty a) => Config -> LocksRef -> IDsRef
       -> String -> ParseM a -> [(String, a)] -> TestTree
mkCase cfg ref ids name parser examples = 
  testCase name $ Right () @=? tripExamples cfg ref ids parser examples

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
      parseWithLeftOver pVar (emptyScript, cfg, ref, ids) (s ++ w) == Right (v, "")
  , testProperty "after symbols" $
    \(ExSymbol c) (ExSpace w) ->
      parseWithLeftOver (pSym c) (emptyScript, cfg, ref, ids) (c:w) == Right ((), "")
  , testProperty "after equals signs in assignment statements" $
    \(ExAssign a) (ExSpace w) ->
      parseWithLeftOver pVarEq (emptyScript, cfg, ref, ids) (a ++ w) == Right (takeVar a, "")
  , testProperty "after quoted strings" $
    \(ExQuoted q) (ExSpace w) ->
      parseWithLeftOver pQuoted (emptyScript, cfg, ref, ids) (q ++ w) == Right (read q, "")
  , testProperty "after numbers" $
    \(ExNum n) (ExSpace w) -> parsedItAll pNum cfg ref ids (n ++ w)
  ]

-- TODO use round-trip here too
acProps :: Config -> LocksRef -> IDsRef -> TestTree
acProps cfg ref ids = testGroup "parse randomly generated cut code"
  [ testProperty "variable names" $
      \(ExVar v@(Var _ s)) -> parseWithLeftOver pVar (emptyScript, cfg, ref, ids) s == Right (v, "")
  , testProperty "symbols (reserved characters)" $
      \(ExSymbol c) -> parseWithLeftOver (pSym c) (emptyScript, cfg, ref, ids) [c] == Right ((), "")
  , testProperty "variables with equal signs after" $
      \(ExAssign a) ->
        parseWithLeftOver pVarEq (emptyScript, cfg, ref, ids) a == Right (takeVar a, "")
  , testProperty "quoted strings" $
      \(ExQuoted q) -> regularParse pQuoted cfg ref ids q == Right (read q)
  , testProperty "positive numbers" $
      \(ExNum n) -> isRight $ regularParse pNum cfg ref ids n
  -- TODO shouldn't have to parse it all since there's a random iden added too
  -- TODO (but why did I do that? lol)
  , testProperty "function names" $
      \(ExFun f) -> isRight $ regularParse pFunName cfg ref ids f
  ]
