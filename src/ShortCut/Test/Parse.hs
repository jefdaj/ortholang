module ShortCut.Test.Parse where

import ShortCut.Core.Types
import ShortCut.Core.Parse
import ShortCut.Core.Pretty -- (prettyShow)
import ShortCut.Test.Parse.Arbitrary
import ShortCut.Test.Parse.Examples
import Test.Tasty.QuickCheck

import Test.Tasty            (TestTree, testGroup)

-- TODO switch this to use HSpec?
import Test.Tasty.HUnit      ((@=?), testCase)

import Text.Parsec           (ParseError)
import Data.Either           (isRight)
import Text.PrettyPrint.HughesPJClass (Pretty(..))

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

-- TODO round-trip function!

regularParse :: ParseM a -> CutConfig -> String -> Either ParseError a
regularParse p cfg = parseWithEof p ([], cfg)

takeVar :: String -> CutVar
takeVar = CutVar . takeWhile (flip elem $ vNonFirstChars)

parsedItAll :: ParseM a -> CutConfig -> String -> Bool
parsedItAll p cfg str' = case parseWithLeftOver p ([], cfg) str' of
  Right (_, "") -> True
  _ -> False

-- parse some cut code, pretty-print it, parse again,
-- and check that the two parsed ASTs are equal
roundTrip :: (Eq a, Show a, Pretty a) => CutConfig
          -> ParseM a -> String -> Either (String, String) a
roundTrip cfg psr code = case regularParse psr cfg code of
  Left  l1 -> Left (code, show l1)
  Right r1 -> case regularParse psr cfg $ prettyShow r1 of
    Left  l2 -> Left (code, show l2)
    Right r2 -> if r1 == r2
                  then Right r2
                  else Left (code, show r2)

-- Test that a list of example strings can be parsed + printed + parsed,
-- and both parses come out correctly, or return the first error.
tripExamples :: (Eq a, Show a, Pretty a) => CutConfig -> ParseM a
             -> [(String, a)] -> Either (String, String) ()
tripExamples _ _ [] = Right ()
tripExamples cfg p ((a,b):xs) = case roundTrip cfg p a of
  Left  l -> Left (a, show l)
  Right r -> if r == b
    then tripExamples cfg p xs
    else Left (a, show r)

-----------
-- tests --
-----------

mkTests :: CutConfig -> IO TestTree
mkTests cfg = return $ testGroup "test parser"
                         [exTests cfg, wsProps cfg, acProps cfg]

mkCase :: (Show a, Eq a, Pretty a) => CutConfig -> String -> ParseM a
       -> [(String, a)] -> TestTree
mkCase cfg name parser examples = 
  testCase name $ Right () @=? tripExamples cfg parser examples

exTests :: CutConfig -> TestTree
exTests cfg = testGroup "round-trip handwritten cut code"
  [ mkCase cfg "function calls"   pFun       exFuns
  , mkCase cfg "terms"            pTerm      exTerms
  , mkCase cfg "expressions"      pExpr      exExprs
  , mkCase cfg "statements"       pStatement exStatements
  -- , mkCase cfg "binary operators" pBop       exBops
  ]

wsProps :: CutConfig -> TestTree
wsProps cfg = testGroup "consume randomly generated whitespace"
  [ testProperty "after variables" $
    \(ExVar v@(CutVar s)) (ExSpace w) ->
      parseWithLeftOver pVar ([], cfg) (s ++ w) == Right (v, "")
  , testProperty "after symbols" $
    \(ExSymbol c) (ExSpace w) ->
      parseWithLeftOver (pSym c) ([], cfg) (c:w) == Right ((), "")
  , testProperty "after equals signs in assignment statements" $
    \(ExAssign a) (ExSpace w) ->
      parseWithLeftOver pVarEq ([], cfg) (a ++ w) == Right (takeVar a, "")
  , testProperty "after quoted strings" $
    \(ExQuoted q) (ExSpace w) ->
      parseWithLeftOver pQuoted ([], cfg) (q ++ w) == Right (read q, "")
  , testProperty "after numbers" $
    \(ExNum n) (ExSpace w) -> parsedItAll pNum cfg (n ++ w)
  , testProperty "before the first identifier on a new line" $
    \(ExComment c) (ExVar (CutVar s)) ->
      parseWithLeftOver pComment ([], cfg) (c ++ "\n" ++ s) == Right ((), s)
  ]

-- TODO use round-trip here too
acProps :: CutConfig -> TestTree
acProps cfg = testGroup "parse randomly generated cut code"
  [ testProperty "variable names" $
      \(ExVar v@(CutVar s)) -> parseWithLeftOver pVar ([], cfg) s == Right (v, "")
  , testProperty "symbols (reserved characters)" $
      \(ExSymbol c) -> parseWithLeftOver (pSym c) ([], cfg) [c] == Right ((), "")
  , testProperty "variables with equal signs after" $
      \(ExAssign a) ->
        parseWithLeftOver pVarEq ([], cfg) a == Right (takeVar a, "")
  , testProperty "quoted strings" $
      \(ExQuoted q) -> regularParse pQuoted cfg q == Right (read q)
  , testProperty "comments" $
      \(ExComment c) -> parseWithLeftOver pComment ([], cfg) c == Right ((), "")
  , testProperty "positive numbers" $
      \(ExNum n) -> isRight $ regularParse pNum cfg n
  ]
