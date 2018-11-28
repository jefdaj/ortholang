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
import Data.Map              (empty)
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

regularParse :: ParseM a -> CutConfig -> Locks -> String -> Either ParseError a
regularParse p cfg ref = parseWithEof p ([], cfg, ref, empty)

takeVar :: String -> CutVar
takeVar = CutVar . takeWhile (flip elem $ vNonFirstChars)

parsedItAll :: ParseM a -> CutConfig -> Locks -> String -> Bool
parsedItAll p cfg ref str' = case parseWithLeftOver p ([], cfg, ref, empty) str' of
  Right (_, "") -> True
  _ -> False

-- parse some cut code, pretty-print it, parse again,
-- and check that the two parsed ASTs are equal
roundTrip :: (Eq a, Show a, Pretty a) => CutConfig -> Locks
          -> ParseM a -> String -> Either (String, String) a
roundTrip cfg ref psr code = case regularParse psr cfg ref code of
  Left  l1 -> Left (code, show l1)
  Right r1 -> case regularParse psr cfg ref $ prettyShow r1 of
    Left  l2 -> Left (code, show l2)
    Right r2 -> if r1 == r2
                  then Right r2
                  else Left (code, show r2)

-- Test that a list of example strings can be parsed + printed + parsed,
-- and both parses come out correctly, or return the first error.
tripExamples :: (Eq a, Show a, Pretty a) => CutConfig -> Locks -> ParseM a
             -> [(String, a)] -> Either (String, String) ()
tripExamples _ _ _ [] = Right ()
tripExamples cfg ref p ((a,b):xs) = case roundTrip cfg ref p a of
  Left  l -> Left (a, show l)
  Right r -> if r == b
    then tripExamples cfg ref p xs
    else Left (a, show r)

-----------
-- tests --
-----------

mkTests :: CutConfig -> Locks -> IO TestTree
mkTests cfg ref = return $ testGroup "test parser"
                             [exTests cfg ref, wsProps cfg ref, acProps cfg ref]

mkCase :: (Show a, Eq a, Pretty a) => CutConfig -> Locks
       -> String -> ParseM a -> [(String, a)] -> TestTree
mkCase cfg ref name parser examples = 
  testCase name $ Right () @=? tripExamples cfg ref parser examples

exTests :: CutConfig -> Locks -> TestTree
exTests cfg ref = testGroup "round-trip handwritten cut code"
  [ mkCase cfg ref "function calls"   pFun       exFuns
  , mkCase cfg ref "terms"            pTerm      exTerms
  , mkCase cfg ref "expressions"      pExpr      exExprs
  , mkCase cfg ref "statements"       pStatement exStatements
  -- , mkCase cfg "binary operators" pBop       exBops
  ]

wsProps :: CutConfig -> Locks -> TestTree
wsProps cfg ref = testGroup "consume randomly generated whitespace"
  [ testProperty "after variables" $
    \(ExVar v@(CutVar s)) (ExSpace w) ->
      parseWithLeftOver pVar ([], cfg, ref, empty) (s ++ w) == Right (v, "")
  , testProperty "after symbols" $
    \(ExSymbol c) (ExSpace w) ->
      parseWithLeftOver (pSym c) ([], cfg, ref, empty) (c:w) == Right ((), "")
  , testProperty "after equals signs in assignment statements" $
    \(ExAssign a) (ExSpace w) ->
      parseWithLeftOver pVarEq ([], cfg, ref, empty) (a ++ w) == Right (takeVar a, "")
  , testProperty "after quoted strings" $
    \(ExQuoted q) (ExSpace w) ->
      parseWithLeftOver pQuoted ([], cfg, ref, empty) (q ++ w) == Right (read q, "")
  , testProperty "after numbers" $
    \(ExNum n) (ExSpace w) -> parsedItAll pNum cfg ref (n ++ w)
  ]

-- TODO use round-trip here too
acProps :: CutConfig -> Locks -> TestTree
acProps cfg ref = testGroup "parse randomly generated cut code"
  [ testProperty "variable names" $
      \(ExVar v@(CutVar s)) -> parseWithLeftOver pVar ([], cfg, ref, empty) s == Right (v, "")
  , testProperty "symbols (reserved characters)" $
      \(ExSymbol c) -> parseWithLeftOver (pSym c) ([], cfg, ref, empty) [c] == Right ((), "")
  , testProperty "variables with equal signs after" $
      \(ExAssign a) ->
        parseWithLeftOver pVarEq ([], cfg, ref, empty) a == Right (takeVar a, "")
  , testProperty "quoted strings" $
      \(ExQuoted q) -> regularParse pQuoted cfg ref q == Right (read q)
  , testProperty "positive numbers" $
      \(ExNum n) -> isRight $ regularParse pNum cfg ref n
  -- TODO shouldn't have to parse it all since there's a random iden added too
  -- TODO (but why did I do that? lol)
  , testProperty "function names" $
      \(ExFun f) -> isRight $ regularParse pFunName cfg ref f
  ]
