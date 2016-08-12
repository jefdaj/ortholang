module ShortCut.ParseSpec where

-- TODO: email test function to jakewheatmail@gmail.com?

import ShortCut.Types
import ShortCut.Parse
import ShortCut.ParseExamples

import Data.Either      (isRight)
import Data.List        (isSuffixOf)
import Data.Scientific()
import System.Directory (getDirectoryContents)
import System.FilePath  (combine)
import Test.Hspec
import Test.Hspec.QuickCheck

test :: (Eq a, Show a) => Parser a -> [(String, a)] -> Either (String, String) ()
test _ [] = Right ()
test p ((a,b):xs) = case regularParse p a of
  Left  l -> Left (a, show l)
  Right r -> if r == b
    then test p xs
    else Left (a, show r)

loadExampleScripts :: IO [(String, ParsedScript)]
loadExampleScripts = do
  let exDir = "examples"
  paths <- getDirectoryContents exDir
  let rawPaths = map (combine exDir) $ filter (isSuffixOf ".cut") paths
      astPaths = map (combine exDir) $ filter (isSuffixOf ".ast") paths
  raws <- mapM readFile rawPaths
  asts <- mapM readFile astPaths
  return $ zip raws $ map read asts

takeVarName :: String -> VarName
takeVarName = VarName . takeWhile (flip elem $ vNonFirstChars)

parsedItAll :: Parser a -> String -> Expectation
parsedItAll p str = (`shouldReturn` True) $
  case parseWithLeftOver p str of
     Right (_, "") -> return True
     _ -> return False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "[p]arses Strings to ParsedExprs" $ do

    describe "pVarName" $ do
      it "parses some valid variable names" $
        test pVarName exVarNames `shouldBe` Right ()
      prop "parses any valid variable name" $
        \(ExVarName v@(VarName s)) -> parseWithLeftOver pVarName s == Right (v, "")
      prop "consumes trailing whitespace" $
        \(ExVarName v@(VarName s)) (ExSpace w) ->
          parseWithLeftOver pVarName (s ++ w) == Right (v, "")

    -- TODO: check that it fails with other trailing chars
    describe "pSym" $ do
      prop "parses any valid symbol (reserved char)" $
        \(ExSymbol c) -> parseWithLeftOver (pSym c) [c] == Right ((), "")
      prop "consumes trailing whitespace" $
        \(ExSymbol c) (ExSpace w) ->
          parseWithLeftOver (pSym c) (c:w) == Right ((), "")

    -- TODO: make sure the - op never catches "-" inside scientific notation
    -- TODO: make sure the - op never catches "-" inside quoted strings
    describe "pExpr" $ do
      it "parses some valid expressions to their correct ASTs" $
        test pExpr exExprs `shouldBe` Right ()
      prop "parses generated expressions (ASTs not checked)" $
        \(ExExpr e) -> parsedItAll pExpr e

    describe "pAssign" $ do
      it "parses statements generated from the vars + expressions" $
        test pAssign exStatements `shouldBe` Right ()

    describe "pScript" $ do
      it "parses the concatenated example statements to the correct AST" $
        test pScript exScripts `shouldBe` Right ()
      examples <- runIO loadExampleScripts
      it "parses the example scripts to their correct ASTs" $
        test pScript examples `shouldBe` Right ()

    describe "pVarEq" $ do
      prop "parses the first half of a statement, up through '='" $
        \(ExAssign a) ->
          parseWithLeftOver pVarEq a == Right (takeVarName a, "")
      prop "consumes trailing whitespace" $
        \(ExAssign a) (ExSpace w) ->
          parseWithLeftOver pVarEq (a ++ w) == Right (takeVarName a, "")

    describe "pQuoted" $ do
      prop "parses quoted strings, preserving internal whitespace" $
        \(ExQuoted q) -> regularParse pQuoted q == Right (read q)
      prop "consumes trailing whitespace" $
        \(ExQuoted q) (ExSpace w) ->
          parseWithLeftOver pQuoted (q ++ w) == Right (read q, "")

    describe "pComment" $ do
      prop "skips comments" $
        \(ExComment c) -> parseWithLeftOver pComment c == Right ((), "")
      prop "stops at the first iden char on a new line" $
        \(ExComment c) (ExVarName (VarName s)) ->
          parseWithLeftOver pComment (c ++ "\n" ++ s) == Right ((), s)

    describe "pNum" $ do
      it "parses some example numbers correctly" $
        test pNum exNums == Right ()
      prop "parses random positive numbers (results not checked)" $
        \(ExNum n) -> isRight $ regularParse pNum n
      prop "consumes trailing whitespace" $
        \(ExNum n) (ExSpace w) -> parsedItAll pNum (n ++ w)
      it "parses negative numbers" $
        pendingWith "how to tell apart from the - op?"

    describe "pCmd" $ do
      it "parses some example functions to their correct ASTs" $
        test pCmd exCmds `shouldBe` Right ()
      prop "parses generated functions (ASTs not checked)" $
        \(ExCmd f) -> parsedItAll pCmd f

    describe "pTerm" $ do
      it "parses some example terms to their correct ASTs" $
        test pTerm exTerms `shouldBe` Right ()
      prop "parses generated terms (ASTs not checked)" $
        \(ExTerm t) -> isRight $ regularParse pTerm t

    describe "pParens" $ do
      it "parses example expressions to their correct ASTs" $
        test pParens parensExamples == Right ()
      prop "parses generated exprs inside parens (ASTS not checked)" $
        \(ExExpr e) -> parsedItAll pParens ("("++ e ++")")
      prop "matches the parse results from expr" $
        \(ExExpr e) ->
          regularParse pParens ("(" ++ e ++ ")")
          ==
          regularParse pExpr e
