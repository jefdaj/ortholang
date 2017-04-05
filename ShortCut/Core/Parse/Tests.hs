module ShortCut.Core.Parse.Tests where

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

-- The generators can also be tested manually in ghci like so:
-- > generate gQuoted
-- "\"\\^bx\\ng_!vd\""
-- > generate arbitrary :: IO Quoted
-- Quoted "\"PkGN>.@T\""

-- Function naming conventions:
--
-- handwritten [ex]amples (strings and their correct parses)
-- [Ex]ample QuickCheck types made with string [g]enerators
-- [v]alid and i[n]valid chars (not done yet)

-- TODO email test function to jakewheatmail@gmail.com?

import Data.Either           (isRight)
import Data.Scientific
import ShortCut.Core.Parse
import ShortCut.Core.Types
import Test.QuickCheck
import Test.Tasty            (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Parsec           (ParseError)

-------------------------
-- Arbitrary instances --
-------------------------

-- variable names --

vFirstChars :: [Char]
vFirstChars = '_':['a'..'z']

vNonFirstChars :: [Char]
vNonFirstChars = vFirstChars ++ ['0'..'9']

gVar :: Gen String
gVar = (:) <$> first <*> listOf rest
  where
    first = elements vFirstChars
    rest  = elements vNonFirstChars

newtype ExVar = ExVar CutVar deriving (Eq, Show)

instance Arbitrary ExVar where
  arbitrary = (ExVar . CutVar) <$> gVar

-- references --

newtype ExRef = ExRef String deriving (Eq, Show)

instance Arbitrary ExRef where
  arbitrary = ExRef <$> gVar

-- whitespace --

-- resize keeps this from growing to fill up most of the examples
gWhite :: Gen String
gWhite = resize 2 $ listOf1 $ elements spaceChars

newtype ExSpace = ExSpace String deriving (Eq, Show)

instance Arbitrary ExSpace where
  arbitrary = ExSpace <$> gWhite

-- symbols --

gSym :: Gen Char
gSym = elements "()=\"#"

newtype ExSymbol = ExSymbol Char deriving (Eq, Show)

instance Arbitrary ExSymbol where
  arbitrary = ExSymbol <$> gSym

-- assignments --

gAssign :: Gen String
gAssign = (++ " =") <$> gVar

newtype ExAssign = ExAssign String deriving (Eq, Show)

instance Arbitrary ExAssign where
  arbitrary = ExAssign <$> gAssign

-- quoted string literals --

-- TODO why both Fil and Quoted?

gEscaped :: Gen String
gEscaped = (\c -> '\\':[c]) <$> elements escapeChars

gLiteral :: Gen String
gLiteral = (\c -> [c]) <$> elements literalChars

-- the repeat 15 thing is just to make sure there are more literal chars
-- than escaped ones, because otherwise the strings are very hard to read
-- TODO should just "" be allowed here?
gQuoted :: Gen String
gQuoted = do
  ss <- (listOf . oneof) $ gEscaped:(take 15 $ repeat gLiteral)
  return $ "\"" ++ concat ss ++ "\""

newtype ExQuoted = ExQuoted String deriving (Eq, Show)
newtype ExFile   = ExFile   String deriving (Eq, Show)

instance Arbitrary ExQuoted where
  arbitrary = ExQuoted <$> gQuoted

instance Arbitrary ExFile where
  arbitrary = ExFile <$> gQuoted

-- comments --

gComment :: Gen String
gComment = (\ss -> '#':(concat ss)) <$> listOf gLiteral

newtype ExComment = ExComment String deriving (Eq, Show)

instance Arbitrary ExComment where
  arbitrary = ExComment <$> gComment

-- numbers --

gSci :: Gen Scientific
gSci = scientific <$> arbitrary <*> arbitrary

-- TODO negative numbers too?
gNum :: Gen String
gNum = (show . abs) <$> gSci

newtype ExNum = ExNum String deriving (Eq, Show)

instance Arbitrary ExNum where
  arbitrary = ExNum <$> gNum

-- commands --

gFunName :: Gen String
gFunName = elements fnNames

gFun :: Gen String
gFun = do
  n  <- gFunName
  as <- listOf1 gTerm
  ws <- infiniteListOf gWhite
  let args = concat $ zipWith (++) ws as
  return $ n ++ (head ws) ++ args

newtype ExFun = ExFun String deriving (Eq, Show)

instance Arbitrary ExFun where
  arbitrary = ExFun <$> gFun

-- binary operators --

-- TODO should messy whitespace be added here, or somewhere else?
-- TODO add another one of these for the set operators
gBop :: Gen String
gBop = do
  op <- elements ["+", "-", "*", "/"]
  s1 <- gExpr
  s2 <- gExpr
  return $ concat $ zipWith (++) [s1,op,s2] (repeat " ")

newtype ExBop = ExBop String deriving (Eq, Show)

instance Arbitrary ExBop where
  arbitrary = ExBop <$> gBop

-- expressions --

-- TODO finish these! there are lots of issues so far...
--      the biggest is they need to generate a context for themselves

-- TODO add parens, fn
-- TODO will need to add sized in order to prevent infinite recursion on fn
-- TODO are plain quoted strings allowed, or do they need to be fn args?
gTerm :: Gen String
gTerm = oneof [gNum, gQuoted, gVar]

gExpr :: Gen String
gExpr = oneof [gBop, gTerm, gFun]

newtype ExTerm = ExTerm String deriving (Eq, Show)
newtype ExExpr = ExExpr String deriving (Eq, Show)

instance Arbitrary ExTerm where
  arbitrary = ExTerm <$> gTerm

instance Arbitrary ExExpr where
  arbitrary = ExExpr <$> gExpr

-- statements --

gStatement :: Gen String
gStatement = undefined

newtype ExStatement = ExStatement String deriving (Eq, Show)

instance Arbitrary ExStatement where
  arbitrary = ExStatement <$> gStatement

-- scripts --

gScript :: Gen String
gScript = undefined

newtype ExScript = ExScript String deriving (Eq, Show)

instance Arbitrary ExScript where
  arbitrary = ExScript <$> gScript

------------------------
-- old gold-ish tests --
------------------------

-- import ShortCut.Core.Types.Tests (loadExamples, readASTs)

-- exNums :: [(String, CutExpr)]
-- exNums =
--   [ ("0"        , Num 0.0)
--   , ("10e0\t\n" , Num 10.0)
--   , ("1e-4"     , Num 1.0e-4)
--   , ("6e-2"     , Num 6.0e-2)
--   , ("0.4e8 "   , Num 4.0e7)
--   , ("9e-7"     , Num 9.0e-7)
--   , ("1100000"  , Num 1100000.0)
--   , ("30e10"    , Num 3.0e11)
--   , ("1100"     , Num 1100.0)
--   , ("7e-5"     , Num 7.0e-5)
--   , ("4.000e-17", Num 4.0e-17)
--   ]

-- exFuns :: [(String, CutExpr)]
-- exFuns =
--   [ ("load_aa_seqs \"tair-plastidcut2.faa\"",
--       CutFun "gens" "load_aa_seqs" [CutLit "str" "tair-plastidcut2.faa"]),
-- 
--     ("load_genomes \"known-good-genomes.txt\"",
--       CutFun "goms" "load_genomes" [CutLit "str" "known-good-genomes.txt"]),
-- 
--     ("filter_genomes knowngenes othercyanos 20",
--       CutFun "goms" "filter_genomes" [CutRef "gens" "knowngenes",
--                                       CutRef "goms" "othercyanos",
--                                       CutLit "num" 20])
--   ]

-- exVars :: [(String, CutVar)]
-- exVars =
--   [ ("plastidcut"    , CutVar "plastidcut")
--   , ("knowngenes"    , CutVar "knowngenes")
--   , ("knowngenomes"  , CutVar "knowngenomes")
--   , ("ucyna"         , CutVar "ucyna")
--   , ("othercyanos"   , CutVar "othercyanos")
--   , ("goodcyanos"    , CutVar "goodcyanos")
--   , ("ingoodcyanos"  , CutVar "ingoodcyanos")
--   , ("inknowngenomes", CutVar "inknowngenomes")
--   
--   , ("mycutoff"      , CutVar "mycutoff")
--   , ("psIIcut"       , CutVar "psIIcut")
--   ]

-- TODO this shouldn't work without variables to look up now, right?
-- exTerms :: [(String, CutExpr)]
-- exTerms = exFuns ++
--   [ ("psIIcut", Ref (CutVar "psIIcut"))
--   ]

-- expression examples with parens added;
-- should still parse the same
-- parensExamples :: [(String, CutExpr)]
-- parensExamples = map (\(a,b) -> ("(" ++ a ++ ")",b)) exExprs

-- TODO add more types
-- exExprs :: [(String, CutExpr)]
-- exExprs = exTerms ++
--   [ ("(ingoodcyanos | inknowngenomes) ~ inucyna",
--       Bop '~' (Bop '|' (Ref (CutVar "ingoodcyanos"))
--                        (Ref (CutVar "inknowngenomes")))
--               (Ref (CutVar "inucyna")))
--   ]

-- exExprs, each assigned to a variable from above, should parse to their
-- same expressions paired with those variable names. Messy, but works!
-- exStatements :: [(String, ParsedAssign)]
-- exStatements = zip statements parsedBoth
--   where
--     vars        = map fst exVars
--     addEq a b   = a ++ " = " ++ b
--     statements  = zipWith addEq vars (map fst exExprs)
--     parsedExprs = map snd exExprs
--     parsedBoth  = zip (map CutVar vars) parsedExprs

-- TODO rewrite to work with golden tests
-- test :: (Eq a, Show a) => ParseM a -> [(String, a)] -> Either (String, String) ()
-- test _ [] = Right ()
-- test p ((a,b):xs) = case regularParse p a of
--   Left  l -> Left (a, show l)
--   Right r -> if r == b
--     then test p xs
--     else Left (a, show r)

-- pExCuts :: IO (Either CutError [ParsedScript])
-- pExCuts = do
--   cuts <- loadExamples ".cut"
--   return $ mapM (regularParse pScript) cuts

-- Same idea, but simpler this time: join the statements into one string,
-- parse it with script, and you should get back all the same statements.
-- Note that these aren't the same as the "example scripts" in examples/
-- (but they do share some of the same statements)
-- exScripts :: [(String, ParsedScript)]
-- exScripts = [(raw, ast)]
--   where
--     raw = unlines $ map fst exStatements
--     ast = map snd exStatements

-----------------------
-- utility functions --
-----------------------

regularParse :: ParseM a -> String -> Either ParseError a
regularParse p = parseWithEof p []

takeVar :: String -> CutVar
takeVar = CutVar . takeWhile (flip elem $ vNonFirstChars)

parsedItAll :: ParseM a -> String -> Bool
parsedItAll p str' = case parseWithLeftOver p [] str' of
  Right (_, "") -> True
  _ -> False

----------
-- main --
----------

mkTests :: CutConfig -> IO TestTree
mkTests _ = return $ testGroup "Parse" [wsProps, acProps]

wsProps :: TestTree
wsProps = testGroup "consume arbitrary whitespace"
  [ testProperty "after variables" $
    \(ExVar v@(CutVar s)) (ExSpace w) ->
      parseWithLeftOver pVar [] (s ++ w) == Right (v, "")
  , testProperty "after symbols" $
    \(ExSymbol c) (ExSpace w) ->
      parseWithLeftOver (pSym c) [] (c:w) == Right ((), "")
  , testProperty "after equals signs in assignment statements" $
    \(ExAssign a) (ExSpace w) ->
      parseWithLeftOver pVarEq [] (a ++ w) == Right (takeVar a, "")
  , testProperty "after quoted strings" $
    \(ExQuoted q) (ExSpace w) ->
      parseWithLeftOver pQuoted [] (q ++ w) == Right (read q, "")
  , testProperty "after numbers" $
    \(ExNum n) (ExSpace w) -> parsedItAll pNum (n ++ w)
  , testProperty "before the first identifier on a new line" $
    \(ExComment c) (ExVar (CutVar s)) ->
      parseWithLeftOver pComment [] (c ++ "\n" ++ s) == Right ((), s)
  ]

acProps :: TestTree
acProps = testGroup "parse arbitrary cut code"
  [ testProperty "variable names" $
      \(ExVar v@(CutVar s)) -> parseWithLeftOver pVar [] s == Right (v, "")
  , testProperty "symbols (reserved characters)" $
      \(ExSymbol c) -> parseWithLeftOver (pSym c) [] [c] == Right ((), "")
  , testProperty "variables with equal signs after" $
      \(ExAssign a) ->
        parseWithLeftOver pVarEq [] a == Right (takeVar a, "")
  , testProperty "quoted strings" $
      \(ExQuoted q) -> regularParse pQuoted q == Right (read q)
  , testProperty "comments" $
      \(ExComment c) -> parseWithLeftOver pComment [] c == Right ((), "")
  , testProperty "positive numbers" $
      \(ExNum n) -> isRight $ regularParse pNum n
  ]

-- spec :: Spec
-- spec = do

  -- describe "[p]arses Strings to CutExprs" $ do

    -- describe "pVar" $ do
      -- TODO put back as a golden test
      -- it "parses some valid variable names" $
      --   test pVar exVars `shouldBe` Right ()

    -- describe "pNum" $ do
      -- TODO put back as a golden test
      -- it "parses some example numbers correctly" $
      --   test pNum exNums == Right ()

    -- TODO make sure the - op never catches "-" inside scientific notation
    -- TODO make sure the - op never catches "-" inside quoted strings
    -- describe "pExpr" $ do
      -- TODO put back as a golden test
      -- it "parses some valid expressions to their correct ASTs" $
      --   test pExpr exExprs `shouldBe` Right ()
      -- TODO fix this! is the Arbitrary wrong or the test?
      -- prop "parses generated expressions (ASTs not checked)" $
        -- \(ExExpr e) -> parsedItAll pExpr e

    -- TODO put back as a golden test
    -- describe "pAssign" $ do
      -- it "parses statements generated from the vars + expressions" $
        -- test pAssign exStatements `shouldBe` Right ()

    -- TODO put back as golden tests
    -- describe "pScript" $ do
      -- it "parses the concatenated example statements to the correct AST" $
      --   test pScript exScripts `shouldBe` Right ()
      -- it "parses the example scripts to their correct ASTs" $ do
      --   parsed  <- pExCuts
      --   written <- readASTs
      --   parsed `shouldBe` (Right written)

    -- describe "pFun" $ do
      -- TODO put back as a golden test
      -- it "parses some example functions to their correct ASTs" $
      --   test pFun exFuns `shouldBe` Right ()
      -- TODO fix this! is the Arbitrary wrong or the test?
      -- prop "parses generated functions (ASTs not checked)" $
        -- \(ExFun f) -> parsedItAll pFun f

    -- describe "pTerm" $ do
      -- it "parses some example terms to their correct ASTs" $
        -- test pTerm exTerms `shouldBe` Right ()
      -- TODO fix this!
      -- prop "parses generated terms (ASTs not checked)" $
        -- \(ExTerm t) -> isRight $ regularParse pTerm t

    -- describe "pParens" $ do
      -- it "parses example expressions to their correct ASTs" $
        -- test pParens parensExamples == Right ()
      -- TODO fix this!
      -- prop "parses generated exprs inside parens (ASTS not checked)" $
        -- \(ExExpr e) -> parsedItAll pParens ("("++ e ++")")
        -- TODO fix this!
      -- prop "matches the parse results from expr" $
        -- \(ExExpr e) ->
          -- regularParse pParens ("(" ++ e ++ ")")
          -- ==
          -- regularParse pExpr e
