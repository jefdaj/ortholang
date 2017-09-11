module ShortCut.Test.Parse.Examples where

import ShortCut.Core.Types
-- import ShortCut.Core.Parse

-- TODO example function calls
-- TODO example bop expressions
-- TODO everything with and without parens
-- TODO everything with comments and newlines inturrupting
-- TODO test operator precedence

exFuns :: [(String, CutExpr)]
exFuns =
  [ ("length {}", CutFun num 0 [] "length" [CutSet EmptySet 0 [] []])
  -- , ()
  ]

-- TODO can this be done with the weird lambda thing? is it worth it?
-- exBops :: [(String, CutExpr)]
-- exBops = undefined

exTerms :: [(String, CutExpr)]
exTerms = []

exExprs :: [(String, CutExpr)]
exExprs = []

exStatements :: [(String, CutAssign)]
exStatements = []

--------------------------
-- old golden-ish tests --
--------------------------

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

----------
-- main --
----------

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
