-- Unlike the other files in test/, this one isn't meant to be run directly
-- with Hspec. It defines how to generate random values of each type, and those
-- are used to define QuickCheck properties in the *Spec.hs files.
--
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

module ShortCut.ParseExamples where

import Data.Scientific (scientific, Scientific())
import ShortCut.Types
import ShortCut.Parse
import Test.QuickCheck

--------------------
-- variable names --
--------------------

exVarNames :: [(String, ParsedVar)]
exVarNames =
  [ ("plastidcut"    , VarName "plastidcut")
  , ("knowngenes"    , VarName "knowngenes")
  , ("knowngenomes"  , VarName "knowngenomes")
  , ("ucyna"         , VarName "ucyna")
  , ("othercyanos"   , VarName "othercyanos")
  , ("goodcyanos"    , VarName "goodcyanos")
  , ("ingoodcyanos"  , VarName "ingoodcyanos")
  , ("inknowngenomes", VarName "inknowngenomes")
  , ("inucyna"       , VarName "inucyna")
  , ("mycutoff"      , VarName "mycutoff")
  , ("psIIcut"       , VarName "psIIcut")
  ]

vFirstChars :: [Char]
vFirstChars = '_':['a'..'z']

vNonFirstChars :: [Char]
vNonFirstChars = vFirstChars ++ ['0'..'9']

gVarName :: Gen String
gVarName = (:) <$> first <*> listOf rest
  where
    first = elements vFirstChars
    rest  = elements vNonFirstChars

newtype ExVarName = ExVarName VarName deriving (Eq, Show)

instance Arbitrary ExVarName where
  arbitrary = (ExVarName . VarName) <$> gVarName

----------------
-- references --
----------------

newtype ExRef = ExRef String deriving (Eq, Show)

instance Arbitrary ExRef where
  arbitrary = ExRef <$> gVarName

----------------
-- whitespace --
----------------

-- resize keeps this from growing to fill up most of the examples
gWhite :: Gen String
gWhite = resize 2 $ listOf1 $ elements whitespaceChars

newtype ExSpace = ExSpace String deriving (Eq, Show)

instance Arbitrary ExSpace where
  arbitrary = ExSpace <$> gWhite

-------------
-- symbols --
-------------

gSym :: Gen Char
gSym = elements "()=\"#"

newtype ExSymbol = ExSymbol Char deriving (Eq, Show)

instance Arbitrary ExSymbol where
  arbitrary = ExSymbol <$> gSym

-----------------
-- assignments --
-----------------

gAssign :: Gen String
gAssign = (++ " =") <$> gVarName

newtype ExAssign = ExAssign String deriving (Eq, Show)

instance Arbitrary ExAssign where
  arbitrary = ExAssign <$> gAssign

----------------------------
-- quoted string literals --
----------------------------

-- TODO: why both Fil and Quoted?

gEscaped :: Gen String
gEscaped = (\c -> '\\':[c]) <$> elements escapeChars

gLiteral :: Gen String
gLiteral = (\c -> [c]) <$> elements literalChars

-- the repeat 15 thing is just to make sure there are more literal chars
-- than escaped ones, because otherwise the strings are very hard to read
-- TODO: should just "" be allowed here?
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

--------------
-- comments --
--------------

gComment :: Gen String
gComment = (\ss -> '#':(concat ss)) <$> listOf gLiteral

newtype ExComment = ExComment String deriving (Eq, Show)

instance Arbitrary ExComment where
  arbitrary = ExComment <$> gComment

-------------
-- numbers --
-------------

exNums :: [(String, ParsedExpr)]
exNums =
  [ ("0"        , Num 0.0)
  , ("10e0\t\n" , Num 10.0)
  , ("1e-4"     , Num 1.0e-4)
  , ("6e-2"     , Num 6.0e-2)
  , ("0.4e8 "   , Num 4.0e7)
  , ("9e-7"     , Num 9.0e-7)
  , ("1100000"  , Num 1100000.0)
  , ("30e10"    , Num 3.0e11)
  , ("1100"     , Num 1100.0)
  , ("7e-5"     , Num 7.0e-5)
  , ("4.000e-17", Num 4.0e-17)
  ]

gSci :: Gen Scientific
gSci = scientific <$> arbitrary <*> arbitrary

-- TODO: negative numbers too?
gNum :: Gen String
gNum = (show . abs) <$> gSci

newtype ExNum = ExNum String deriving (Eq, Show)

instance Arbitrary ExNum where
  arbitrary = ExNum <$> gNum

--------------
-- commands --
--------------

exCmds :: [(String, ParsedExpr)]
exCmds =
  [ ("load_aa_seqs \"tair-plastidcut2.faa\"",
      Cmd "load_aa_seqs" [Fil "tair-plastidcut2.faa"]),

    ("load_genomes \"known-good-genomes.txt\"",
      Cmd "load_genomes" [Fil "known-good-genomes.txt"]),

    ("filter_genomes knowngenes othercyanos 20",
      Cmd "filter_genomes" [Ref (VarName "knowngenes"),
                            Ref (VarName "othercyanos"),
                            Num 20])
  ]

gCmdName :: Gen String
gCmdName = elements fnNames

gCmd :: Gen String
gCmd = do
  n  <- gCmdName
  as <- listOf1 gTerm
  ws <- infiniteListOf gWhite
  let args = concat $ zipWith (++) ws as
  return $ n ++ (head ws) ++ args

newtype ExCmd = ExCmd String deriving (Eq, Show)

instance Arbitrary ExCmd where
  arbitrary = ExCmd <$> gCmd

----------------------
-- binary operators --
----------------------

-- TODO: should messy whitespace be added here, or somewhere else?
gBop :: Gen String
gBop = do
  op <- elements ["+", "-", "*"]
  s1 <- gExpr
  s2 <- gExpr
  return $ concat $ zipWith (++) [s1,op,s2] (repeat " ")

newtype ExBop = ExBop String deriving (Eq, Show)

instance Arbitrary ExBop where
  arbitrary = ExBop <$> gBop

-----------------
-- expressions --
-----------------

-- TODO: finish these! there are lots of issues so far...

-- TODO: add parens, fn
-- TODO: will need to add sized in order to prevent infinite recursion on fn
-- TODO: are plain quoted strings allowed, or do they need to be fn args?
gTerm :: Gen String
gTerm = oneof [gNum, gQuoted, gVarName]

gExpr :: Gen String
gExpr = oneof [gBop, gTerm, gCmd]

newtype ExTerm = ExTerm String deriving (Eq, Show)
newtype ExExpr = ExExpr String deriving (Eq, Show)

instance Arbitrary ExTerm where
  arbitrary = ExTerm <$> gTerm

instance Arbitrary ExExpr where
  arbitrary = ExExpr <$> gExpr

exTerms :: [(String, ParsedExpr)]
exTerms = exCmds ++
  [ ("psIIcut", Ref (VarName "psIIcut"))
  ]

-- TODO: add more types
exExprs :: [(String, ParsedExpr)]
exExprs = exTerms ++
  [ ("(ingoodcyanos + inknowngenomes) - inucyna",
      Bop '-' (Bop '+' (Ref (VarName "ingoodcyanos"))
                       (Ref (VarName "inknowngenomes")))
              (Ref (VarName "inucyna")))
  ]

-- expression examples with parens added;
-- should still parse the same
parensExamples :: [(String, ParsedExpr)]
parensExamples = map (\(a,b) -> ("(" ++ a ++ ")",b)) exExprs

----------------
-- statements --
----------------

-- exExprs, each assigned to a variable from above, should parse to their
-- same expressions paired with those variable names. Messy, but works!
exStatements :: [(String, ParsedAssign)]
exStatements = zip statements parsedBoth
  where
    vars        = map fst exVarNames
    addEq a b   = a ++ " = " ++ b
    statements  = zipWith addEq vars (map fst exExprs)
    parsedExprs = map snd exExprs
    parsedBoth  = zip (map VarName vars) parsedExprs

gStatement :: Gen String
gStatement = undefined

newtype ExStatement = ExStatement String deriving (Eq, Show)

instance Arbitrary ExStatement where
  arbitrary = ExStatement <$> gStatement

-------------
-- scripts --
-------------

-- Same idea, but simpler this time: join the statements into one string,
-- parse it with script, and you should get back all the same statements.
-- Note that these aren't the same as the "example scripts" in examples/
-- (but they do share some of the same statements)
exScripts :: [(String, ParsedScript)]
exScripts = [(raw, ast)]
  where
    raw = unlines $ map fst exStatements
    ast = map snd exStatements

gScript :: Gen String
gScript = undefined

newtype ExScript = ExScript String deriving (Eq, Show)

instance Arbitrary ExScript where
  arbitrary = ExScript <$> gScript
