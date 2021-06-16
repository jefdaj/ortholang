module OrthoLang.Test.Parse.Arbitrary where

-- The generators can also be tested manually in ghci like so:
--
-- > generate gQuoted
-- "\"\\^bx\\ng_!vd\""
-- > generate arbitrary :: IO ExQuoted
-- ExQuoted "\"PkGN>.@T\""
-- > sample (arbitrary :: Gen ExQuoted)
-- ExQuoted "\"\""
-- ExQuoted "\"kc\""
-- ExQuoted "\"4\""
-- ExQuoted "\"[S\""
-- ExQuoted "\")M=;pe\""
-- ExQuoted "\"i\\\"5,gvrU\\\"\\\"\""
-- ExQuoted "\".s8Kq/#Z`J\""
-- ExQuoted "\"{L2[{2}.NEO\""
-- ExQuoted "\"\\\"\""
-- ExQuoted "\"j\""
-- ExQuoted "\"s\\\\G8g4Dm\""

-- Function naming conventions:
--
-- handwritten [ex]amples (strings and their correct parses)
-- [Ex]ample QuickCheck types made with string [g]enerators
-- [v]alid and i[n]valid chars (not done yet)

-- TODO email test function to jakewheatmail@gmail.com?

import Data.Scientific
import OrthoLang.Types
import OrthoLang.Interpreter
import Test.QuickCheck

import OrthoLang.Modules (modules)

-------------------------
-- Arbitrary instances --
-------------------------

-- variable names --

-- TODO any reason you would need to start with _?
-- TODO should dashes be valid in names too? probably
-- TODO starting with a capital should be ok too right?
vFirstChars :: [Char]
vFirstChars = ['a'..'z']

vNonFirstChars :: [Char]
vNonFirstChars = vFirstChars ++ '_':'-':['0'..'9']

-- TODO make sure the var isn't accidentally a fn name? (unlikely)
gVar :: Gen String
gVar = (:) <$> first <*> listOf rest
  where
    first = elements vFirstChars
    rest  = elements vNonFirstChars

newtype ExVar = ExVar Var deriving (Eq, Show)

instance Arbitrary ExVar where
  arbitrary = ExVar . Var (RepID Nothing) <$> gVar

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
gLiteral = (: []) <$> elements literalChars

-- the repeat 15 thing is just to make sure there are more literal chars
-- than escaped ones, because otherwise the strings are very hard to read
-- TODO should just "" be allowed here?
gQuoted :: Gen String
gQuoted = do
  ss <- (listOf . oneof) $ gEscaped:replicate 15 gLiteral
  return $ "\"" ++ concat ss ++ "\""

newtype ExQuoted = ExQuoted String deriving (Eq, Show)
newtype ExFile   = ExFile   String deriving (Eq, Show)

instance Arbitrary ExQuoted where
  arbitrary = ExQuoted <$> gQuoted

instance Arbitrary ExFile where
  arbitrary = ExFile <$> gQuoted

-- comments --

gComment :: Gen String
gComment = (\ss -> '#':concat ss) <$> listOf gLiteral

newtype ExComment = ExComment String deriving (Eq, Show)

instance Arbitrary ExComment where
  arbitrary = ExComment <$> gComment

-- numbers --

gSci :: Gen Scientific
gSci = scientific <$> arbitrary <*> arbitrary

-- TODO negative numbers too?
gNum :: Gen String
gNum = show . abs <$> gSci

newtype ExNum = ExNum String deriving (Eq, Show)

instance Arbitrary ExNum where
  arbitrary = ExNum <$> gNum

-- commands --

-- TODO need to use forAll with another function with Config arg rather than
-- an Arbitrary instance here? for now, just faking it...
gFunName :: Gen String
gFunName = elements fnNames

-- this is duplicated from Types.hs without the Config argument
-- TODO should this version replace it?
fnNames :: [String]
fnNames = map fName $ concatMap mFunctions modules

-- TODO why the one argument? can't do typechecking here anyway
gFun :: Gen String
gFun = do
  n <- gFunName
  w <- gWhite
  return $ n ++ w
-- gFun = do
--   n  <- gFunName
--   as <- listOf1 gTerm
--   ws <- infiniteListOf gWhite
--   let args = concat $ zipWith (++) ws as
--   return $ n ++ (head ws) ++ args

newtype ExFun = ExFun String deriving (Eq, Show)

instance Arbitrary ExFun where
  arbitrary = ExFun <$> gFun

-- binary operators --

-- TODO should messy whitespace be added here, or somewhere else?
-- TODO add another one of these for the set operators
gBop :: Gen String
gBop = do
  op <- elements ["+", "-", "*", "/", "&", "~", "|"]
  s1 <- gExpr
  s2 <- gExpr
  return $ concatMap (\ x -> ((++)) x " ") [s1,op,s2]

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
