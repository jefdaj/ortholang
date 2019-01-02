module Detourrr.Test.Parse.Arbitrary where

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
import Detourrr.Core.Types
import Detourrr.Core.Parse   (spaceChars, escapeChars, literalChars)
import Test.QuickCheck

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

newtype ExVar = ExVar DtrVar deriving (Eq, Show)

instance Arbitrary ExVar where
  arbitrary = (ExVar . DtrVar) <$> gVar

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

-- TODO need to use forAll with another function with DtrConfig arg rather than
-- an Arbitrary instance here? for now, just faking it...
gFunName :: Gen String
gFunName = elements fnNames

-- TODO is it possible to have an Arbitrary instance depend on a cfg?
--      if not, the best we can do is list them all manually
fnNames :: [String]
fnNames =
  [ "all", "any", "best_hits", "best_hits_each", "blastdbget", "blastdblist",
  "blastn", "blastn_db", "blastn_db_each", "blastn_each", "blastn_rbh",
  "blastn_rbh_each", "blastn_rev", "blastn_rev_each", "blastp", "blastp_db",
  "blastp_db_each", "blastp_each", "blastp_rbh", "blastp_rbh_each", "blastp_rev",
  "blastp_rev_each", "blastx", "blastx_db", "blastx_db_each", "blastx_each",
  "concat_bht", "concat_bht_each", "concat_faa", "concat_faa_each", "concat_fna",
  "concat_fna_each", "crb_blast", "crb_blast_each", "diff", "extract_ids",
  "extract_ids_each", "extract_queries", "extract_queries_each",
  "extract_scored", "extract_scores", "extract_seqs", "extract_seqs_each",
  "extract_targets", "extract_targets_each", "filter_evalue",
  "filter_evalue_each", "gbk_to_faa", "gbk_to_faa_each", "gbk_to_fna",
  "gbk_to_fna_each", "get_genomes", "get_proteomes", "glob_files", "histogram",
  "leave_each_out", "length", "length_each", "linegraph", "load_faa",
  "load_faa_each", "load_fna", "load_fna_each", "load_gbk", "load_gbk_each",
  "load_list", "load_nucl_db", "load_nucl_db_each", "load_prot_db",
  "load_prot_db_each", "makeblastdb_nucl_all",
  "makeblastdb_prot_all", "megablast", "megablast_db",
  "megablast_db_each", "megablast_each", "megablast_rbh", "megablast_rbh_each",
  "megablast_rev", "megablast_rev_each", "parse_searches", "psiblast",
  "psiblast_all", "psiblast_db", "psiblast_db_each", "psiblast_each",
  "psiblast_each_pssm_db", "psiblast_pssm", "psiblast_pssm_all",
  "psiblast_pssm_db", "psiblast_pssm_db_each", "psiblast_pssm_each",
  "psiblast_pssms", "psiblast_pssms_db", "psiblast_train", "psiblast_train_all",
  "psiblast_train_db", "psiblast_train_db_each", "psiblast_train_each",
  "reciprocal_best", "reciprocal_best_each", "repeat", "repeat_each",
  "scatterplot", "score_repeats", "some", "split_faa", "split_faa_each",
  "split_fna", "split_fna_each", "tblastn", "tblastn_db", "tblastn_db_each",
  "tblastn_each", "tblastx", "tblastx_db", "tblastx_db_each", "tblastx_each",
  "tblastx_rbh", "tblastx_rbh_each", "tblastx_rev", "tblastx_rev_each",
  "translate", "translate_each",

  -- and these ones which have been giving so much trouble:
  "load_faa_glob", "load_fna_glob", "load_gbk_glob"
  ]

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
